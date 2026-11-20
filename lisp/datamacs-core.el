;;; datamacs-core.el --- Core types for datamacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Connection types and settings shared by all datamacs drivers.

;;; Code:

(require 'cl-lib)
(require 'sql)
(require 'auth-source)

(defgroup datamacs nil
  "Database client for Emacs."
  :group 'tools
  :prefix "datamacs-")

;;; Configuration: connections the user declares

(defcustom datamacs-connections nil
  "Database connections known to datamacs.
Each element is a plist.  Every connection has its own :name and
:driver.  Other keys depend on the driver."
  :type '(repeat plist)
  :group 'datamacs)

;;; Live connections

(cl-defstruct (datamacs-conn (:constructor nil))
  "An open database connection.  Shared by all database drivers."
  (name nil :documentation "Connection name.")
  (driver nil :documentation "Driver symbol, such as `sqlite'.")
  (spec nil :documentation "Connection plist this was opened from.")
  (status 'closed :documentation "One of `open', `closed' or `error'.")
  (readonly nil :documentation "Non-nil means only read queries are allowed.")
  (handle nil :documentation "The driver's native object."))

;;; Results

(cl-defstruct (datamacs-result (:constructor nil))
  "Base type of all query results.")

(cl-defstruct (datamacs-table-result
               (:include datamacs-result)
               (:constructor datamacs-table-result-create))
  "Rows with named columns, as returned by SQL queries."
  (columns nil :documentation "Column names.")
  (rows nil :documentation "Rows, each a list of values.")
  (table nil :documentation "Table the rows come from, or nil if not editable.")
  (key nil :documentation "Primary key columns of the table."))

(cl-defstruct (datamacs-documents-result
               (:include datamacs-result)
               (:constructor datamacs-documents-result-create))
  "Documents, as returned by MongoDB."
  (documents nil :documentation "List of documents, each an alist."))

(cl-defstruct (datamacs-value-result
               (:include datamacs-result)
               (:constructor datamacs-value-result-create))
  "A single value, as returned by Redis."
  (value nil :documentation "String, number, list or nil."))

(cl-defstruct (datamacs-affected-result
               (:include datamacs-result)
               (:constructor datamacs-affected-result-create))
  "Number of changed rows or documents."
  (count 0 :documentation "Number of changed rows."))

;;; Driver interface

(cl-defgeneric datamacs-driver-open (driver spec)
  "Open a DRIVER connection from SPEC.")

(cl-defgeneric datamacs-driver-close (conn)
  "Close CONN.")

(cl-defgeneric datamacs-driver-execute (conn query)
  "Run QUERY on CONN and return a `datamacs-result'.")

(cl-defgeneric datamacs-driver-schema (conn)
  "Return the schema of CONN as a list of (SCHEMA . TABLES).
Each table is (NAME TYPE COLUMNS) and each column is (NAME TYPE KEY),
where KEY is non-nil for primary key columns.")

(cl-defgeneric datamacs-driver-in-transaction-p (conn)
  "Return non-nil if CONN has an open transaction.")

(cl-defmethod datamacs-driver-in-transaction-p (_conn)
  "Assume there is no open transaction."
  nil)

(cl-defgeneric datamacs-driver-update-cell (conn table key column old new)
  "Change COLUMN from OLD to NEW in the TABLE row matching KEY on CONN.
KEY is an alist of primary key columns and values.  Return the
number of changed rows.")

(defun datamacs-call-with-password (spec fn)
  "Call FN with the password of the SPEC connection and return its value.
The password is SPEC's :password, or comes from auth-source, which
asks for it when there is none.  A new password is offered for
saving once FN succeeds, and forgotten if FN fails."
  (if-let* ((password (plist-get spec :password)))
      (funcall fn password)
    (let* ((query (seq-mapcat
                   (lambda (key)
                     (when-let* ((value (plist-get spec key)))
                       (list key (format "%s" value))))
                   '(:host :port :user)))
           (found (car (apply #'auth-source-search
                              :max 1 :require '(:secret) :create t query)))
           (secret (plist-get found :secret)))
      (condition-case err
          (prog1 (funcall fn (if (functionp secret) (funcall secret) secret))
            (when-let* ((save (plist-get found :save-function)))
              (funcall save)))
        (error
         (apply #'auth-source-forget+ query)
         (signal (car err) (cdr err)))))))

(defvar datamacs--sessions (make-hash-table :test #'equal)
  "Open session connections, keyed by their spec.")

(defun datamacs-call-with-connection (spec fn)
  "Call FN with a connection for SPEC.
If SPEC has a :session, reuse its open connection.  Otherwise open
a new connection and close it afterwards."
  (if (plist-get spec :session)
      (let ((conn (gethash spec datamacs--sessions)))
        (unless (and conn (eq (datamacs-conn-status conn) 'open))
          (setq conn (datamacs-driver-open (plist-get spec :driver) spec))
          (puthash spec conn datamacs--sessions))
        (funcall fn conn))
    (let ((conn (datamacs-driver-open (plist-get spec :driver) spec)))
      (unwind-protect (funcall fn conn)
        (datamacs-driver-close conn)))))

(defun datamacs-readonly-spec (spec)
  "Return a copy of SPEC for a read-only connection without a session."
  (plist-put (plist-put (copy-sequence spec) :session nil) :readonly t))

(defun datamacs-disconnect ()
  "Close all session connections."
  (interactive)
  (when (datamacs--confirm-sessions)
    (let ((n (hash-table-count datamacs--sessions)))
      (maphash (lambda (_spec conn) (datamacs-driver-close conn)) datamacs--sessions)
      (clrhash datamacs--sessions)
      (message "Closed %d session%s" n (if (= n 1) "" "s")))))

(defun datamacs--confirm-sessions ()
  "Ask before rolling back open session transactions.
Return non-nil if there are none, or the user agrees."
  (let (busy)
    (maphash (lambda (spec conn)
               (when (and (eq (datamacs-conn-status conn) 'open)
                          (datamacs-driver-in-transaction-p conn))
                 (push (plist-get spec :session) busy)))
             datamacs--sessions)
    (or (null busy)
        (yes-or-no-p (format "Roll back open transactions in %s? "
                             (string-join busy ", "))))))

(add-hook 'kill-emacs-query-functions #'datamacs--confirm-sessions)

(defun datamacs-sql-statement-at (text pos)
  "Return the SQL statement of TEXT at POS, counted from 0.
Statements are separated by `;' outside strings and comments.  If
POS is in an empty statement, return the one before it."
  (with-temp-buffer
    (set-syntax-table (make-syntax-table sql-mode-syntax-table))
    (modify-syntax-entry ?\" "\"")
    (insert text)
    (goto-char (point-min))
    (let ((start 0) prev)
      (catch 'found
        (while t
          (let* ((end (if (search-forward ";" nil t) (1- (point)) (length text)))
                 (stmt (string-trim (substring text start end) nil "[ \t\n\r;]+")))
            (unless (and (< end (length text)) (nth 8 (syntax-ppss)))
              (when (<= pos end)
                (throw 'found (if (string-empty-p stmt) prev stmt)))
              (unless (string-empty-p stmt)
                (setq prev stmt))
              (setq start end))))))))

(provide 'datamacs-core)
;;; datamacs-core.el ends here
