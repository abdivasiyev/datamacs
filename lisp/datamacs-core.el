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

(defcustom datamacs-log-queries t
  "Non-nil means log every query sent to a server in *Messages*."
  :type 'boolean
  :group 'datamacs)

(defcustom datamacs-connect-timeout 10
  "Seconds to wait for a database server to accept a connection."
  :type 'natnum
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
Each table is (NAME TYPE COLUMNS [SECTIONS]) and each column is
\(NAME TYPE KEY), where KEY is non-nil for primary key columns.
SECTIONS is an alist of (TITLE . LINES), such as indexes, and each
line is (LABEL TEXT TARGET), where TARGET is a (SCHEMA . NAME) that
RET shows, or nil.")

(defun datamacs-table-add-line (table title line)
  "Add LINE to the TITLE section of TABLE, a schema table entry."
  (let ((section (assoc title (nth 3 table))))
    (cond (section (setcdr section (append (cdr section) (list line))))
          ((nthcdr 3 table)
           (setcar (nthcdr 3 table) (append (nth 3 table) (list (list title line)))))
          (t (nconc table (list (list (list title line))))))))

(cl-defgeneric datamacs-driver-multi-database-p (driver)
  "Return non-nil if DRIVER servers hold several databases.")

(cl-defmethod datamacs-driver-multi-database-p (_driver)
  "Assume one database per connection."
  nil)

(cl-defgeneric datamacs-driver-databases (conn)
  "Return the names of the databases on the server of CONN.")

(cl-defgeneric datamacs-driver-in-transaction-p (conn)
  "Return non-nil if CONN has an open transaction.")

(cl-defmethod datamacs-driver-in-transaction-p (_conn)
  "Assume there is no open transaction."
  nil)

(cl-defgeneric datamacs-driver-update-cell (conn table key column old new)
  "Change COLUMN from OLD to NEW in the TABLE row matching KEY on CONN.
KEY is an alist of primary key columns and values.  Return the
number of changed rows.")

(defvar datamacs--progress-label nil
  "What datamacs is waiting for, shown by the progress timer.")

(defvar datamacs-progress-seconds 0
  "Seconds the last `datamacs-call-with-progress' took.")

(defun datamacs-call-with-progress (label fn)
  "Call FN while showing LABEL and the seconds waited in the echo area.
Return FN's value.  A nested call only changes the label shown."
  (if datamacs--progress-label
      (let ((datamacs--progress-label label))
        (message "%s..." label)
        (funcall fn))
    (let* ((datamacs--progress-label label)
           (start (float-time))
           (timer (run-with-timer
                   1 1 (lambda ()
                         (message "%s... %ds" datamacs--progress-label
                                  (round (- (float-time) start)))))))
      (message "%s..." label)
      (unwind-protect
          (prog1 (funcall fn)
            (setq datamacs-progress-seconds (- (float-time) start))
            (message "%s...done (%.1fs)" label datamacs-progress-seconds))
        (cancel-timer timer)))))

(defun datamacs-log-query (conn query &optional values)
  "Log QUERY with its parameter VALUES, sent on CONN, to *Messages*.
It is not shown in the echo area."
  (when datamacs-log-queries
    (let ((inhibit-message t))
      (message "datamacs: %s%s: %s%s"
               (datamacs-describe (datamacs-conn-spec conn))
               (if (datamacs-conn-readonly conn) " [read-only]" "")
               (string-trim query)
               (if values (format "  -- %S" values) "")))))

(defun datamacs-describe (spec)
  "Return the connection name of SPEC, with its database if any."
  (if-let* ((db (plist-get spec :database)))
      (format "%s (%s)" (plist-get spec :name) db)
    (plist-get spec :name)))

(defun datamacs-check-reachable (name host port)
  "Signal an error unless HOST accepts connections on PORT.
NAME is the connection name shown while waiting.  Give up after
`datamacs-connect-timeout' seconds; \\[keyboard-quit] stops earlier."
  (datamacs-call-with-progress
   (format "Connecting to %s (%s:%s)" name host port)
   (lambda () (datamacs--wait-reachable host port))))

(defun datamacs--wait-reachable (host port)
  "Wait until HOST accepts connections on PORT, or signal an error."
  (let ((proc (condition-case err
                   (make-network-process :name "datamacs-check" :host host :service port
                                         :nowait t :noquery t)
                 (error (user-error "Cannot connect to %s:%s: %s"
                                    host port (error-message-string err)))))
        (deadline (+ (float-time) datamacs-connect-timeout)))
    (unwind-protect
        (progn
          (while (and (eq (process-status proc) 'connect)
                      (< (float-time) deadline))
            (accept-process-output nil 0.1))
          (pcase (process-status proc)
            ('open t)
            ('connect (user-error "Cannot connect to %s:%s: no answer after %d seconds"
                                  host port datamacs-connect-timeout))
            (_ (user-error "Cannot connect to %s:%s: connection refused or unreachable"
                           host port))))
      (delete-process proc))))

(defun datamacs-call-with-password (spec fn)
  "Call FN with a password function for the SPEC connection.
The function returns SPEC's :password or the password stored in
auth-source, or nil.  Called with a non-nil argument after the
server rejected that, it asks the user.  A password typed for a
connection with nothing stored is offered for saving once FN
succeeds, and forgotten if FN fails."
  (let ((query (seq-mapcat
                (lambda (key)
                  (when-let* ((value (plist-get spec key)))
                    (list key (format "%s" value))))
                '(:host :port :user)))
        found)
    (condition-case err
        (prog1 (funcall fn (lambda (&optional ask)
                             (let ((stored (car (apply #'auth-source-search
                                                       :max 1 :require '(:secret) query))))
                               (cond ((not ask)
                                      (or (plist-get spec :password)
                                          (datamacs--secret stored)))
                                     ((or stored (plist-get spec :password))
                                      (read-passwd
                                       (format "Password for %s@%s (the saved one was rejected): "
                                               (plist-get spec :user) (plist-get spec :host))))
                                     (t
                                      (setq found (car (apply #'auth-source-search
                                                              :max 1 :require '(:secret)
                                                              :create t query)))
                                      (datamacs--secret found))))))
          (when-let* ((save (plist-get found :save-function)))
            (funcall save)
            ;; auth-source remembers that the first search found nothing.
            (apply #'auth-source-forget+ query)))
      (error
       (when found
         (apply #'auth-source-forget+ query))
       (signal (car err) (cdr err))))))

(defun datamacs--secret (entry)
  "Return the password of the auth-source ENTRY, or nil."
  (let ((secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

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

(defun datamacs-check-readonly (conn query)
  "Signal an error if CONN is read-only and QUERY does not only read.
Reading queries start with SELECT, WITH, VALUES, TABLE, SHOW, or
EXPLAIN without ANALYZE."
  (when (and (datamacs-conn-readonly conn)
             (not (with-temp-buffer
                    (set-syntax-table sql-mode-syntax-table)
                    (insert query)
                    (goto-char (point-min))
                    (forward-comment (buffer-size))
                    (skip-chars-forward "( \t\n\r")
                    (let ((case-fold-search t)
                          (word (upcase (buffer-substring
                                         (point)
                                         (progn (skip-chars-forward "[:alpha:]") (point))))))
                      (or (member word '("SELECT" "WITH" "VALUES" "TABLE" "SHOW"))
                          (and (equal word "EXPLAIN")
                               (not (string-match-p "\\banaly[sz]e\\b" query))))))))
    (user-error "Read-only connection: only SELECT, WITH, VALUES, TABLE, SHOW and EXPLAIN can run")))

(defun datamacs-sql-statement-at (text pos)
  "Return the SQL statement of TEXT at POS, counted from 0.
Statements are separated by `;' outside strings, comments and
PostgreSQL $$ quotes.  If POS is in an empty statement, return the
one before it."
  (with-temp-buffer
    (set-syntax-table (make-syntax-table sql-mode-syntax-table))
    (modify-syntax-entry ?\" "\"")
    (setq-local parse-sexp-lookup-properties t)
    (insert text)
    (goto-char (point-min))
    (let ((start 0) prev)
      (catch 'found
        (while t
          (let ((end (length text)))
            (while (and (re-search-forward
                         ";\\|\\$\\(?:[[:alpha:]_][[:alnum:]_]*\\)?\\$" nil t)
                        (let ((match (match-string 0))
                              (beg (match-beginning 0)))
                          (cond ((nth 8 (save-excursion (syntax-ppss beg))) t)
                                ((equal match ";") (setq end (1- (match-end 0))) nil)
                                ((and (char-before beg)
                                      (memq (char-syntax (char-before beg)) '(?w ?_)))
                                 t)
                                (t (datamacs-sql--mark-dollar-quote beg match) t)))))
            (let ((stmt (string-trim (substring text start end) nil "[ \t\n\r;]+")))
              (when (<= pos end)
                (throw 'found (if (string-empty-p stmt) prev stmt)))
              (unless (string-empty-p stmt)
                (setq prev stmt))
              (setq start end))))))))

(defun datamacs-sql--mark-dollar-quote (beg tag)
  "Mark the $TAG$ quote starting at BEG as a string and move past it."
  (let ((close (or (search-forward tag nil t) (point-max))))
    (put-text-property beg (1+ beg) 'syntax-table (string-to-syntax "|"))
    (put-text-property (1- close) close 'syntax-table (string-to-syntax "|"))
    (syntax-ppss-flush-cache beg)
    (goto-char close)))

(provide 'datamacs-core)
;;; datamacs-core.el ends here
