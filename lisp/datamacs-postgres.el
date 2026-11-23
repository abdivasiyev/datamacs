;;; datamacs-postgres.el --- PostgreSQL driver for datamacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; PostgreSQL driver, built on the pg package from MELPA.  It is only
;; loaded when a PostgreSQL connection is opened.
;;
;;   (:name "prod" :driver postgres :host "db.example.com" :port 5432
;;    :user "app" :database "shop" :sslmode require)
;;
;; :sslmode is `disable', `prefer' (the default) or `require'.

;;; Code:

(require 'datamacs-core)

(defvar pg-disable-type-coercion)
(declare-function pg-connect-plist "pg")
(declare-function pg-disconnect "pg")
(declare-function pg-exec "pg")
(declare-function pg-result "pg")
(declare-function pgcon-transaction-status "pg")

(cl-defstruct (datamacs-postgres-conn
               (:include datamacs-conn (driver 'postgres))
               (:constructor datamacs-postgres-conn-create))
  "An open PostgreSQL connection.")

(cl-defmethod datamacs-driver-open ((_driver (eql 'postgres)) spec)
  "Open the PostgreSQL connection described by SPEC."
  (unless (require 'pg nil t)
    (user-error "PostgreSQL needs the pg package from MELPA"))
  (let ((handle
         (condition-case err
             (datamacs-call-with-password
              spec
              (lambda (password)
                ;; Ask for a password only if the server wants one.
                (condition-case err
                    (datamacs-postgres--connect spec (or (funcall password) ""))
                  (pg-invalid-password
                   (if (string-match-p "SQLSTATE=28P01" (format "%s" (cadr err)))
                       (datamacs-postgres--connect spec (funcall password t))
                     (signal (car err) (cdr err)))))))
           (error (datamacs-postgres--error err)))))
    (datamacs-postgres-conn-create
     :name (plist-get spec :name)
     :spec spec
     :readonly (plist-get spec :readonly)
     :handle handle
     :status 'open)))

(defun datamacs-postgres--connect (spec password)
  "Connect to the PostgreSQL server of SPEC with PASSWORD.
With :sslmode `prefer', fall back to a plain connection if TLS fails."
  (let* ((user (or (plist-get spec :user) (user-login-name)))
         (port (plist-get spec :port))
         (sslmode (format "%s" (or (plist-get spec :sslmode) "prefer")))
         (connect (lambda (tls)
                    (let ((pg-disable-type-coercion t))
                      (pg-connect-plist (or (plist-get spec :database) user) user
                                        :password password
                                        :host (or (plist-get spec :host) "localhost")
                                        :port (if (stringp port)
                                                  (string-to-number port)
                                                (or port 5432))
                                        :tls-options tls)))))
    (pcase sslmode
      ("disable" (funcall connect nil))
      ("require" (funcall connect t))
      ("prefer" (condition-case err
                    (funcall connect t)
                  (pg-invalid-password (signal (car err) (cdr err)))
                  (error (funcall connect nil))))
      (_ (user-error "Unknown :sslmode %s" sslmode)))))

(cl-defmethod datamacs-driver-close ((conn datamacs-postgres-conn))
  "Close the PostgreSQL connection CONN."
  (when (datamacs-conn-handle conn)
    (ignore-errors (pg-disconnect (datamacs-conn-handle conn))))
  (setf (datamacs-conn-handle conn) nil
        (datamacs-conn-status conn) 'closed))

(cl-defmethod datamacs-driver-execute ((conn datamacs-postgres-conn) query)
  "Run QUERY on the PostgreSQL connection CONN."
  (let* ((query (or (datamacs-sql-statement-at query 0) query))
         (result (progn (datamacs-check-readonly conn query)
                        (datamacs-postgres--exec conn query)))
         (columns (mapcar #'car (pg-result result :attributes))))
    (if columns
        (datamacs-table-result-create
         :columns columns :rows (pg-result result :tuples))
      (let ((status (pg-result result :status)))
        (datamacs-affected-result-create
         :count (if (string-match "\\([0-9]+\\)\\'" status)
                    (string-to-number (match-string 1 status))
                  0))))))

(defun datamacs-postgres--exec (conn query)
  "Run QUERY on CONN and return the pg result.
On a read-only connection QUERY runs in a READ ONLY transaction that
is rolled back.  Values are kept as the text PostgreSQL sends."
  (let ((handle (datamacs-conn-handle conn))
        (pg-disable-type-coercion t))
    (unless handle
      (user-error "Connection %s is closed" (datamacs-conn-name conn)))
    (condition-case err
        (if (datamacs-conn-readonly conn)
            (progn
              (pg-exec handle "BEGIN READ ONLY")
              (unwind-protect (pg-exec handle query)
                (pg-exec handle "ROLLBACK")))
          (pg-exec handle query))
      (error (datamacs-postgres--error err)))))

(cl-defmethod datamacs-driver-multi-database-p ((_driver (eql 'postgres)))
  "PostgreSQL servers hold several databases."
  t)

(cl-defmethod datamacs-driver-databases ((conn datamacs-postgres-conn))
  "Return the databases on the server of CONN that accept connections."
  (mapcar #'car (pg-result (datamacs-postgres--exec conn "SELECT datname
FROM pg_database WHERE datallowconn AND NOT datistemplate ORDER BY datname")
                           :tuples)))

(cl-defmethod datamacs-driver-schema ((conn datamacs-postgres-conn))
  "Return the schema of the PostgreSQL database of CONN."
  (let (schemas)
    (dolist (row (pg-result (datamacs-postgres--exec conn "SELECT n.nspname, c.relname,
       CASE c.relkind WHEN 'v' THEN 'view' WHEN 'm' THEN 'materialized view'
                      WHEN 'f' THEN 'foreign table' ELSE 'table' END,
       a.attname, format_type(a.atttypid, a.atttypmod),
       EXISTS (SELECT 1 FROM pg_index i
               WHERE i.indrelid = c.oid AND i.indisprimary
                 AND a.attnum = ANY (i.indkey))
FROM pg_class c
JOIN pg_namespace n ON n.oid = c.relnamespace
JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped
WHERE c.relkind IN ('r', 'p', 'v', 'm', 'f')
  AND n.nspname NOT IN ('pg_catalog', 'information_schema')
  AND n.nspname NOT LIKE 'pg\\_toast%' AND n.nspname NOT LIKE 'pg\\_temp%'
ORDER BY n.nspname, c.relname, a.attnum")
                            :tuples))
      (pcase-let* ((`(,schema ,table ,type ,column ,ctype ,key) row)
                   (s (or (assoc schema schemas)
                          (car (push (list schema) schemas))))
                   (tb (or (assoc table (cdr s))
                           (car (setcdr s (cons (list table type nil) (cdr s)))))))
        (setcar (nthcdr 2 tb) (cons (list column ctype (equal key "t")) (nth 2 tb)))))
    (mapcar (lambda (s)
              (cons (car s)
                    (mapcar (lambda (tb) (list (nth 0 tb) (nth 1 tb) (reverse (nth 2 tb))))
                            (reverse (cdr s)))))
            (reverse schemas))))

(cl-defmethod datamacs-driver-in-transaction-p ((conn datamacs-postgres-conn))
  "Return non-nil if the PostgreSQL connection CONN has an open transaction."
  (memq (pgcon-transaction-status (datamacs-conn-handle conn)) '(?T ?E)))

(defun datamacs-postgres--error (err)
  "Signal ERR from pg as a short `user-error'."
  (let ((case-fold-search nil)
        (text (cond ((eq (car err) 'file-error)
                     (format "Cannot connect: %s" (nth 2 err)))
                    ((stringp (cadr err)) (cadr err))
                    (t (error-message-string err)))))
    (when (string-match-p "TLS connection.*read char 78\\'" text)
      (setq text "The server does not accept TLS connections"))
    (when (string-match "\\`\\(?:ERROR\\|FATAL\\)[^:]*: " text)
      (setq text (substring text (match-end 0))))
    ;; pg appends "(routine: ..., hint: ..., detail: ...) SQLSTATE=...".
    (when (string-match " ([a-z][a-z ]*: .*\\'" text)
      (let ((details (match-string 0 text)))
        (setq text (substring text 0 (match-beginning 0)))
        (dolist (key '("detail" "hint"))
          (when (string-match (concat key ": \\(.*?\\)\\(?:, [a-z][a-z ]*: \\|) SQLSTATE\\)")
                              details)
            (setq text (concat text "\n" (capitalize key) ": "
                               (match-string 1 details)))))))
    (user-error "%s" text)))

(provide 'datamacs-postgres)
;;; datamacs-postgres.el ends here
