;;; datamacs-postgres.el --- PostgreSQL driver for datamacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; SPDX-License-Identifier: GPL-3.0-or-later

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
(defvar pg-connect-timeout)
(declare-function pg-connect-plist "pg")
(declare-function pg-disconnect "pg")
(declare-function pg-exec "pg")
(declare-function pg-result "pg")
(declare-function pgcon-transaction-status "pg")
(declare-function pg-escape-identifier "pg")
(declare-function pg-escape-literal "pg")

(cl-defstruct (datamacs-postgres-conn
               (:include datamacs-conn (driver 'postgres))
               (:constructor datamacs-postgres-conn-create))
  "An open PostgreSQL connection.")

(cl-defmethod datamacs-driver-open ((_driver (eql 'postgres)) spec)
  "Open the PostgreSQL connection described by SPEC."
  (unless (require 'pg nil t)
    (user-error "PostgreSQL needs the pg package from MELPA"))
  (datamacs-check-reachable (plist-get spec :name)
                            (or (plist-get spec :host) "localhost")
                            (or (plist-get spec :port) 5432))
  (let ((handle
         (condition-case err
             (datamacs-call-with-password
              spec
              (lambda (password)
                ;; Ask for a password only if the server wants one.
                (condition-case err
                    (datamacs-postgres--connect spec (or (funcall password) ""))
                  (pg-invalid-password
                   (if (string-match-p "SQLSTATE=28P01\\|password authentication failed"
                                       (format "%s" (cadr err)))
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
                    (let ((pg-disable-type-coercion t)
                          (pg-connect-timeout datamacs-connect-timeout))
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
                  ((pg-invalid-password pg-connect-timeout file-error)
                   (signal (car err) (cdr err)))
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
                        (datamacs-log-query conn query)
                        (datamacs-postgres--exec conn query)))
         (columns (mapcar #'car (pg-result result :attributes))))
    (if columns
        (let ((source (datamacs-postgres--source conn query columns)))
          (datamacs-table-result-create
           :columns columns :rows (pg-result result :tuples)
           :table (car source) :key (cdr source)))
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

(defun datamacs-postgres--source (conn query columns)
  "Return (TABLE . KEY) if rows of QUERY on CONN can be edited.
Only a plain SELECT from one table whose COLUMNS include the
primary key qualifies."
  (when-let* ((table (datamacs-sql-single-table query))
              (key (mapcar #'car (datamacs-postgres--rows conn (format "SELECT a.attname
FROM pg_index i
JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY (i.indkey)
WHERE i.indrelid = to_regclass(%s) AND i.indisprimary
ORDER BY array_position(i.indkey::int2[], a.attnum)" (pg-escape-literal table))))))
    (when (seq-every-p (lambda (k) (member k columns)) key)
      (cons table key))))

(cl-defmethod datamacs-driver-update-cell
  ((conn datamacs-postgres-conn) table key column old new)
  "Change COLUMN from OLD to NEW in the TABLE row matching KEY on CONN.
Values are compared as the text PostgreSQL shows."
  (let* ((literal (lambda (v) (if v (pg-escape-literal (format "%s" v)) "NULL")))
         (col (pg-escape-identifier column))
         ;; format() gives the text PostgreSQL shows, unlike ::text for booleans.
         (shown (format "format('%%s', %s)" col))
         (sql (format "UPDATE %s SET %s = %s WHERE %s AND %s RETURNING %s"
                      (mapconcat (lambda (part) (pg-escape-identifier (downcase part)))
                                 (split-string table "\\.") ".")
                      col (funcall literal new)
                      (mapconcat (lambda (k)
                                   (format "%s = %s" (pg-escape-identifier (car k))
                                           (funcall literal (cdr k))))
                                 key " AND ")
                      (if old
                          (format "%s = %s" shown (funcall literal old))
                        (format "%s IS NULL" col))
                      shown))
         (rows (progn (datamacs-log-query conn sql)
                      (datamacs-postgres--rows conn sql))))
    (when rows
      (list (and new (caar rows))))))

(cl-defmethod datamacs-driver-multi-database-p ((_driver (eql 'postgres)))
  "PostgreSQL servers hold several databases."
  t)

(cl-defmethod datamacs-driver-databases ((conn datamacs-postgres-conn))
  "Return the databases on the server of CONN that accept connections."
  (mapcar #'car (pg-result (datamacs-postgres--exec conn "SELECT datname
FROM pg_database WHERE datallowconn AND NOT datistemplate ORDER BY datname")
                           :tuples)))

(cl-defmethod datamacs-driver-schema ((conn datamacs-postgres-conn))
  "Return the schema of the PostgreSQL database of CONN.
Partitions are listed under their table, not as tables."
  (let ((schemas (datamacs-postgres--tables conn))
        (tables (make-hash-table :test #'equal)))
    (dolist (s schemas)
      (dolist (table (cdr s))
        (puthash (cons (car s) (car table)) table tables)))
    (datamacs-postgres--add-partitions conn tables)
    (datamacs-postgres--add-constraints conn tables)
    (datamacs-postgres--add-indexes conn tables)
    schemas))

(defun datamacs-postgres--rows (conn query)
  "Return the rows of QUERY on CONN."
  (pg-result (datamacs-postgres--exec conn query) :tuples))

(defun datamacs-postgres--tables (conn)
  "Return the schemas of CONN with their tables and columns."
  (let (schemas)
    (dolist (row (datamacs-postgres--rows conn "SELECT n.nspname, c.relname,
       CASE c.relkind WHEN 'v' THEN 'view' WHEN 'm' THEN 'materialized view'
                      WHEN 'f' THEN 'foreign table' WHEN 'p' THEN 'partitioned table'
                      ELSE 'table' END,
       a.attname, format_type(a.atttypid, a.atttypmod),
       EXISTS (SELECT 1 FROM pg_index i
               WHERE i.indrelid = c.oid AND i.indisprimary
                 AND a.attnum = ANY (i.indkey))
FROM pg_class c
JOIN pg_namespace n ON n.oid = c.relnamespace
JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped
WHERE c.relkind IN ('r', 'p', 'v', 'm', 'f') AND NOT c.relispartition
  AND n.nspname NOT IN ('pg_catalog', 'information_schema')
  AND n.nspname NOT LIKE 'pg\\_toast%' AND n.nspname NOT LIKE 'pg\\_temp%'
ORDER BY n.nspname, c.relname, a.attnum"))
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

(defun datamacs-postgres--add-partitions (conn tables)
  "Add the partition key and partitions of the TABLES of CONN."
  (let ((children (make-hash-table :test #'equal)))
    (dolist (row (datamacs-postgres--rows conn "SELECT pn.nspname, p.relname, cn.nspname, c.relname,
       pg_get_expr(c.relpartbound, c.oid), c.relkind = 'p', pg_get_partkeydef(p.oid)
FROM pg_inherits i
JOIN pg_class c ON c.oid = i.inhrelid AND c.relispartition
JOIN pg_class p ON p.oid = i.inhparent
JOIN pg_namespace pn ON pn.oid = p.relnamespace
JOIN pg_namespace cn ON cn.oid = c.relnamespace
ORDER BY 1, 2, 4"))
      (push row (gethash (cons (nth 0 row) (nth 1 row)) children)))
    (maphash (lambda (key table)
               (when-let* ((rows (gethash key children)))
                 (datamacs-table-add-line table "Partitioned by" (list "" (nth 6 (car rows)) nil))
                 (datamacs-postgres--add-partition-lines table children key 0)))
             tables)))

(defun datamacs-postgres--add-partition-lines (table children key depth)
  "Add the partitions of KEY to TABLE, indented by DEPTH, from CHILDREN."
  (dolist (row (reverse (gethash key children)))
    (let ((child (cons (nth 2 row) (nth 3 row))))
      (datamacs-table-add-line
       table "Partitions"
       (list (concat (make-string (* 2 depth) ?\s) (nth 3 row))
             (concat (nth 4 row) (if (equal (nth 5 row) "t") " · partitioned" ""))
             child))
      (datamacs-postgres--add-partition-lines table children child (1+ depth)))))

(defun datamacs-postgres--add-constraints (conn tables)
  "Add the foreign keys and other constraints of the TABLES of CONN."
  (let ((rows (datamacs-postgres--rows conn "SELECT n.nspname, c.relname, con.conname,
       con.contype, pg_get_constraintdef(con.oid)
FROM pg_constraint con
JOIN pg_class c ON c.oid = con.conrelid
JOIN pg_namespace n ON n.oid = c.relnamespace
WHERE con.contype IN ('f', 'p', 'u', 'c', 'x')
ORDER BY 1, 2, 3")))
    (dolist (foreign '(t nil))
      (dolist (row rows)
        (when-let* ((table (gethash (cons (nth 0 row) (nth 1 row)) tables)))
          (when (eq foreign (equal (nth 3 row) "f"))
            (datamacs-table-add-line
             table (if foreign "Foreign keys" "Constraints")
             (list (nth 2 row)
                   (if foreign
                       (string-replace " REFERENCES " " → "
                                       (string-remove-prefix "FOREIGN KEY " (nth 4 row)))
                     (nth 4 row))
                   nil))))))))

(defun datamacs-postgres--add-indexes (conn tables)
  "Add the indexes of the TABLES of CONN."
  (dolist (row (datamacs-postgres--rows conn "SELECT n.nspname, t.relname, i.relname,
       x.indisunique, pg_get_indexdef(x.indexrelid)
FROM pg_index x
JOIN pg_class i ON i.oid = x.indexrelid
JOIN pg_class t ON t.oid = x.indrelid
JOIN pg_namespace n ON n.oid = t.relnamespace
ORDER BY 1, 2, 3"))
    (when-let* ((table (gethash (cons (nth 0 row) (nth 1 row)) tables)))
      (let ((def (nth 4 row)))
        (datamacs-table-add-line
         table "Indexes"
         (list (nth 2 row)
               (concat (if (equal (nth 3 row) "t") "UNIQUE " "")
                       (string-remove-prefix
                        "btree "
                        (if (string-match " USING \\(.*\\)\\'" def) (match-string 1 def) def)))
               nil))))))

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
