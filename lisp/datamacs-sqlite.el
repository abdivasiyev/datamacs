;;; datamacs-sqlite.el --- SQLite driver for datamacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; SQLite driver, built on the SQLite support in Emacs 29 and later.

;;; Code:

(require 'datamacs-core)

(cl-defstruct (datamacs-sqlite-conn
               (:include datamacs-conn (driver 'sqlite))
               (:constructor datamacs-sqlite-conn-create))
  "An open SQLite connection."
  (file nil :documentation "Absolute path of the database file."))

(cl-defmethod datamacs-driver-open ((_driver (eql 'sqlite)) spec)
  "Open the SQLite database described by SPEC."
  (unless (sqlite-available-p)
    (error "This Emacs was built without SQLite support"))
  (let ((file (expand-file-name (plist-get spec :file))))
    (unless (file-exists-p file)
      (user-error "No such database file: %s" file))
    (let* ((handle (sqlite-open file))
           (conn (datamacs-sqlite-conn-create
                  :name (plist-get spec :name)
                  :spec spec
                  :file file
                  :readonly (plist-get spec :readonly)
                  :handle handle
                  :status 'open)))
      (when (datamacs-conn-readonly conn)
        (sqlite-execute handle "PRAGMA query_only = ON"))
      conn)))

(cl-defmethod datamacs-driver-close ((conn datamacs-sqlite-conn))
  "Close the SQLite connection CONN."
  (when (datamacs-conn-handle conn)
    (sqlite-close (datamacs-conn-handle conn)))
  (setf (datamacs-conn-handle conn) nil
        (datamacs-conn-status conn) 'closed))

(cl-defmethod datamacs-driver-execute ((conn datamacs-sqlite-conn) query)
  "Run QUERY on the SQLite connection CONN."
  (let ((handle (datamacs-conn-handle conn)))
    (unless handle
      (user-error "Connection %s is closed" (datamacs-conn-name conn)))
    (setq query (or (datamacs-sql-statement-at query 0) query))
    (if-let* ((columns (datamacs-sqlite--columns handle query)))
        ;; `sqlite-select' hides errors, so rows come from `sqlite-execute'.
        (let ((source (datamacs-sqlite--source handle query columns))
              (rows (sqlite-execute handle query)))
          (datamacs-table-result-create
           :columns columns :rows (and (consp rows) rows)
           :table (car source) :key (cdr source)))
      (let ((before (datamacs-sqlite--total-changes handle)))
        (sqlite-execute handle query)
        (datamacs-affected-result-create
         :count (- (datamacs-sqlite--total-changes handle) before))))))

(defun datamacs-sqlite--columns (handle query)
  "Return the column names of QUERY on HANDLE without running it."
  (let ((stmt (sqlite-select handle query nil 'set)))
    (prog1 (sqlite-columns stmt)
      (sqlite-finalize stmt))))

(cl-defmethod datamacs-driver-in-transaction-p ((conn datamacs-sqlite-conn))
  "Return non-nil if the SQLite connection CONN has an open transaction."
  ;; SQLite refuses BEGIN inside a transaction.
  (let ((handle (datamacs-conn-handle conn)))
    (condition-case nil
        (progn (sqlite-execute handle "BEGIN")
               (sqlite-execute handle "ROLLBACK")
               nil)
      (sqlite-error t))))

(cl-defmethod datamacs-driver-update-cell
  ((conn datamacs-sqlite-conn) table key column old new)
  "Change COLUMN from OLD to NEW in the TABLE row matching KEY on CONN."
  (let ((q #'datamacs-sqlite--quote))
    (sqlite-execute
     (datamacs-conn-handle conn)
     (format "UPDATE %s SET %s = ? WHERE %s AND %s IS ?"
             (funcall q table) (funcall q column)
             (mapconcat (lambda (k) (concat (funcall q (car k)) " IS ?"))
                        key " AND ")
             (funcall q column))
     (append (list new) (mapcar #'cdr key) (list old)))))

(cl-defmethod datamacs-driver-schema ((conn datamacs-sqlite-conn))
  "Return the schema of the SQLite connection CONN."
  (let ((handle (datamacs-conn-handle conn)))
    (mapcar
     (lambda (db)
       (let ((schema (datamacs-sqlite--quote (nth 1 db))))
         (cons (nth 1 db)
               (mapcar
                (lambda (table)
                  (list (nth 0 table) (nth 1 table)
                        (mapcar (lambda (c) (list (nth 1 c) (nth 2 c) (> (nth 5 c) 0)))
                                (sqlite-select
                                 handle (format "PRAGMA %s.table_info(%s)" schema
                                                (datamacs-sqlite--quote (nth 0 table)))))))
                (sqlite-select
                 handle (format "SELECT name, type FROM %s.sqlite_master
WHERE type IN ('table', 'view') AND name NOT LIKE 'sqlite_%%'
ORDER BY name" schema))))))
     (seq-remove (lambda (db) (equal (nth 1 db) "temp"))
                 (sqlite-select handle "PRAGMA database_list")))))

(defun datamacs-sqlite--source (handle query columns)
  "Return (TABLE . KEY) if rows of QUERY on HANDLE can be edited.
Only a plain SELECT from one table whose COLUMNS include the
primary key qualifies."
  (let ((case-fold-search t)
        (sql (string-trim (replace-regexp-in-string "[ \t\n]+" " " query))))
    (when (and (string-match "\\`select .+? from \\([[:alnum:]_]+\\)\\(?: \\(?:where\\|order\\|limit\\) .*\\)?;?\\'" sql)
               (not (string-match-p "\\_<\\(?:join\\|group\\|having\\|union\\|intersect\\|except\\|distinct\\)\\_>" sql)))
      (let* ((table (match-string 1 sql))
             (info (sqlite-select handle (format "PRAGMA table_info(%s)"
                                                 (datamacs-sqlite--quote table))))
             (key (mapcar #'cadr (sort (seq-filter (lambda (c) (> (nth 5 c) 0)) info)
                                       (lambda (a b) (< (nth 5 a) (nth 5 b)))))))
        (when (and key (seq-every-p (lambda (k) (member k columns)) key))
          (cons table key))))))

(defun datamacs-sqlite--quote (name)
  "Quote NAME as an SQL identifier."
  (concat "\"" (string-replace "\"" "\"\"" name) "\""))

(defun datamacs-sqlite--total-changes (handle)
  "Return the number of rows changed so far on HANDLE."
  (caar (sqlite-select handle "SELECT total_changes()")))

(provide 'datamacs-sqlite)
;;; datamacs-sqlite.el ends here
