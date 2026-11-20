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
    (if-let* ((columns (datamacs-sqlite--columns handle query)))
        (datamacs-table-result-create
         :columns columns :rows (sqlite-select handle query))
      ;; `sqlite-select' ignores write errors, so writes use `sqlite-execute'.
      (let ((before (datamacs-sqlite--total-changes handle)))
        (sqlite-execute handle query)
        (datamacs-affected-result-create
         :count (- (datamacs-sqlite--total-changes handle) before))))))

(defun datamacs-sqlite--columns (handle query)
  "Return the column names of QUERY on HANDLE without running it."
  (let ((stmt (sqlite-select handle query nil 'set)))
    (prog1 (sqlite-columns stmt)
      (sqlite-finalize stmt))))

(defun datamacs-sqlite--total-changes (handle)
  "Return the number of rows changed so far on HANDLE."
  (caar (sqlite-select handle "SELECT total_changes()")))

(provide 'datamacs-sqlite)
;;; datamacs-sqlite.el ends here
