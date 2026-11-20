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

(provide 'datamacs-sqlite)
;;; datamacs-sqlite.el ends here
