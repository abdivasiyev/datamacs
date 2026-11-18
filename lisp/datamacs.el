;;; datamacs.el --- A tool to collect all database connections inside Emacs itself. -*- lexical-binding:t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; Author: Asliddin Azizovich <asliddin.abdivasiyev@gmail.com>
;; Maintainer: Asliddin Azizovich <asliddin.abdivasiyev@gmail.com>

;; Homepage: https://github.com/abdivasiyev/datamacs
;; Keywords: database tools sql nosql

;; Package-Version: 0.0.1
;; Package-Requires: (
;;     (emacs "29.1")
;; )

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Datamacs is a database client for Emacs.
;; It lets you collect your connections inside an Emacs and
;; you don't have to switch between apps while you are working.
;;
;; Passwords are managed by auth-source, with ~/.authinfo.gpg (or ~/.authinfo).

;;; Code:
(require 'cl-lib)

;;; Configuration what user writes to connect to their instances
(defcustom datamacs-connections nil
  "Database connections known to datamacs.
Each element is a plist.
Every connection has its own :name and :driver.
Other keys depend on the driver."
  :type '(repeat plist)
  :group 'datamacs)

;;; Live connections
(cl-defstruct (datamacs-conn (:constructor nil))
  "An open database connection. Shared by all database drivers."
  (name nil :documentation "Connection name")
  (driver nil :documentation "Driver type to connect")
  (spec nil :documentation "Connection plist this was opened from")
  (status 'closed :documentation "The one of `open', `closed' or `error'")
  (handle nil :documentation "The driver's native object"))

;; SQLite connection
(cl-defstruct (datamacs-sqlite-conn
               (:include datamacs-conn (driver 'sqlite))
               (:constructor datamacs-sqlite-conn-create))
  "An open SQLite connection"
  (file nil :documentation "Absolute path of the database file"))

(provide 'datamacs)
;;; datamacs.el ends here
