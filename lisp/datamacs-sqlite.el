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

(provide 'datamacs-sqlite)
;;; datamacs-sqlite.el ends here
