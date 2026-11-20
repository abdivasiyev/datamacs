;;; datamacs.el --- Database client for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; Author: Asliddin Azizovich <asliddin.abdivasiyev@gmail.com>
;; Maintainer: Asliddin Azizovich <asliddin.abdivasiyev@gmail.com>

;; Homepage: https://github.com/abdivasiyev/datamacs
;; Keywords: data, tools

;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Datamacs is a database client for Emacs.  It lets you keep your
;; connections inside Emacs, so you don't have to switch between
;; apps while you are working.
;;
;; Passwords are managed by auth-source, with ~/.authinfo.gpg (or
;; ~/.authinfo).

;;; Code:

(require 'datamacs-core)
(require 'datamacs-sqlite)
(require 'datamacs-result)
(require 'ob-datamacs)

(provide 'datamacs)
;;; datamacs.el ends here
