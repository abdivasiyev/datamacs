;;; datamacs.el --- Database client for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; Author: Asliddin Azizovich <asliddin.abdivasiyev@gmail.com>
;; Maintainer: Asliddin Azizovich <asliddin.abdivasiyev@gmail.com>

;; Homepage: https://github.com/abdivasiyev/datamacs
;; Keywords: data, tools

;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
(require 'datamacs-postgres)
(require 'datamacs-result)
(require 'ob-datamacs)
(require 'datamacs-schema)

(provide 'datamacs)
;;; datamacs.el ends here
