;;; datamacs-core.el --- Core types for datamacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Connection types and settings shared by all datamacs drivers.

;;; Code:

(require 'cl-lib)

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
  (rows nil :documentation "Rows, each a list of values."))

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

(provide 'datamacs-core)
;;; datamacs-core.el ends here
