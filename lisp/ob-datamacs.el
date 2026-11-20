;;; ob-datamacs.el --- Org Babel support for datamacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Run queries from Org source blocks:
;;
;;   #+begin_src datamacs :driver sqlite :file ~/notes.db
;;   select * from people
;;   #+end_src
;;
;; Or name a connection from `datamacs-connections':
;;
;;   #+begin_src datamacs :conn notes
;;
;; Set a default for the whole file with Org's own header arguments:
;;
;;   #+PROPERTY: header-args:datamacs :conn notes
;;
;; Results are shown in a read-only side window.  Add `:results replace'
;; to a block to write them into the Org file instead.

;;; Code:

(require 'ob)
(require 'datamacs-core)
(require 'datamacs-result)

(add-to-list 'org-src-lang-modes '("datamacs" . sql))

(defvar org-babel-default-header-args:datamacs '((:results . "none"))
  "Default header arguments for datamacs blocks.")

(defun org-babel-execute:datamacs (body params)
  "Run BODY with Babel PARAMS and return the result for Org."
  (let* ((spec (datamacs-babel--spec params))
         (result (datamacs-call-with-connection
                  spec (lambda (conn) (datamacs-driver-execute conn body)))))
    (if (seq-some (lambda (p) (member p '("none" "silent")))
                  (alist-get :result-params params))
        (progn (datamacs-result-show result spec body) nil)
      (datamacs-result-render result))))

(defun datamacs-babel--spec (params)
  "Build a connection spec from Babel PARAMS.
Header arguments override the `:conn' connection, except that
`:readonly' can only be turned on."
  (let* ((name (alist-get :conn params))
         (conn (and name
                    (or (seq-find (lambda (c) (equal (plist-get c :name) name))
                                  datamacs-connections)
                        (user-error "Unknown connection: %s" name))))
         (driver (or (alist-get :driver params) (plist-get conn :driver)
                     (user-error "Missing :conn or :driver header argument")))
         (spec (copy-sequence conn)))
    (setq spec (plist-put spec :name (or name "babel")))
    (setq spec (plist-put spec :driver (if (stringp driver) (intern driver) driver)))
    (when-let* ((file (alist-get :file params)))
      (setq spec (plist-put spec :file file)))
    (plist-put spec :readonly (or (plist-get conn :readonly)
                                  (equal (alist-get :readonly params) "t")))))

(provide 'ob-datamacs)
;;; ob-datamacs.el ends here
