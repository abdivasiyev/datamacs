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
  (let* ((spec (list :name "babel"
                     :driver (intern (or (alist-get :driver params)
                                         (user-error "Missing :driver header argument")))
                     :file (alist-get :file params)
                     :readonly (equal (alist-get :readonly params) "t")))
         (result (datamacs-call-with-connection
                  spec (lambda (conn) (datamacs-driver-execute conn body)))))
    (if (seq-some (lambda (p) (member p '("none" "silent")))
                  (alist-get :result-params params))
        (progn (datamacs-result-show result spec) nil)
      (datamacs-result-render result))))

(provide 'ob-datamacs)
;;; ob-datamacs.el ends here
