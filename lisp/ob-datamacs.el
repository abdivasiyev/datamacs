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
;; C-c C-c runs the statement under the cursor, or the selected text.
;;
;; Blocks with the same `:session NAME' share one open connection, so
;; a transaction can span blocks.  M-x datamacs-disconnect closes them.
;;
;; Results are shown in a read-only side window.  Add `:results replace'
;; to a block to write them into the Org file instead.

;;; Code:

(require 'ob)
(require 'org-element)
(require 'datamacs-core)
(require 'datamacs-result)

(add-to-list 'org-src-lang-modes '("datamacs" . sql))

(defvar org-babel-default-header-args:datamacs '((:results . "none"))
  "Default header arguments for datamacs blocks.")

(defun org-babel-execute:datamacs (body params)
  "Run BODY with Babel PARAMS and return the result for Org."
  (let* ((body (or (datamacs-babel--query) body))
         (spec (datamacs-babel--spec params))
         (result (datamacs-call-with-connection
                  spec (lambda (conn) (datamacs-driver-execute conn body)))))
    (if (seq-some (lambda (p) (member p '("none" "silent")))
                  (alist-get :result-params params))
        (progn (datamacs-result-show result spec body) nil)
      (datamacs-result-render result))))

(defun datamacs-babel--query ()
  "Return the selected text or the statement at point in the block body.
Return nil if point is outside the body."
  (let ((block (org-element-at-point)))
    (when (eq (org-element-type block) 'src-block)
      (let ((beg (save-excursion
                   (goto-char (org-element-property :post-affiliated block))
                   (line-beginning-position 2)))
            (end (save-excursion
                   (goto-char (org-element-property :end block))
                   (skip-chars-backward " \t\n")
                   (line-beginning-position))))
        (cond ((and (use-region-p)
                    (<= beg (region-beginning)) (<= (region-end) end))
               (buffer-substring-no-properties (region-beginning) (region-end)))
              ((<= beg (point) end)
               (datamacs-sql-statement-at
                (buffer-substring-no-properties beg end) (- (point) beg))))))))

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
    (setq spec (plist-put spec :name (or name (alist-get :file params) "babel")))
    (setq spec (plist-put spec :driver (if (stringp driver) (intern driver) driver)))
    (let ((session (alist-get :session params)))
      (unless (member session '(nil "none"))
        (setq spec (plist-put spec :session session))))
    (when-let* ((file (alist-get :file params)))
      (setq spec (plist-put spec :file (expand-file-name file))))
    (plist-put spec :readonly (or (plist-get conn :readonly)
                                  (equal (alist-get :readonly params) "t")))))

(provide 'ob-datamacs)
;;; ob-datamacs.el ends here
