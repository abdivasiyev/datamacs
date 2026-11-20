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
(require 'org-table)
(require 'json)
(require 'datamacs-core)

(add-to-list 'org-src-lang-modes '("datamacs" . sql))

(defvar org-babel-default-header-args:datamacs '((:results . "none"))
  "Default header arguments for datamacs blocks.")

(defun org-babel-execute:datamacs (body params)
  "Run BODY with Babel PARAMS and return the result for Org."
  (let* ((conn (datamacs-driver-open
                (intern (or (alist-get :driver params)
                            (user-error "Missing :driver header argument")))
                (list :name "babel"
                      :file (alist-get :file params)
                      :readonly (equal (alist-get :readonly params) "t"))))
         (value (unwind-protect
                    (datamacs-babel-render (datamacs-driver-execute conn body))
                  (datamacs-driver-close conn))))
    (if (seq-some (lambda (p) (member p '("none" "silent")))
                  (alist-get :result-params params))
        (datamacs-babel--show value)
      value)))

(define-derived-mode datamacs-result-mode special-mode "Datamacs-Result"
  "Read-only view of a query result."
  (setq truncate-lines t))

(defun datamacs-babel--show (value)
  "Show VALUE in a read-only side window."
  (let ((buffer (get-buffer-create "*datamacs*")))
    (with-current-buffer buffer
      (datamacs-result-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if (stringp value) value (orgtbl-to-orgtbl value nil)))
        (goto-char (point-min))))
    (display-buffer buffer '(display-buffer-in-side-window
                             (side . bottom)
                             (window-height . 0.3)))
    (if (stringp value)
        (message "%s" (car (split-string value "\n")))
      (let ((n (length (if (memq 'hline value) (cdr (memq 'hline value)) value))))
        (message "%d row%s" n (if (= n 1) "" "s"))))
    nil))

(cl-defgeneric datamacs-babel-render (result)
  "Convert RESULT into a value Org can insert.")

(cl-defmethod datamacs-babel-render ((result datamacs-table-result))
  "Render RESULT as an Org table."
  (append (list (datamacs-table-result-columns result) 'hline)
          (mapcar (lambda (row) (mapcar #'datamacs-babel--cell row))
                  (datamacs-table-result-rows result))))

(cl-defmethod datamacs-babel-render ((result datamacs-affected-result))
  "Render RESULT as a short message."
  (let ((n (datamacs-affected-result-count result)))
    (format "%d row%s affected" n (if (= n 1) "" "s"))))

(cl-defmethod datamacs-babel-render ((result datamacs-value-result))
  "Render RESULT as text, or a list as a one-column table."
  (let ((value (datamacs-value-result-value result)))
    (if (consp value)
        (mapcar (lambda (v) (list (datamacs-babel--cell v))) value)
      (format "%s" value))))

(cl-defmethod datamacs-babel-render ((result datamacs-documents-result))
  "Render RESULT as pretty-printed JSON."
  (let ((json-encoding-pretty-print t))
    (json-encode (vconcat (datamacs-documents-result-documents result)))))

(defun datamacs-babel--cell (value)
  "Make VALUE safe for an Org table cell."
  (cond ((null value) "NULL")
        ((stringp value)
         (replace-regexp-in-string
          "\n" " " (replace-regexp-in-string "|" "\\vert{}" value t t)))
        (t value)))

(provide 'ob-datamacs)
;;; ob-datamacs.el ends here
