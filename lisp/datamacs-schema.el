;;; datamacs-schema.el --- Schema sidebar for datamacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; M-x datamacs-schema shows all connections in a sidebar.  RET on a
;; connection loads its tables, RET on a table shows its first rows,
;; TAB folds and g reloads.

;;; Code:

(require 'org)
(require 'datamacs-core)
(require 'datamacs-result)
(require 'ob-datamacs)

(defvar-keymap datamacs-schema-mode-map
  "RET" #'datamacs-schema-open
  "g" #'datamacs-schema-refresh
  "q" #'quit-window)

(define-minor-mode datamacs-schema-mode
  "Keys for the datamacs schema buffer."
  :keymap datamacs-schema-mode-map)

(defun datamacs-schema ()
  "Show the schema sidebar, at the connection of the block at point."
  (interactive)
  (let ((current (datamacs-schema--block-spec))
        (buffer (get-buffer-create "*datamacs schema*"))
        pos)
    (with-current-buffer buffer
      (unless datamacs-schema-mode
        (org-mode)
        (datamacs-schema-mode 1)
        (setq buffer-read-only t))
      (setq-local org-pretty-entities nil)
      (let ((inhibit-read-only t))
        (dolist (spec (append datamacs-connections (and current (list current))))
          (unless (datamacs-schema--find (plist-get spec :name))
            (goto-char (point-max))
            (insert (propertize (format "* %s (%s)" (plist-get spec :name)
                                        (plist-get spec :driver))
                                'datamacs-spec spec)
                    "\n"))))
      (setq pos (and current (datamacs-schema--find (plist-get current :name)))))
    (let ((window (display-buffer buffer '(display-buffer-in-side-window
                                           (side . left)
                                           (window-width . 0.25)))))
      (select-window window)
      (when pos
        (set-window-point window pos)))))

(defun datamacs-schema--block-spec ()
  "Return the connection spec of the datamacs block at point, or nil."
  (when (derived-mode-p 'org-mode)
    (when-let* ((info (org-babel-get-src-block-info 'no-eval)))
      (when (equal (car info) "datamacs")
        (datamacs-babel--spec (nth 2 info))))))

(defun datamacs-schema--find (name)
  "Return the position of the connection heading NAME, or nil."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (re-search-forward "^\\* " nil t)
        (when (equal (plist-get (datamacs-schema--spec) :name) name)
          (throw 'found (line-beginning-position)))))))

(defun datamacs-schema--spec ()
  "Return the connection spec of the heading at or above point."
  (save-excursion
    (end-of-line)
    (when (re-search-backward "^\\* " nil t)
      (get-text-property (point) 'datamacs-spec))))

(defun datamacs-schema-open ()
  "Show the table at point, or load the connection at point."
  (interactive)
  (if-let* ((table (get-text-property (line-beginning-position) 'datamacs-table)))
      (datamacs-schema--preview table)
    (datamacs-schema-refresh)))

(defun datamacs-schema-refresh ()
  "Load the tables of the connection at point."
  (interactive)
  (let* ((spec (or (datamacs-schema--spec) (user-error "No connection here")))
         (schema (datamacs-call-with-connection
                  (plist-put (copy-sequence spec) :readonly t)
                  #'datamacs-driver-schema))
         (inhibit-read-only t))
    (goto-char (datamacs-schema--find (plist-get spec :name)))
    (forward-line 1)
    (delete-region (point) (if (re-search-forward "^\\* " nil t)
                               (line-beginning-position)
                             (point-max)))
    (dolist (s schema)
      (insert "** " (car s) "\n")
      (dolist (table (cdr s))
        (insert (propertize (format "*** %s (%s)" (nth 0 table) (nth 1 table))
                            'datamacs-table (cons (car s) (nth 0 table)))
                "\n")
        (let ((width (apply #'max 0 (mapcar (lambda (c) (length (car c)))
                                            (nth 2 table)))))
          (dolist (column (nth 2 table))
            (insert "  " (string-pad (nth 0 column) width) "  "
                    (propertize (nth 1 column) 'font-lock-face 'font-lock-type-face)
                    (if (nth 2 column)
                        (propertize "  PK" 'font-lock-face 'font-lock-keyword-face)
                      "")
                    "\n")))))
    (goto-char (datamacs-schema--find (plist-get spec :name)))
    (org-fold-hide-subtree)
    (org-fold-show-children 2)))

(defun datamacs-schema--preview (table)
  "Show the first rows of TABLE, a (SCHEMA . NAME) pair."
  (let* ((spec (datamacs-schema--spec))
         (name (if (equal (car table) "main")
                   (datamacs-schema--name (cdr table))
                 (concat (datamacs-schema--name (car table)) "."
                         (datamacs-schema--name (cdr table)))))
         (query (format "SELECT * FROM %s LIMIT 100" name)))
    (datamacs-result-show
     (datamacs-call-with-connection
      spec (lambda (conn) (datamacs-driver-execute conn query)))
     spec query)))

(defun datamacs-schema--name (name)
  "Return NAME, quoted if it is not a plain SQL identifier."
  (if (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*\\'" name)
      name
    (concat "\"" (string-replace "\"" "\"\"" name) "\"")))

(provide 'datamacs-schema)
;;; datamacs-schema.el ends here
