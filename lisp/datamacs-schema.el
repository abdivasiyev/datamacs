;;; datamacs-schema.el --- Schema sidebar for datamacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; M-x datamacs-schema shows all connections in a sidebar.  RET on a
;; connection shows its tables, RET on a table shows its first rows,
;; TAB folds and g reads the schema again from the database.
;;
;; Schemas are cached on disk with `multisession', so the database is
;; only read the first time or on g.  The cache also completes table
;; and column names in datamacs blocks with C-M-i.

;;; Code:

(require 'org)
(require 'multisession)
(require 'subr-x)
(require 'datamacs-core)
(require 'datamacs-result)
(require 'ob-datamacs)

(define-multisession-variable datamacs-schema-cache nil
  "Cached schemas, an alist of (KEY TIME . SCHEMA)."
  :package "datamacs")

(defun datamacs-schema-get (spec &optional reload)
  "Return (TIME . SCHEMA) of the SPEC connection from the cache.
Read it from the database if it is not cached or RELOAD is non-nil."
  (let ((key (datamacs-schema--key spec)))
    (or (and (not reload)
             (assoc-default key (multisession-value datamacs-schema-cache)))
        (let ((entry (cons (format-time-string "%Y-%m-%d %H:%M")
                           (datamacs-call-with-connection
                            (datamacs-readonly-spec spec)
                            #'datamacs-driver-schema))))
          (setf (multisession-value datamacs-schema-cache)
                (cons (cons key entry)
                      (assoc-delete-all
                       key (copy-sequence (multisession-value datamacs-schema-cache)))))
          entry))))

(defun datamacs-schema--key (spec)
  "Return the cache key of SPEC: its driver and location."
  (mapcar (lambda (k)
            (let ((v (plist-get spec k)))
              (if (and v (eq k :file)) (expand-file-name v) v)))
          '(:driver :file :host :port :database)))

(defun datamacs-schema--heading (spec)
  "Return the sidebar heading of the SPEC connection."
  (let ((entry (assoc-default (datamacs-schema--key spec)
                              (multisession-value datamacs-schema-cache))))
    (propertize (format "* %s (%s)%s" (plist-get spec :name) (plist-get spec :driver)
                        (if entry (format " · cached %s" (car entry)) ""))
                'datamacs-spec spec)))

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
            (insert (datamacs-schema--heading spec) "\n")
            (when (assoc-default (datamacs-schema--key spec)
                                 (multisession-value datamacs-schema-cache))
              (forward-line -1)
              (datamacs-schema--show nil)))))
      (setq pos (and current (datamacs-schema--find (plist-get current :name)))))
    (let ((window (display-buffer buffer '(display-buffer-in-side-window
                                           (side . left)
                                           (window-width . 0.25)))))
      (select-window window)
      (when pos
        (set-window-point window pos)))))

(defun datamacs-schema--block-spec ()
  "Return the connection spec of the datamacs block at point, or nil.
In a \\[org-edit-special] buffer, use the block being edited."
  (let ((info (cond ((bound-and-true-p org-src--babel-info))
                    ((derived-mode-p 'org-mode)
                     (org-babel-get-src-block-info 'no-eval)))))
    (when (equal (car info) "datamacs")
      (datamacs-babel--spec (nth 2 info)))))

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
  "Show the table at point, or the tables of the connection at point."
  (interactive)
  (if-let* ((table (get-text-property (line-beginning-position) 'datamacs-table)))
      (datamacs-schema--preview table)
    (datamacs-schema--show nil)))

(defun datamacs-schema-refresh ()
  "Read the schema of the connection at point again from the database."
  (interactive)
  (datamacs-schema--show t))

(defun datamacs-schema--show (reload)
  "Show the tables of the connection at point.
With RELOAD, read them from the database instead of the cache."
  (let* ((spec (or (datamacs-schema--spec) (user-error "No connection here")))
         (schema (cdr (datamacs-schema-get spec reload)))
         (start (datamacs-schema--find (plist-get spec :name)))
         (inhibit-read-only t))
    (goto-char start)
    (forward-line 1)
    (delete-region start (if (re-search-forward "^\\* " nil t)
                             (line-beginning-position)
                           (point-max)))
    (goto-char start)
    (insert (datamacs-schema--heading spec) "\n")
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
    (goto-char start)
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

(defun datamacs-completion-at-point ()
  "Complete table and column names in datamacs blocks."
  (when-let* ((spec (ignore-errors (datamacs-schema--block-spec)))
              (schema (cdr (ignore-errors (datamacs-schema-get spec)))))
    (let ((notes (make-hash-table :test #'equal)))
      (dolist (s schema)
        (dolist (table (cdr s))
          (puthash (car table) "table" notes)
          (dolist (column (nth 2 table))
            (let ((note (gethash (car column) notes)))
              (unless (equal note "table")
                (puthash (car column)
                         (if note
                             (concat note ", " (car table))
                           (concat "column of " (car table)))
                         notes))))))
      (list (save-excursion (skip-chars-backward "[:alnum:]_") (point))
            (save-excursion (skip-chars-forward "[:alnum:]_") (point))
            (hash-table-keys notes)
            :annotation-function (lambda (c) (concat "  " (gethash c notes)))
            :exclusive 'no))))

(defun datamacs-completion-setup ()
  "Use `datamacs-completion-at-point' in this buffer."
  (add-hook 'completion-at-point-functions #'datamacs-completion-at-point nil t))

(add-hook 'org-mode-hook #'datamacs-completion-setup)
(add-hook 'org-src-mode-hook #'datamacs-completion-setup)

(provide 'datamacs-schema)
;;; datamacs-schema.el ends here
