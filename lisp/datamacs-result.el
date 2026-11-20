;;; datamacs-result.el --- Result buffer for datamacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Shows query results in a read-only side window.  Press RET or click
;; on a cell to change its value.  With C-u, the value is set to NULL.

;;; Code:

(require 'org-table)
(require 'json)
(require 'datamacs-core)

(defvar-local datamacs-result--result nil
  "Result shown in this buffer.")

(defvar-local datamacs-result--spec nil
  "Spec of the connection the result came from.")

(defvar-keymap datamacs-result-mode-map
  :parent special-mode-map
  "RET" #'datamacs-result-edit-cell
  "<mouse-1>" #'datamacs-result-edit-cell)

(define-derived-mode datamacs-result-mode special-mode "Datamacs-Result"
  "Read-only view of a query result."
  (setq truncate-lines t))

(defun datamacs-result-show (result spec)
  "Show RESULT of the SPEC connection in a side window."
  (let ((buffer (get-buffer-create "*datamacs*")))
    (with-current-buffer buffer
      (datamacs-result-mode)
      (setq datamacs-result--result result
            datamacs-result--spec spec)
      (goto-char (point-min))
      (datamacs-result--redisplay))
    (display-buffer buffer '(display-buffer-in-side-window
                             (side . bottom)
                             (window-height . 0.3)))
    (let ((value (datamacs-result-render result)))
      (if (stringp value)
          (message "%s" (car (split-string value "\n")))
        (let ((n (length (or (cdr (memq 'hline value)) value))))
          (message "%d row%s" n (if (= n 1) "" "s")))))))

(defun datamacs-result--redisplay ()
  "Draw the result again, keeping point on the same cell."
  (let ((line (line-number-at-pos))
        (column (current-column))
        (value (datamacs-result-render datamacs-result--result))
        (inhibit-read-only t))
    (erase-buffer)
    (insert (if (stringp value) value (orgtbl-to-orgtbl value nil)))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))

(defun datamacs-result--cell-at-point ()
  "Return (ROW . COLUMN) of the table cell at point, or nil."
  (let ((result datamacs-result--result))
    (when (datamacs-table-result-p result)
      (let ((row (- (line-number-at-pos) 3))
            (column (1- (cl-count ?| (buffer-substring
                                      (line-beginning-position) (point))))))
        (when (and (< -1 row (length (datamacs-table-result-rows result)))
                   (< -1 column (length (datamacs-table-result-columns result))))
          (cons row column))))))

(defun datamacs-result-edit-cell (&optional event)
  "Change the value of the cell at point or at mouse EVENT.
With a prefix argument, set it to NULL."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (posn-set-point (event-end event)))
  (let ((result datamacs-result--result)
        (spec datamacs-result--spec)
        (cell (datamacs-result--cell-at-point)))
    (unless cell
      (user-error "No cell here"))
    (when (plist-get spec :readonly)
      (user-error "Connection is read-only"))
    (unless (datamacs-table-result-table result)
      (user-error "Only single-table results with a primary key can be edited"))
    (let* ((columns (datamacs-table-result-columns result))
           (row (nth (car cell) (datamacs-table-result-rows result)))
           (column (nth (cdr cell) columns))
           (old (nth (cdr cell) row))
           (new (unless current-prefix-arg
                  (read-string (format "%s: " column)
                               (and old (format "%s" old)))))
           (key (mapcar (lambda (k)
                          (cons k (nth (cl-position k columns :test #'equal) row)))
                        (datamacs-table-result-key result))))
      (when (zerop (datamacs-call-with-connection
                    spec
                    (lambda (conn)
                      (datamacs-driver-update-cell
                       conn (datamacs-table-result-table result)
                       key column old new))))
        (user-error "Not updated: the row was changed or removed"))
      (setf (nth (cdr cell) row) new)
      (datamacs-result--redisplay)
      (message "Updated %s" column))))

(cl-defgeneric datamacs-result-render (result)
  "Convert RESULT into a value Org can insert.")

(cl-defmethod datamacs-result-render ((result datamacs-table-result))
  "Render RESULT as an Org table."
  (append (list (datamacs-table-result-columns result) 'hline)
          (mapcar (lambda (row) (mapcar #'datamacs-result--cell row))
                  (datamacs-table-result-rows result))))

(cl-defmethod datamacs-result-render ((result datamacs-affected-result))
  "Render RESULT as a short message."
  (let ((n (datamacs-affected-result-count result)))
    (format "%d row%s affected" n (if (= n 1) "" "s"))))

(cl-defmethod datamacs-result-render ((result datamacs-value-result))
  "Render RESULT as text, or a list as a one-column table."
  (let ((value (datamacs-value-result-value result)))
    (if (consp value)
        (mapcar (lambda (v) (list (datamacs-result--cell v))) value)
      (format "%s" value))))

(cl-defmethod datamacs-result-render ((result datamacs-documents-result))
  "Render RESULT as pretty-printed JSON."
  (let ((json-encoding-pretty-print t))
    (json-encode (vconcat (datamacs-documents-result-documents result)))))

(defun datamacs-result--cell (value)
  "Make VALUE safe for an Org table cell."
  (cond ((null value) "NULL")
        ((stringp value)
         (replace-regexp-in-string
          "\n" " " (replace-regexp-in-string "|" "\\vert{}" value t t)))
        (t value)))

(provide 'datamacs-result)
;;; datamacs-result.el ends here
