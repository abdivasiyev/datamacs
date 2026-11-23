;;; datamacs-result.el --- Result buffer for datamacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The Datamacs Project contributors

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Shows each query result in its own read-only window at the bottom
;; of the frame, so results can be compared.  Press RET or click
;; on a cell to change its value.  With C-u, the value is set to NULL.
;; Press g to run the query again.  It always runs read-only, so a
;; query that writes, like UPDATE ... RETURNING, fails instead.

;;; Code:

(require 'org-table)
(require 'json)
(require 'datamacs-core)

(defvar-local datamacs-result--result nil
  "Result shown in this buffer.")

(defvar-local datamacs-result--spec nil
  "Spec of the connection the result came from.")

(defvar-local datamacs-result--query nil
  "Query that produced the result.")

(defvar-keymap datamacs-result-mode-map
  :parent special-mode-map
  "RET" #'datamacs-result-edit-cell
  "<mouse-1>" #'datamacs-result-edit-cell)

(define-derived-mode datamacs-result-mode special-mode "Datamacs-Result"
  "Read-only view of a query result."
  (setq truncate-lines t)
  (setq-local revert-buffer-function #'datamacs-result--refresh))

(defun datamacs-result--buffers ()
  "Return all result buffers, sorted by name."
  (sort (seq-filter (lambda (b)
                      (eq (buffer-local-value 'major-mode b) 'datamacs-result-mode))
                    (buffer-list))
        (lambda (a b) (string< (buffer-name a) (buffer-name b)))))

(defun datamacs-result--buffer (spec query)
  "Return the result buffer for QUERY on the SPEC connection."
  (or (seq-find (lambda (b)
                  (with-current-buffer b
                    (and (equal datamacs-result--spec spec)
                         (equal datamacs-result--query query))))
                (datamacs-result--buffers))
      (generate-new-buffer
       (format "*datamacs: %s*"
               (truncate-string-to-width
                (replace-regexp-in-string "[ \t\n]+" " " query) 40 nil nil "…")))))

(defun datamacs-result--next-slot ()
  "Return a side window slot right of all shown results."
  (let ((slots (delq nil (mapcar (lambda (w)
                                   (with-current-buffer (window-buffer w)
                                     (and (derived-mode-p 'datamacs-result-mode)
                                          (window-parameter w 'window-slot))))
                                 (window-list)))))
    (if slots (1+ (apply #'max slots)) 0)))

(defun datamacs-result-show (result spec query &optional seconds)
  "Show RESULT of QUERY on the SPEC connection in a side window.
SECONDS is how long the query took."
  (let* ((query (string-trim query))
         (buffer (datamacs-result--buffer spec query)))
    (with-current-buffer buffer
      (datamacs-result-mode)
      (setq datamacs-result--result result
            datamacs-result--spec spec
            datamacs-result--query query)
      (goto-char (point-min))
      (datamacs-result--redisplay))
    (display-buffer buffer `((display-buffer-reuse-window
                              display-buffer-in-side-window)
                             (side . bottom)
                             (slot . ,(datamacs-result--next-slot))
                             (window-height . 0.3)))
    (datamacs-result--summary result seconds)))

(defun datamacs-result--refresh (&rest _)
  "Run the query again on a read-only connection and show the new result."
  (unless (datamacs-table-result-p datamacs-result--result)
    (user-error "Only query results can be refreshed"))
  (let ((result (datamacs-call-with-progress
                 (format "Refreshing query on %s" (datamacs-describe datamacs-result--spec))
                 (lambda ()
                   (datamacs-call-with-connection
                    (datamacs-readonly-spec datamacs-result--spec)
                    (lambda (conn)
                      (datamacs-driver-execute conn datamacs-result--query)))))))
    (setq datamacs-result--result result)
    (datamacs-result--redisplay)
    (datamacs-result--summary result datamacs-progress-seconds)))

(defun datamacs-result--summary (result &optional seconds)
  "Show a one-line summary of RESULT, which took SECONDS, in the echo area."
  (let ((value (datamacs-result-render result))
        (time (if seconds (format " (%.1fs)" seconds) "")))
    (if (stringp value)
        (message "%s%s" (car (split-string value "\n")) time)
      (let ((n (length (or (cdr (memq 'hline value)) value))))
        (message "%d row%s%s" n (if (= n 1) "" "s") time)))))

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
      (when (zerop (datamacs-call-with-progress
                    (format "Updating %s.%s on %s" (datamacs-table-result-table result)
                            column (datamacs-describe spec))
                    (lambda ()
                      (datamacs-call-with-connection
                       spec
                       (lambda (conn)
                         (datamacs-driver-update-cell
                          conn (datamacs-table-result-table result)
                          key column old new))))))
        (user-error "Not updated: the row was changed or removed"))
      (setf (nth (cdr cell) row) new)
      (datamacs-result--redisplay))))

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
