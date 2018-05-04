;;; -*- lexical-binding: t -*-

(require 'monto-util)

;;;;;;;;;;;;;;;;;;;;
;;; User Options ;;;
;;;;;;;;;;;;;;;;;;;;

(defconst monto-errors-face 'underline
  "The face the error marker is drawn with.")
(defconst monto-highlighting-styles nil
  "An alist of faces cooresponding to styles.")

;;;;;;;;;;;;;;;;;;;;;;;
;;; Library Globals ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar monto-errors--buffer nil
  "The buffer errors are displayed in.")

;;;;;;;;;;;;;;;;;;;;;;
;;; Errors Handler ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun monto-errors--handler (path errors)
  (unless monto-errors--buffer
    (setq monto-errors--buffer (generate-new-buffer "*monto-errors*"))
    (with-current-buffer monto-errors--buffer
      (read-only-mode 1)))
  (unless (get-buffer-window monto-errors--buffer)
    (set-window-buffer (split-window-right) monto-errors--buffer))
  (with-current-buffer monto-errors--buffer
    (let ((inhibit-read-only t))
      (goto-char 1)
      (erase-buffer)
      (remove-overlays (point-min) (point-max) 'monto-errors t)
      (if (= (length errors) 0)
        (insert "No errors!\n")
        (dotimes (i (length errors))
          (let ((err (elt errors i)))
            (insert (monto--json-get err 'description))
            (insert "\n\n"))))))
  (dotimes (i (length errors))
    (let* ((err (elt errors i))
           (loc (monto--json-get err 'offset)))
      (monto-errors--draw path loc))))

(defun monto-errors--draw (path location)
  (let ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (let* ((start (+ (point-min) location))
             (end (1+ location))
             (overlay (make-overlay start end buf)))
        (overlay-put overlay 'face monto-errors-face)
        (overlay-put overlay 'monto-errors t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlighting Handler ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun monto-highlighting--handler (path tokens)
  (let ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (dotimes (i (length tokens))
        (let* ((tok   (elt tokens i))
               (start (+ (point-min) (alist-get 'start_byte tok)))
               (end   (+ (point-min) (alist-get 'end_byte tok)))
			   (color (>-> '(color value) tok))
			   (face  (cdr (assoc color monto-highlighting-styles))))
          (put-text-property start end 'font-lock-face face))))))

(provide 'monto-handlers)
