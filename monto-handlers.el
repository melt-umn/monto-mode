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

(defmacro def-monto-highlighting-styles (&rest styles)
  (let ((ast nil))
    (dolist (asc styles)
      (let ((style (car asc))
            (attrs (cdr asc))
            (face (gensym)))
        (let ((def `(defface ,face '((t ,@attrs)) "The docs are mandatory."))
              (add `(add-to-list
                      'monto-highlighting-styles
                      (cons ,(prin1-to-string style) ',face))))
          (setq ast (cons def
                    (cons add
                          ast))))))
    `(prog1 nil ,@ast)))

(defun monto-highlighting--handler (path tokens)
  (let ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (dotimes (i (length tokens))
        (let* ((tok (elt tokens i))
               (length (monto--json-get tok 'length))
               (offset (monto--json-get tok 'offset))
               (style (monto--json-get tok 'style))
               (start (+ (point-min) offset))
               (end (+ start length)))
          (if (> (length style) 0)
            (let* ((style (elt style 0))
                   (face (cdr (assoc style monto-highlighting-styles))))
              (if face
                (let ((overlay (make-overlay start end buf)))
                  (overlay-put overlay 'face face)
                  (overlay-put overlay 'monto-highlighting t))
                (print (concat "Unknown style: " style))))))))))

(provide 'monto-handlers)
