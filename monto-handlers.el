;;;;;;;;;;;;;;;;;;;;;;;
;;; Library Globals ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar monto--errors--buffer nil
  "The buffer errors are displayed in.")

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handler Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun monto--errors--handler (errors)
  (unless monto--errors--buffer
    (setq monto--errors--buffer (generate-new-buffer "*monto-errors*"))
    (with-current-buffer monto--errors--buffer
      (read-only-mode 1)))
  (with-current-buffer monto--errors--buffer
    (let ((inhibit-read-only t))
      (goto-char 1)
      (erase-buffer)
      (insert (prin1-to-string errors)))))

(provide 'monto-handlers)
