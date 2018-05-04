;;; -*- lexical-binding: t -*-

(require 'monto-handlers)
(require 'monto-macros)
(require 'monto-protocol)

;; Monto language map. Add a mapping from extension to language name to enable
;; Monto for the language. Entries should be two strings, e.g. ("sv" . "silver").
(defconst monto-language-alist nil
  "An alist of languages to use with Monto.")

(defconst monto-product-handlers (list
  (cons "errors" #'monto-errors--handler)
  (cons "highlighting" #'monto-highlighting--handler))
  "Handlers for various products.")

(defconst monto--debounce-time 50000 ; 0.05 sec
  "The minimum number of microseconds to wait.")
(defvar monto--last-send '(0 0 0)
  "The last time a message was sent.")

(defun monto--language ()
  ; TODO Rewrite this to be less... bad. Maybe this needs a macro for the
  ; let-if pair?
  "Returns the language associated with the current buffer, if Monto is enabled
  for the file's extension. If not, returns nil."
  (let ((path (buffer-file-name)))
    (if path
      (let ((ext (file-name-extension path)))
        (if (> (length ext) 0)
          (let ((entry (assoc ext monto-language-alist)))
            (if entry (cdr entry))))))))

(defun monto--debounce (now)
  (let* ((res (cl-mapcar #'- now monto--last-send))
         (usec (+ (nth 2 res) (* 1000000 (cadr res)))))
    (and (> usec monto--debounce-time))))

(define-derived-mode monto-mode prog-mode "Monto"
  "Major mode using Monto."
  (monto-init)
  (add-hook 'after-change-functions 'monto-change nil t)
  (monto-change 0 0 0))

(defun monto-change (start end len)
  "Fires on a change in a monto-mode'd buffer."
  (let ((path (buffer-file-name))
        (src  (buffer-string))
        (lang (monto--language))
        (now  (cl-subseq (current-time) 0 3)))
    (when (and lang (monto--debounce now))
      (monto-update-source path src lang)
      (monto-get-product path "highlighting" lang
        (lambda (res)
		  (condition-case err
		    (monto-highlighting--handler path (alist-get 'contents res))
			(error (print err)))))
      (setq monto--last-send now))))

(provide 'monto-mode)
