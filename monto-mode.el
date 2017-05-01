(require 'monto-protocol)

;; Variable for whether monto-mode is enabled or not.
(defvar monto-mode nil
  "Mode for Monto syntax highlighting.")
(make-variable-buffer-local 'monto-mode)

;; Monto language map. Add a mapping from extension to language name to enable
;; Monto for the language. Entries should be two strings, e.g. ("sv" . "silver").
(defvar monto-language-alist nil
  "An alist of languages to use with Monto.")

(defun monto--language ()
  ; TODO Rewrite this to be less... bad. Maybe this needs a macro for the
  ; let-if pair?
  (let ((path (buffer-file-name)))
	(if path
	  (let ((ext (file-name-extension path)))
		(if (> (length ext) 0)
		  (let ((entry (assoc ext monto-language-alist)))
			(if entry (cdr entry))))))))

;; Monto Mode entry function.
(defun monto-mode (&optional arg)
  "Monto minor mode."
  (interactive "P")
  (setq monto-mode
    (if (null arg)
        (not monto-mode)
        (> (prefix-numeric-value arg) 0)))
  (if monto-mode
    (add-hook 'after-change-functions 'monto-change nil t)
    (remove-hook 'after-change-functions 'monto-change t)))

;; Register monto-mode.
(if (not (assq 'monto-mode minor-mode-alist))
  (setq minor-mode-alist
    (cons '(monto-mode " Monto")
          minor-mode-alist)))

;; Monto on-change function.
(defun monto-change (start end len)
  (let ((lang (monto--language)))
	(when lang
	  (print 'monto-change)
	  (print (list start end len))
	  (print lang))))
