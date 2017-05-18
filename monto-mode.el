(require 'monto-handlers)
(require 'monto-macros)
(require 'monto-protocol)

;; Variable for whether monto-mode is enabled or not.
(defvar monto-mode nil
  "Mode for Monto syntax highlighting.")
(make-variable-buffer-local 'monto-mode)

;; Monto language map. Add a mapping from extension to language name to enable
;; Monto for the language. Entries should be two strings, e.g. ("sv" . "silver").
(defconst monto-language-alist nil
  "An alist of languages to use with Monto.")

(defconst monto-product-handlers (list
  (cons "errors" #'monto-errors--handler)
  (cons "highlighting" #'monto-highlighting--handler))
  "Handlers for various products.")

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

(defun monto-mode (&optional arg)
  "Monto minor mode."
  (interactive "P")
  (setq monto-mode
    (if (null arg)
        (not monto-mode)
        (> (prefix-numeric-value arg) 0)))
  (if monto-mode
	(progn
	  (font-lock-mode 0)
	  (font-lock-set-defaults)
      (add-hook 'after-change-functions 'monto-change nil t)
	  (monto-init)
      (setq monto-handlers
        (cons (cons "product" #'monto-on-product)
              monto-handlers)))
    (remove-hook 'after-change-functions 'monto-change t)))

(if (not (assq 'monto-mode minor-mode-alist))
  "Registers monto-mode as a minor mode."
  (setq minor-mode-alist
    (cons '(monto-mode " Monto")
          minor-mode-alist)))

(defun monto-change (start end len)
  "Fires on a change in a monto-mode'd buffer."
  (let ((lang (monto--language)))
	(when lang
      (monto-send-source-message (buffer-file-name) lang (buffer-string)))))

(defun monto-on-product (product)
  "The handler for Monto products. Delegates to the appropriate subhandler
   (see the monto-product-handlers assoc)."
  (let ((contents (monto--json-get product 'contents))
        (physical-name (monto--json-get product 'source 'physical_name))
        (product-type (monto--json-get product 'product))
        (version (monto--json-get product 'id)))
    (when (monto-most-recent-versionp physical-name version)
      (let ((handler (cdr (assoc product-type monto-product-handlers))))
        (if handler
          (funcall handler physical-name contents)
          (print (concat "No product handler for " product-type)))))))

(defun monto--json-get (obj &rest path)
  "A helper to get properties from a JSON object. Note that this is recursive,
   so don't pass more than a few dozen parameters for the path.
   
   Why does Emacs Lisp not have tail recursion? Who knows."
  (if (eq path nil)
    obj
    (apply #'monto--json-get (cdr (assoc (car path) obj)) (cdr path))))

(provide 'monto-mode)
