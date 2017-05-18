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
  (add-hook 'after-change-functions 'monto-change nil t)
  (add-to-list 'monto-handlers (cons "product" #'monto-on-product))
  (monto-init)
  (monto-change 0 0 0))

(defun monto-change (start end len)
  "Fires on a change in a monto-mode'd buffer."
  (let ((lang (monto--language))
        (cur-time (cl-subseq (current-time) 0 3)))
    (when (and lang (monto--debounce cur-time))
      (monto-send-source-message (buffer-file-name) lang (buffer-string)))
      (setq monto--last-send cur-time)))

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
