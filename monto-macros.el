(require 'cl-lib)

(defmacro def-monto-highlighting-styles (&rest styles)
  "Defines the styles used for highlighting with Monto. See the
   example-dot-emacs-file file for an example."
  (let ((ast nil))
    (dolist (asc styles)
      (let ((style (car asc))
            (attrs (cdr asc))
            (face (cl-gensym)))
        (let ((def `(defface ,face '((t ,@attrs)) ""))
              (add `(add-to-list
                      'monto-highlighting-styles
                      (cons ,(prin1-to-string style) ',face))))
          (setq ast (cons def
                    (cons add
                          ast))))))
    `(prog1 nil ,@ast)))

(defmacro def-monto-languages (&rest entries)
  "Defines languages to be used with Monto."
  (defun add-one (lang ext)
    (list
      `(add-to-list 'monto-language-alist '(,ext . ,lang))
      (let ((pattern (concat "\\." ext "\\'")))
        `(add-to-list 'auto-mode-alist '(,pattern . monto-mode)))))
  (defun helper (lang &rest exts)
    (apply #'append (mapcar (lambda (ext) (add-one lang ext)) exts)))
  (let ((exprs (apply #'append (mapcar (lambda (l) (apply #'helper l)) entries))))
    `(prog1 nil ,@exprs)))

(provide 'monto-macros) 
