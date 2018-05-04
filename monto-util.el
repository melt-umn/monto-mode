;;; -*- lexical-binding: t -*-

(require 'cl-lib)

;; The arrow macro of Clojure.
(defmacro ->> (x &rest fs)
  (defun helper (fs)
    (pcase fs
      (`()                 x)
      (`(,hd . ,tl) (pcase hd
                      (`(,f . ,a) `(,f ,@a ,(helper tl)))
                      (_          `(,hd ,(helper tl)))))))
  (helper (reverse fs)))

;; An accessor chaining function.
(defun >-> (ks x)
  (if (null ks)
    x
    (>-> (cdr ks) (alist-get (car ks) x))))

;; Standard list flatmap.
(defun flatmap (f l)
  (apply #'append (mapcar f l)))

(provide 'monto-util)
