;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'monto-protocol-wire)
(require 'monto-util)

;;;;;;;;;;;;;;;;;;;;
;;; Private Data ;;;
;;;;;;;;;;;;;;;;;;;;

(defvar monto--services nil
  "The services known by the broker.")
(defvar monto--products nil
  "An association from (PRODUCT-TYPE . LANGUAGE) to a list of services.")
(defvar monto--inited nil
  "Whether Monto has been initialized or not.")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "Public" Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun monto-init ()
  (defun convert-product (prd)
    (let ((name (alist-get 'name     prd))
          (lang (alist-get 'language prd)))
      (cons name lang)))
  (defun convert-service (svc)
    (let ((name     (>-> '(service id) svc))
          (products (>-> '(products)   svc)))
      (cons name (mapcar #'convert-product products))))
  (defun products<-services (svcs)
    (let ((pspairs (flatmap (lambda (x)
            (mapcar (lambda (p) (cons (car x) p)) (cdr x))) svcs))
          (prds nil))
      (dolist (pair pspairs prds)
        (->> prds
          (alist-get (cdr pair))
          (cons (car pair))
          (setf (alist-get (cdr pair) prds))))))

  (->>
    (monto-negotiation)
    (alist-get 'services)
    (mapcar #'convert-service)
    (setq monto--services)
    products<-services
    (setq monto--products))
  (setq monto--inited t))

(defun monto-must-init ()
  (unless monto--inited
    (monto-init)))

(defun monto-get-product (path product-type language ok-cb &optional err-cb)
  (monto-must-init)
  (let ((service (cadr (assoc (cons product-type language) monto--products))))
    (if service
      (monto-request-product-from service product-type path language ok-cb
                                  err-cb)
      (funcall (or err-cb #'message) (concat "No service for product-type "
                                             product-type " and language "
                                             language))))
  nil)

(provide 'monto-protocol)
