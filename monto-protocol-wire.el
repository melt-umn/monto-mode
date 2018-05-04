;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'json)
(require 'request)

;;;;;;;;;;;;;;;;;;;;
;;; User Options ;;;
;;;;;;;;;;;;;;;;;;;;

(defvar monto-broker-url "http://localhost:28888"
  "The URL of the broker. This should not end with a slash.")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "Public" Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun monto-negotiation ()
  "Performs the Monto negotiation."
  
  ; Send a negotiation.
  (cl-defun error-handler (&key error-thrown &allow-other-keys)
    (throw 'monto-error (cons 'negotiation error-thrown)))
  (setq client-negotiation '(
    (monto
      (major . 3)
      (minor . 0)
      (patch . 0))
    (client
      (id . "edu.umn.cs.melt.monto3.emacs")
      (name . "monto3-mode")
      (vendor . "MELT")
      (major . 0)
      (minor . 1)
      (patch . 0))))
  (setq monto-init-cbn nil)
  (cl-defun ok-handler (&key data &allow-other-keys)
    ; TODO: Actually check compatibility... Right now this relies on the broker
    ; to do so, because I hate elisp.
    (setq monto-init-cbn data))
  (request
    (concat monto-broker-url "/monto/version")
    :headers '(("Content-Type" . "text/plain"))
    :data    (json-encode client-negotiation)
    :error   #'error-handler
    :parser  #'json-read
    :success #'ok-handler
    :sync    t
    :type    "POST")
  monto-init-cbn)

(defun monto-update-source (path contents &optional language cb)
  "Sends a product to the broker know about an updated version of a source
  file."

  (cl-defun ok-handler (&key data &allow-other-keys)
    (cb data))
  (cl-defun error-handler (&key error-thrown &allow-other-keys)
    (throw 'monto-error (cons 'update-source error-thrown)))
  (request
    (concat monto-broker-url "/monto/broker/source")
    :headers '(("Content-Type" . "text/plain"))
    :data    contents
    :error   #'error-handler
    :params  `((path . ,path))
    :success (if cb #'ok-handler (lambda (&rest _) nil))
    :type    "PUT"))

(defun monto-request-product-from (service-id product-type path language ok-cb &optional err-cb)
  "Requests a product from the broker, calling OK-CB with the product on
  success or ERR-CB on failure."

  (cl-defun ok-handler (&key data &allow-other-keys)
    (funcall ok-cb data))
  (cl-defun error-handler (&key error-thrown &allow-other-keys)
    (throw 'monto-error (cons 'request-product-from error-thrown)))
  (request
    (concat monto-broker-url "/monto/" service-id "/" product-type)
    :error   (or err-cb #'error-handler)
    :params  `((path . ,path) (language . ,language))
    :parser  #'json-read
    :success #'ok-handler
    :type    "GET"))

(provide 'monto-protocol-wire)
