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
    (message "Monto Error: negotiation: %s" error-thrown))
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

(defun monto-update-source (path contents &optional language ok-cb err-cb)
  "Sends a product to the broker know about an updated version of a source
  file."

  (cl-defun error-handler (&key error-thrown &allow-other-keys)
    (message "Monto Error: update-source: %s" error-thrown)
	(if err-cb (funcall err-cb)))
  
  (let ((params `((path . ,path))))
	(if language
	  (setq params (append params `((language . ,language)))))
    (request
      (concat monto-broker-url "/monto/broker/source")
      :headers '(("Content-Type" . "text/plain"))
      :data    contents
      :error   #'error-handler
      :params  params
      :success (lambda (&rest _) (if ok-cb (funcall ok-cb)))
      :type    "PUT")))

(defun monto-request-product-from (service-id product-type path language ok-cb &optional err-cb)
  "Requests a product from the broker, calling OK-CB with the product on
  success or ERR-CB on failure."

  (cl-defun ok-handler (&key data &allow-other-keys)
    (funcall ok-cb data))
  (cl-defun error-handler (&key error-thrown &allow-other-keys)
    (message "Monto Error: request-product-from: %s" error-thrown))

  (request
    (concat monto-broker-url "/monto/" service-id "/" product-type)
    :error   (or err-cb #'error-handler)
    :params  `((path . ,path) (language . ,language))
    :parser  #'json-read
    :success #'ok-handler
    :type    "GET"))

(provide 'monto-protocol-wire)
