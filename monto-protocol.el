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

(cl-defun monto-init ()
  "Initializes Monto."
  
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
  (let ((body (json-encode client-negotiation))
	    (handlers `(
		  (200 . ,#'ok-handler))))
    (request
	  (concat monto-broker-url "/monto/version")
	  :type        "POST"
	  :data        body
	  :error       #'error-handler
	  :parser      #'json-read
	  :status-code handlers
	  :sync        t))
    monto-init-cbn)

(defun monto-update-sources (path contents &optional language cb)
  "Sends a product to the broker know about an updated version of a source
  file."
  (cl-defun error-handler (&key error-thrown &allow-other-keys)
    (throw 'monto-error (cons 'update-source error-thrown)))
  (request
	(concat monto-broker-url "/monto/broker/source")
	:params      `((path . ,path))
	:type        "PUT"
	:data        contents
	:error       #'error-handler
	:status-code (if cb `((200 . ,cb)) nil)))

(defun monto-request-product-from (service-id product-type path language ok-cb &optional err-cb)
  "Requests a product from the broker, calling OK-CB with the product on
  success or ERR-CB on failure."
  TODO)

(provide 'monto-protocol)
