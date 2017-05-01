(require 'ffi)
(require 'json)

(defvar monto--context nil)
(defvar monto--socket nil)

;; TODO These may change from system to system...
(defconst monto--DONTWAIT 1)
(defconst monto--EAGAIN 11)

(defun monto-init ()
  "Initializes Monto."
  (if (not monto--context)
    (setq monto--context (ffi-call "libzmq.so" "zmq_ctx_new" [:pointer])))
  (if (not monto--socket)
	todo open socket))
(defun monto-exit ()
  "Deinitializes Monto."
  (when monto--socket
	todo close socket
	(setq monto--socket nil))
  (when monto--context
    (ffi-call "libzmq.so" "zmq_ctx_destroy" [:void :pointer] monto--context)
	(setq monto--context nil)))

(defun monto--recv (&optional len)
  "Receives a message if one is queued. Returns a decoded object, not a string.
  If no message was queued, returns nil."
  (let* ((buf (ffi-call nil "malloc" [:pointer :uint64] (or len 1048576)))
         (ret (ffi-call "libzmq.so" "zmq_recv" [:int :pointer :pointer :uint64 :uint64] monto--socket buf len monto--DONTWAIT)))
	(if (>= ret 0)
	  (let ((str (ffi-get-string buf)))
		(ffi-call nil "free" [:void :pointer] buf)
		(json-read-from-string str))
	  (progn
		(ffi-call nil "free" [:void :pointer] buf)
		nil)))) ; TODO Get errno, compare with monto--EAGAIN.

(defun monto--send (msg)
  "Sends a message. The message should be a JSON-able object, not a string.
  Returns t if the message was sent successfully, nil if not."
  (let* ((str (json-encode msg))
		 (ret (ffi-call "libzmq.so" "zmq_send" [:int :pointer :pointer :uint64 :int] monto--socket str (length str) 0)))
	(>= ret 0)))

(provide 'monto-protocol)
