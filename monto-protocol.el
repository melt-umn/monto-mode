(require 'cl-lib)
(require 'ffi)
(require 'json)

(defvar monto-libzmq "libzmq.so"
  "The ZeroMQ library's name.")
(defvar monto--context nil)
(defvar monto--debug nil)
(defvar monto--recv-socket nil)
(defvar monto--send-socket nil)

;; TODO These may change from system to system...
;; They're defined as macros, too, so that makes it even worse.
(defconst monto--DONTWAIT 1)
(defconst monto--EAGAIN 11)

(defmacro monto-debug-wrap (msg form)
  `(progn
	 (if monto--debug (print (concat "starting " ,msg)))
	 ,form
	 (if monto--debug (print (concat "done " ,msg)))))

(cl-defun monto-init (&optional (send-addr "tcp://127.0.0.1:5001") (recv-addr "tcp://127.0.0.1:5000"))
  "Initializes Monto."
  (unless monto--context
	(monto-debug-wrap "Making ZeroMQ Context"
      (setq monto--context (ffi-call monto-libzmq "zmq_ctx_new" [:pointer]))))
  (unless monto--recv-socket
	(setq monto--recv-socket (ffi-call monto-libzmq "zmq_socket" [:pointer :pointer :sint64] monto--context 0))
	(ffi-call monto-libzmq "zmq_connect" [:sint64 :pointer :pointer] monto--recv-socket recv-addr))
  (unless monto--send-socket
	(setq monto--send-socket (ffi-call monto-libzmq "zmq_socket" [:pointer :pointer :sint64] monto--context 0))
	(ffi-call monto-libzmq "zmq_connect" [:sint64 :pointer :pointer] monto--send-socket send-addr)))

(defun monto-exit ()
  "Deinitializes Monto. This is still a bit flaky; Monto interacts...
  non-perfectly with exits from clients. To be on the safe side, restart the
  broker."
  (when monto--recv-socket
	(ffi-call monto-libzmq "zmq_close" [:sint64 :pointer] monto--recv-socket)
	(setq monto--recv-socket nil))
  (when monto--send-socket
	(ffi-call monto-libzmq "zmq_close" [:sint64 :pointer] monto--send-socket)
	(setq monto--send-socket nil))
  (when monto--context
    (ffi-call monto-libzmq "zmq_ctx_destroy" [:void :pointer] monto--context)
	(setq monto--context nil)))

(defun monto-ensure ()
  "Ensures Monto is initialized."
  (unless monto--context
	(monto-init)))

(defun monto-discover ()
  "Sends a discovery request. As this is asynchronous, the response will
  eventually be returned from (monto-recv)."
  (monto-ensure)
  (monto--send '(
	(tag . discovery)
	(contents . (
	  (discover-services . [])
	)
  ))))

(defun monto-recv (&optional len)
  "Receives a message if one is queued. Returns a decoded object, not a string.
  If no message was queued, returns nil."
  (let* ((buf (ffi-call nil "malloc" [:pointer :uint64] (or len 1048576)))
         (return-code (ffi-call-errno monto-libzmq "zmq_recv" [:sint64 :pointer :pointer :uint64 :uint64] monto--recv-socket buf len monto--DONTWAIT))
	     (status (car return-code))
		 (errno (cdr return-code)))
	(if (>= status 0)
	  (let ((str (ffi-get-string buf)))
		(ffi-call nil "free" [:void :pointer] buf)
		(json-read-from-string str))
	  (progn
		(ffi-call-errno nil "free" [:void :pointer] buf)
		(if (= monto--EAGAIN errno)
		  nil
		  (throw 'zmq-recv-error errno))))))

(defun monto--send (msg)
  "Sends a message. The message should be a JSON-able object, not a string.
  Returns t if the message was sent successfully, nil if not."
  (let* ((str (json-encode msg))
		 (ret (ffi-call monto-libzmq "zmq_send" [:sint64 :pointer :pointer :uint64 :sint64] monto--send-socket str (length str) 0)))
	(>= ret 0)))

(provide 'monto-protocol)
