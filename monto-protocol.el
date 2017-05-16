(require 'cl-lib)
(require 'ffi)
(require 'json)

(defvar monto-libzmq "libzmq.so"
  "The ZeroMQ library's name.")
(defvar monto--context nil)
(defvar monto--debug nil)
(defvar monto--ids nil)
(defvar monto--recv-socket nil)
(defvar monto--send-socket nil)
(defvar monto--zmq-version nil)

;; TODO These may change from system to system...
;; They're defined as C macros, too, so that makes it even worse.
(defconst monto--DONTWAIT 1)
(defconst monto--EAGAIN 11)
(defconst monto--recv-bufsize 1048576)

(cl-defun monto-init (&optional (send-addr "tcp://127.0.0.1:5000") (recv-addr "tcp://127.0.0.1:5001"))
  "Initializes Monto."
  (unless monto--zmq-version
    (let ((major-buf (ffi-call nil "malloc" [:pointer :uint64] 4))
          (minor-buf (ffi-call nil "malloc" [:pointer :uint64] 4))
          (patch-buf (ffi-call nil "malloc" [:pointer :uint64] 4)))
      (ffi-call monto-libzmq "zmq_version" [:void :pointer :pointer :pointer] major-buf minor-buf patch-buf)
      (let ((major (ffi-deref major-buf :sint32 0))
            (minor (ffi-deref minor-buf :sint32 0))
            (patch (ffi-deref patch-buf :sint32 0)))
        (ffi-call nil "free" [:void :pointer] major-buf)
        (ffi-call nil "free" [:void :pointer] minor-buf)
        (ffi-call nil "free" [:void :pointer] patch-buf)
        (setq monto--zmq-version (list major minor patch))))
    (if (or (/= (first  monto--zmq-version) 4)
            (<  (second monto--zmq-version) 2))
      (throw 'zmq-version-error)))
  (unless monto--context
    (setq monto--context (ffi-call monto-libzmq "zmq_ctx_new" [:pointer])))
  (unless monto--recv-socket
    (setq monto--recv-socket (ffi-call monto-libzmq "zmq_socket" [:pointer :pointer :sint32] monto--context 0))
    (ffi-call monto-libzmq "zmq_connect" [:sint32 :pointer :pointer] monto--recv-socket recv-addr))
  (unless monto--send-socket
    (setq monto--send-socket (ffi-call monto-libzmq "zmq_socket" [:pointer :pointer :sint32] monto--context 0))
    (ffi-call monto-libzmq "zmq_connect" [:sint32 :pointer :pointer] monto--send-socket send-addr)))

(defun monto-exit ()
  "Deinitializes Monto. This is still a bit flaky; Monto interacts...
  non-perfectly with exits from clients. To be on the safe side, restart the
  broker."
  (when monto--recv-socket
    (ffi-call monto-libzmq "zmq_close" [:sint32 :pointer] monto--recv-socket)
    (setq monto--recv-socket nil))
  (when monto--send-socket
    (ffi-call monto-libzmq "zmq_close" [:sint32 :pointer] monto--send-socket)
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
  (monto--send '(
    (tag . "discovery")
    (contents . (
      (discover_services . []))))))

(defun monto-send-source-message (physical-name id language contents)
  "Sends a source message."
  (monto--send `(
    (tag . "source")
    (contents . (
      (source . (
        (physical-name . ,physical-name)))
      (id . ,id)
      (language . ,language)
      (contents . ,contents))))))

(defun monto-recv (&optional len)
  "Receives a message if one is queued. Returns a decoded object, not a string.
  If no message was queued, returns nil."
  (monto-ensure)
  (let* ((buf (ffi-call nil "malloc" [:pointer :uint64] (or len monto--recv-bufsize)))
         (return-code (ffi-call-errno monto-libzmq "zmq_recv" [:sint32 :pointer :pointer :uint64 :sint32] monto--recv-socket buf len monto--DONTWAIT))
         (status (car return-code))
         (errno (cdr return-code)))
    (if (> status 0)
      (let ((bytes (ffi-read-array buf :uint8 status)))
        (ffi-call nil "free" [:void :pointer] buf)
        (print (list status bytes (ffi-get-string str))))
        ; (json-read-from-string str))
      (progn
        (print (cons status errno))
        (ffi-call-errno nil "free" [:void :pointer] buf)
        (if (or (= 0 errno) (= monto--EAGAIN errno))
          nil
          (throw 'zmq-recv-error errno))))))

(defun monto--next-version (name)
  "Returns the next ID for this name."
  (let* ((pair (assoc name monto--ids))
         (id (if pair (1+ (cdr pair)) 1)))
    (setf (alist-get name monto--ids) id)
    id))

(defun monto--send (msg)
  "Sends a message. The message should be a JSON-able object, not a string.
  Returns t if the message was sent successfully, nil if not."
  (monto-ensure)
  (let* ((str (json-encode msg))
         (ret (ffi-call monto-libzmq "zmq_send" [:sint32 :pointer :pointer :uint64 :sint32] monto--send-socket str (length str) 0)))
    (print str)
    (>= ret 0)))

(defun monto--test ()
  (print (monto-discover))
  (print (list
    monto--context
    monto--send-socket
    monto--recv-socket))
  (let ((msg nil))
    (while (not msg)
      (setq msg (monto-recv)))
    msg))

(provide 'monto-protocol)
