(require 'cl-lib)
(require 'ffi)
(require 'json)

;;;;;;;;;;;;;;;;;;;;
;;; User Options ;;;
;;;;;;;;;;;;;;;;;;;;

(defvar monto-libzmq "libzmq.so"
  "The ZeroMQ library's name.")
(defconst monto-recv-bufsize 1048576
  "The number of bytes to allocate to the receive buffer.")

;;;;;;;;;;;;;;;;;;;;;;;
;;; Library Globals ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar monto--context nil
  "The FFI address of the ZeroMQ context.")
(defvar monto-handlers (list
  (cons "discovery" #'print))
  "The handler functions for each type of response.")
(defvar monto--ids nil
  "An association between file paths and their last known IDs.")
(defvar monto--recv-socket nil
  "The FFI address of the ZeroMQ socket used for receiving data from the
   broker.")
(defvar monto--send-socket nil
  "The FFI address of the ZeroMQ socket used for sending data to the broker.")
(defvar monto--timer nil
  "The timer object for the recv timer callback.")
(defvar monto--zmq-version nil
  "The (detected) version of ZeroMQ we're linking against.")

;;;;;;;;;;;;;;;;;;;;;
;;; FFI Constants ;;;
;;;;;;;;;;;;;;;;;;;;;

;; TODO These may change from system to system...
;; They're defined as C macros, too, so that makes it even worse.
(defconst monto--DONTWAIT 1
  "Tells ZeroMQ not to block while reading.")
(defconst monto--EAGAIN 11
  "The errno returned to indicate that DONTWAIT was passed and there was no
   message queued.")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "Public" Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun monto-init (&optional (send-addr "tcp://127.0.0.1:5000") (recv-addr "tcp://127.0.0.1:5001"))
  "Initializes Monto."
  ; Get the version number.
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
        (setq monto--zmq-version (list major minor patch)))))
  ; Check the version number.
  (if (or (/= (first  monto--zmq-version) 4)
          (<  (second monto--zmq-version) 1))
    (throw 'zmq-version-error monto--zmq-version))
  ; Create a context.
  (unless monto--context
    (setq monto--context (ffi-call monto-libzmq "zmq_ctx_new" [:pointer])))
  ; Create the receiving socket and connect to the broker.
  (unless monto--recv-socket
    (setq monto--recv-socket (ffi-call monto-libzmq "zmq_socket" [:pointer :pointer :sint32] monto--context 0))
    (ffi-call monto-libzmq "zmq_connect" [:sint32 :pointer :pointer] monto--recv-socket recv-addr))
  ; Create the sending socket and connect to the broker.
  (unless monto--send-socket
    (setq monto--send-socket (ffi-call monto-libzmq "zmq_socket" [:pointer :pointer :sint32] monto--context 0))
    (ffi-call monto-libzmq "zmq_connect" [:sint32 :pointer :pointer] monto--send-socket send-addr)) 
  ; Create and start the timer.
  (unless monto--timer
    (setq monto--timer (run-at-time 0.01 0.01 #'monto--timer)))
  ; Return nil instead of the last-completed operation's result.
  nil)

(defun monto-exit ()
  "Deinitializes Monto. This is still a bit flaky; Monto interacts...
  non-perfectly with exits from clients. To be on the safe side, restart the
  broker."
  ; Stop the timer.
  (when monto--timer
    (cancel-timer monto--timer)
    (setq monto--timer nil))
  ; Close and destroy the receiving socket.
  (when monto--recv-socket
    (ffi-call monto-libzmq "zmq_close" [:sint32 :pointer] monto--recv-socket)
    (setq monto--recv-socket nil))
  ; Close and destroy the sending socket.
  (when monto--send-socket
    (ffi-call monto-libzmq "zmq_close" [:sint32 :pointer] monto--send-socket)
    (setq monto--send-socket nil))
  ; Destroy the context.
  (when monto--context
    (ffi-call monto-libzmq "zmq_ctx_destroy" [:void :pointer] monto--context)
    (setq monto--context nil)))

(defun monto-discover ()
  "Sends a discovery request. As this is asynchronous, the response will
  eventually be returned from (monto--recv)."
  (monto--send '(
    (tag . "discovery")
    (contents . (
      (discover_services . []))))))

(defun monto-send-source-message (physical-name id language contents)
  "Sends a source message. As this is asynchronous, the response will
  eventually be returned from (monto--recv)."
  (monto--send `(
    (tag . "source")
    (contents . (
      (source . (
        (physical-name . ,physical-name)))
      (id . ,id)
      (language . ,language)
      (contents . ,contents))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "Private" Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun monto--ensure ()
  "Ensures Monto is initialized."
  (unless monto--context
    (monto-init)))

(defun monto--next-version (name)
  "Returns the next ID for this name."
  (let* ((pair (assoc name monto--ids))
         (id (if pair (1+ (cdr pair)) 1)))
    (setf (alist-get name monto--ids) id)
    id))

(defun monto--recv ()
  "Receives a message if one is queued. Returns a decoded object, not a string.
  If no message was queued, returns nil."
  (monto--ensure)
  (let* ((buf (ffi-call nil "malloc" [:pointer :uint64] monto-recv-bufsize))
         (return-code (ffi-call-errno monto-libzmq "zmq_recv" [:sint32 :pointer :pointer :uint64 :sint32] monto--recv-socket buf monto-recv-bufsize monto--DONTWAIT))
         (status (car return-code))
         (errno (cdr return-code)))
    (if (> status 0)
      ; The status is the length of the received message if it's positive.
      (let* ((bytes (ffi-read-array buf :uint8 status))
			 (str (string-as-multibyte (apply #'unibyte-string bytes))))
        (ffi-call nil "free" [:void :pointer] buf)
        (json-read-from-string str))
      (progn
        (ffi-call-errno nil "free" [:void :pointer] buf)
        (if (or (= 0 errno) (= monto--EAGAIN errno))
          nil
          (throw 'zmq-recv-error errno))))))

(defun monto--send (msg)
  "Sends a message. The message should be a JSON-able object, not a string.
  Returns t if the message was sent successfully, nil if not."
  (monto--ensure)
  (let* ((str (json-encode msg))
         (ret (ffi-call monto-libzmq "zmq_send" [:sint32 :pointer :pointer :uint64 :sint32] monto--send-socket str (length str) 0)))
    (when (< ret 0)
	  (throw 'zmq-send-error ret))))

(defun monto--timer ()
  "The timer function that checks for a response."
  (let ((recv (monto--recv)))
    (when recv
      (let* ((tag (cdr (assoc 'tag recv)))
             (contents (cdr (assoc 'contents recv)))
             (fn (cdr (assoc tag monto--handlers))))
        (funcall fn contents)))))

(provide 'monto-protocol)
