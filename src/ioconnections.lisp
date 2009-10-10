;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hi)

(defmethod invoke-with-event-loop ((backend (eql :iolib)) fun)
  (let ((iolib.multiplex::*default-multiplexer*
         ;; the epoll muxer gives me segfaults and memory corruption.
         ;; Don't know why, but select works, so let's use it:
         'iolib.multiplex::select-multiplexer))
    (iolib:with-event-base (*event-base*)
      (funcall fun))))

(defmethod dispatch-events-with-backend ((backend (eql :iolib)))
  (handler-case
      (iolib:event-dispatch *event-base* :one-shot t)
    (iolib.syscalls:etimedout (c)
      (warn "ignoring ~A in dispatch-events" c))))

(defmethod dispatch-events-no-hang-with-backend ((backend (eql :iolib)))
  (iolib:event-dispatch *event-base*
                        :one-shot t
                        :timeout 0
                        :min-step 0))

(defmethod invoke-later ((backend (eql :iolib)) fun)
  (iolib.multiplex:add-timer *event-base* fun 0 :one-shot t))


;;;;
;;;; IOLIB-CONNECTION
;;;;

(defclass iolib-connection (io-connection)
  ((fd :initarg :fd
       :initform nil
       :accessor connection-fd)
   (write-buffers :initform nil
                  :accessor connection-write-buffers)))

(defmethod initialize-instance :after
    ((instance iolib-connection) &key)
  )

(defmethod (setf connection-fd)
    :after
    ((newval t) (connection iolib-connection))
  (when (connection-fd instance)
    (set-iolib-handlers instance)))

(defun set-iolib-handlers (connection)
  (let ((fd (connection-fd connection)))
    (iolib:set-io-handler
     *event-base*
     fd
     :read
     (lambda (.fd event error)
       (declare (ignore event))
       (when (eq error :error) (error "error with ~A" .fd))
       (process-incoming-data connection)))))

(defmethod %read ((connection iolib-connection))
  (let* ((fd (connection-fd connection))
         (buffer (connection-input-buffer connection)))
    ;; fixme: with-pointer-to-vector-data isn't portable
    (subseq buffer
            0
            (cffi-sys:with-pointer-to-vector-data (ptr buffer)
              (iolib.syscalls:%sys-read fd ptr (length buffer))))))

(defmethod delete-connection :before ((connection iolib-connection))
  (let ((fd (connection-fd connection)))
    (when fd
      (iolib.syscalls:%sys-close fd))))

(defmethod connection-listen ((connection iolib-connection))
  (iolib.multiplex:fd-readablep (connection-fd connection)))

(defmethod connection-write (data (connection iolib-connection))
  (let ((bytes (filter-connection-output connection data))
        (fd (connection-fd connection))
        (need-handler (null (connection-write-buffers connection)))
        handler)
    (check-type bytes (simple-array (unsigned-byte 8) (*)))
    (setf (connection-write-buffers connection)
          (nconc (connection-write-buffers connection)
                 (list bytes)))
    (when need-handler
      (setf handler
            (iolib:set-io-handler
             *event-base*
             fd
             :write
             (lambda (.fd event error)
               (declare (ignore event))
               (when (eq error :error) (error "error with ~A" .fd))
               ;; fixme: with-pointer-to-vector-data isn't portable
               (let ((bytes (pop (connection-write-buffers connection))))
                 (check-type bytes (simple-array (unsigned-byte 8) (*)))
                 (cffi-sys:with-pointer-to-vector-data (ptr bytes)
                   (let ((n-bytes-written
                          (iolib.syscalls:%sys-write fd ptr (length bytes))))
                     (unless (eql n-bytes-written (length bytes))
                       (push (subseq bytes n-bytes-written)
                             (connection-write-buffers connection)))))
                 (setf (iolib.multiplex::fd-handler-one-shot-p handler)
                       (null (connection-write-buffers connection))))))))))


;;;;
;;;; PROCESS-CONNECTION/IOLIB
;;;;

(defclass process-connection/iolib
    (process-connection-mixin)
  ())


;;;;
;;;; TCP-CONNECTION/IOLIB
;;;;

(defclass tcp-connection/iolib (tcp-connection-mixin iolib-connection)
  ((socket :accessor connection-socket)))

(defmethod initialize-instance :after ((instance tcp-connection/iolib) &key)
  (with-slots (fd socket host port) instance
    (connection-note-event instance :initialized)
    (unless fd
      (setf socket
            (iolib.sockets:make-socket :address-family :internet
                                       :connect :active
                                       :type :stream
                                       :remote-host host
                                       :remote-port port))
      (setf fd (iolib.sockets:socket-os-fd socket)))
    (set-iolib-handlers instance)
    (note-connected instance)))

;;;
;;; PTY-CONNECTION/IOLIB
;;;

(defclass pty-connection/iolib (pty-connection-mixin iolib-connection)
  ())

(defmethod initialize-instance :after ((instance pty-connection/iolib) &key)
  (connection-note-event instance :initialized)
  (set-iolib-handlers instance))

(defmethod (setf connection-fd)
    :after
    (newval (connection pty-connection/iolib))
  ;; ...
  )

;;;;
;;;; LISTENING-CONNECTION/IOLIB
;;;;

(defclass listening-connection/iolib (listening-connection)
  ((socket :accessor connection-socket)
   (fd :initform nil
       :accessor connection-fd)))

(defmethod initialize-instance :after
    ((instance listening-connection/iolib) &key)
  (with-slots (fd socket host port) instance
    (unless fd
      (connection-note-event instance :initialized)
      (let ((addr (if host
                      (iolib.sockets:ensure-hostname host)
                      iolib.sockets:+ipv4-unspecified+)))
        (setf socket
              (flet ((doit (port)
                       (iolib.sockets:make-socket :address-family :internet
                                                  :connect :passive
                                                  :type :stream
                                                  :local-host addr
                                                  :local-port port)))
                (if port
                    (doit port)
                    (iter:iter (iter:for p from 1024 below 65536)
                               (handler-case
                                   (doit p)
                                 (:no-error (socket)
                                   (setf port p)
                                   (return socket))
                                 (error (c)
                                   (warn "trying next port"))))))))
      (setf fd (iolib.sockets:socket-os-fd socket)))
    (set-iolib-server-handlers instance)))

(defun set-iolib-server-handlers (instance)
  (iolib:set-io-handler
   *event-base*
   (connection-fd instance)
   :read
   (lambda (.fd event error)
     (declare (ignore event))
     (when (eq error :error) (error "error with ~A" .fd))
     (process-incoming-connection instance))))

(defmethod delete-connection :before ((connection listening-connection/iolib))
  (close (connection-socket connection)))

(defmethod (setf connection-fd)
    :after
    ((newval t) (connection listening-connection/iolib))
  (set-iolib-server-handlers connection))

(defun %tcp-connection-from-fd (name fd host port initargs)
  (apply #'make-instance
         'tcp-connection/iolib
         :name name
         :fd fd
         :host host
         :port port
         initargs))


;;;;
;;;; TCP-LISTENER/IOLIB
;;;;

(defclass tcp-listener/iolib (tcp-listener-mixin listening-connection/iolib)
  ())

(defmethod initialize-instance :after ((instance tcp-listener/iolib) &key)
  ;; ...
  )

(defmethod convert-pending-connection ((connection tcp-listener/iolib))
  (iolib.sockets::with-sockaddr-storage-and-socklen (ss size)
    (let* ((socket (connection-socket connection))
           (fd (iolib.sockets::%accept (iolib.streams:fd-of socket) ss size)))
      (multiple-value-bind (host port)
          (iolib.sockets::sockaddr-storage->sockaddr ss)
        (%tcp-connection-from-fd
         (format nil "Accepted for: ~A" (connection-name connection))
         fd
         host
         port
         (connection-initargs connection))))))

#+(or)
(trace connection-write
       %read
       iolib.syscalls:%sys-read
       iolib.syscalls:%sys-write)
