;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hi)
#+sbcl (declaim (optimize (speed 2)))

(defmethod invoke-with-event-loop ((backend (eql :iolib)) fun)
  ;; the epoll muxer gives me segfaults and memory corruption.
  ;; Don't know why, but select works, so let's use it:
  (iolib:with-event-base
      (*event-base* :mux 'iolib.multiplex:select-multiplexer)
    (funcall fun)))

(defmethod dispatch-events-with-backend ((backend (eql :iolib)))
  (handler-case
      (iolib:event-dispatch *event-base* :one-shot t :min-step 0)
    ((or iolib.syscalls:etimedout iolib.syscalls:eagain) ())))

(defmethod dispatch-events-no-hang-with-backend ((backend (eql :iolib)))
  (handler-case
      (iolib:event-dispatch *event-base*
                            :one-shot t
                            :timeout 0
                            :min-step 0)
    ((or iolib.syscalls:etimedout iolib.syscalls:eagain) ())))

(defmethod invoke-later ((backend (eql :iolib)) fun)
  (iolib.multiplex:add-timer *event-base* fun 0 :one-shot t))


;;;;
;;;; IOLIB-CONNECTION
;;;;

(defclass iolib-connection (io-connection)
  ((read-fd :initarg :read-fd
            :initarg :fd
            :initform nil
            :accessor connection-read-fd)
   (write-fd :initarg :write-fd
             :initarg :fd
             :initform nil
             :accessor connection-write-fd)
   (write-buffers :initform nil
                  :accessor connection-write-buffers)))

(defmethod initialize-instance :after
    ((instance iolib-connection) &key)
  )

(defmethod (setf connection-read-fd)
    :after
    ((newval t) (connection iolib-connection))
  (when (connection-read-fd connection)
    (set-iolib-handlers connection)))

(defmethod (setf connection-write-fd)
    :after
    ((newval t) (connection iolib-connection))
  (when (connection-write-fd connection)
    (set-iolib-handlers connection)))

(defun set-iolib-handlers (connection)
  (let ((fd (connection-read-fd connection)))
    (iolib:set-io-handler
     *event-base*
     fd
     :read
     (lambda (.fd event error)
       (declare (ignore event))
       (when (or (eq error :error)
                 (eq (process-incoming-data connection) :eof))
         (iolib:remove-fd-handlers *event-base* fd :read t))))))

(defmethod %read ((connection iolib-connection))
  (let* ((fd (connection-read-fd connection))
         (buffer (connection-input-buffer connection))
         (n
          ;; fixme: with-pointer-to-vector-data isn't portable
          (cffi-sys:with-pointer-to-vector-data (ptr buffer)
            (iolib.syscalls:%sys-read fd ptr (length buffer)))))
    (cond
      ((zerop n)
       :eof)
      (t
       (subseq buffer 0 n)))))

(defmethod delete-connection :before ((connection iolib-connection))
  (with-slots (read-fd write-fd) connection
    (when read-fd (iolib.syscalls:%sys-close read-fd))
    (when write-fd (iolib.syscalls:%sys-close write-fd))))

(defmethod connection-listen ((connection iolib-connection))
  (iolib.multiplex:fd-readablep (connection-read-fd connection)))

(defmethod connection-write (data (connection iolib-connection))
  (let ((bytes (filter-connection-output connection data))
        (fd (connection-write-fd connection))
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
    (process-connection-mixin iolib-connection)
  ((pid :initform nil
        :accessor connection-pid)))

(defmethod initialize-instance
    :after
    ((instance process-connection/iolib) &key)
  (with-slots (read-fd write-fd pid command slave-pty-name directory) instance
    (connection-note-event instance :initialized)
    (when (stringp command)
      (setf command (cl-ppcre:split " " command)))
    (assert (every #'stringp command))
    (assert command)
    (setf (values pid read-fd write-fd)
          (%fork-and-exec (car command) command directory slave-pty-name))
    (set-iolib-handlers instance)
    (note-connected instance)))

(defmethod delete-connection :before ((connection process-connection/iolib))
  (isys:%sys-kill (connection-pid connection) 15))

;; ccl gives an exception in foreign code without this:
#+ccl
(defun invoke-without-interrupts (fun)
  (ccl::without-interrupts (funcall fun)))

#-ccl
(defun invoke-without-interrupts (fun)
  (funcall fun))

(defmacro maybe-without-interrupts (&body body)
  `(invoke-without-interrupts (lambda () ,@body)))

(defun %exec
       (stdin-read stdin-write stdout-read stdout-write file args directory
                   slave-pty-name)
  (maybe-without-interrupts
   (iolib.syscalls:%sys-close stdin-write)
   (iolib.syscalls:%sys-close stdout-read)
   (iolib.syscalls:%sys-dup2 stdin-read 0)
   (iolib.syscalls:%sys-dup2 stdout-write 1)
   (iolib.syscalls:%sys-dup2 stdout-write 2)
   (iolib.syscalls:%sys-close stdin-read)
   (iolib.syscalls:%sys-close stdout-write)
   (when slave-pty-name
     (iolib.syscalls:%sys-setsid)
     (handler-case
         (iolib.syscalls:%sys-open "/dev/tty" iolib.syscalls:o-rdwr)
       (iolib.syscalls:enoent ())
       (iolib.syscalls:enxio ())
       (:no-error (fd)
         (iolib.syscalls:%sys-ioctl fd osicat-posix:tiocnotty 0)
         (iolib.syscalls:%sys-close fd)))
     (iolib.syscalls:%sys-close 0)
     (iolib.syscalls:%sys-open slave-pty-name iolib.syscalls:o-rdwr)
     (iolib.syscalls:%sys-dup2 0 1)
     (iolib.syscalls:%sys-dup2 0 2)
     (cffi:with-foreign-object (tios 'osicat-posix::termios)
       (osicat-posix::tcgetattr 0 tios)
       (cffi:with-foreign-slots ((osicat-posix::iflag
                                  osicat-posix::oflag
                                  osicat-posix::lflag
                                  osicat-posix::cc)
                                 tios osicat-posix::termios)
         (setf osicat-posix::lflag
               (logandc2 osicat-posix::lflag
                         (logior osicat-posix::tty-echo
                                 osicat-posix::tty-echonl)))
         (setf osicat-posix::iflag
               (logior (logandc2 osicat-posix::iflag
                                 osicat-posix::tty-brkint)
                       osicat-posix::tty-icanon
                       osicat-posix::tty-icrnl))
         (setf osicat-posix::oflag
               (logandc2 osicat-posix::oflag
                         osicat-posix::tty-onlcr ))
         (setf (cffi:mem-ref osicat-posix::cc
                             :uint8
                             osicat-posix::cflag-verase)
               #o177)
         (osicat-posix::tcsetattr 0 osicat-posix::tcsaflush tios))))
   (when directory
     (iolib.syscalls:%sys-chdir directory))
   (let ((n (length args)))
     (cffi:with-foreign-object (argv :pointer (1+ n))
       (iter:iter (iter:for i from 0)
                  (iter:for arg in args)
                  (setf (cffi:mem-aref argv :pointer i)
                        (cffi:foreign-string-alloc arg)))
       (setf (cffi:mem-aref argv :pointer n) (cffi:null-pointer))
       (iolib.syscalls:%sys-execvp file argv)))
   (iolib.syscalls::%sys-exit 1)))

(defun %fork-and-exec (file args &optional directory slave-pty-name)
  (multiple-value-bind (stdin-read stdin-write)
      (iolib.syscalls:%sys-pipe)
    (multiple-value-bind (stdout-read stdout-write)
        (iolib.syscalls:%sys-pipe)
      (let ((pid (iolib.syscalls:%sys-fork)))
        (case pid
          (0 (%exec stdin-read
                    stdin-write
                    stdout-read
                    stdout-write
                    file
                    args
                    directory
                    slave-pty-name))
          (t
           (iolib.syscalls:%sys-close stdin-read)
           (iolib.syscalls:%sys-close stdout-write)
           (values pid stdout-read stdin-write)))))))


;;;;
;;;; TCP-CONNECTION/IOLIB
;;;;

(defclass tcp-connection/iolib (tcp-connection-mixin iolib-connection)
  ((socket :accessor connection-socket)))

(defmethod initialize-instance :after ((instance tcp-connection/iolib) &key)
  (with-slots (read-fd write-fd socket host port) instance
    (connection-note-event instance :initialized)
    (unless (or read-fd write-fd)
      (setf socket
            (iolib.sockets:make-socket :address-family :ipv4
                                       :connect :active
                                       :type :stream
                                       :remote-host host
                                       :remote-port port))
      (setf read-fd (iolib.sockets:socket-os-fd socket))
      (setf write-fd (iolib.sockets:socket-os-fd socket)))
    (set-iolib-handlers instance)
    (note-connected instance)))

;;;
;;; PIPELIKE-CONNECTION/IOLIB
;;;

(defclass pipelike-connection/iolib
    (pipelike-connection-mixin iolib-connection)
  ())

(defmethod initialize-instance
    :after
    ((instance pipelike-connection/iolib) &key)
  (connection-note-event instance :initialized)
  (set-iolib-handlers instance))


;;;
;;; PROCESS-WITH-PTY-CONNECTION/IOLIB
;;;

(defclass process-with-pty-connection/iolib
    (process-with-pty-connection-mixin pipelike-connection/iolib)
  ())


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
                       (iolib.sockets:make-socket :address-family :ipv4
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
                                 (error (c) (warn "~A" c))))))))
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
