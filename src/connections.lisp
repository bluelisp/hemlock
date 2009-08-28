;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :qt-hemlock)
(named-readtables:in-readtable :qt-hemlock)

(defvar *all-connections* nil)

(defun list-all-connections ()
  (copy-seq *all-connections*))


;;;;
;;;; CONNECTION
;;;;

(defparameter +input-buffer-size+ #x2000)

(defclass connection ()
  ((name :initarg :name
         :accessor connection-name)
   (buffer :initarg :buffer
           :initform nil
           :accessor connection-buffer)
   (connection-sentinel :initarg :sentinel
                        :initform nil
                        :accessor connection-sentinel)))

(defmethod print-object ((instance connection) stream)
  (print-unreadable-object (instance stream :identity nil :type t)
    (format stream "~A" (connection-name instance))))

(defmethod initialize-instance :after
    ((instance connection) &key buffer)
  (flet ((delete-hook (buffer)
           (when (eq buffer (connection-buffer instance))
             (setf (connection-buffer instance) nil))))
    (typecase buffer
      ((eql t)
       (setf (connection-buffer instance)
             (make-buffer-with-unique-name
              ;; note the space in the buffer name
              (format nil " *Connection ~A*" (connection-name instance))
              :delete-hook (list #'delete-hook))))
      (hi::buffer
       (push #'delete-hook (buffer-delete-hook buffer)))
      (null)
      (t
       (error "expected NIL, T, or a buffer, but found ~A" buffer))))
  (setf (connection-name instance)
        (unique-connection-name (connection-name instance)))
  (push instance *all-connections*))

(defun unique-connection-name (base)
  (let ((name base))
    (iter:iter (iter:for i from 1)
               (iter:while (find name
                                 *all-connections*
                                 :test #'equal
                                 :key #'connection-name))
               (setf name (format nil "~A<~D>" base i)))
    name))

(defun delete-connection-buffer (connection)
  (when (connection-buffer connection)
    (delete-buffer (connection-buffer connection))
    (setf (connection-buffer connection) nil)))

(defmethod delete-connection ((connection connection))
  (delete-connection-buffer connection)
  (setf *all-connections* (remove connection *all-connections*)))

(defun connection-note-event (connection event)
  (let ((sentinel (connection-sentinel connection)))
    (when sentinel
      (funcall sentinel connection event))))


;;;;
;;;; IO-CONNECTION
;;;;

(defparameter +input-buffer-size+ #x2000)

(defclass io-connection (connection)
  ((connection-filter :initarg :filter
                      :initform nil
                      :accessor connection-filter)
   (io-device :initarg :io-device
              :initform nil
              :accessor connection-io-device)
   (input-buffer :initform (make-array +input-buffer-size+
                                       :element-type '(unsigned-byte 8))
                 :accessor connection-input-buffer)
   (encoding :initform :utf-8
             :initarg :encoding
             :accessor connection-encoding)))

(defmethod initialize-instance :after
    ((instance io-connection) &key)
  (let ((enc (connection-encoding instance)))
    (when (symbolp enc)
      (setf (connection-encoding instance)
            (babel-encodings:get-character-encoding enc))))
  (when (connection-io-device instance)
    (connect-io-device-signals instance)))

(defmethod delete-connection :before ((connection io-connection))
  (when (connection-io-device connection)
    (#_close (connection-io-device connection))))

(defun filter-connection-output (connection data)
  (etypecase data
    (string
     (babel:string-to-octets data :encoding (connection-encoding connection)))
    ((array (unsigned-byte 8) (*))
     data)))

(defun connection-listen (connection)
  (check-type connection io-connection)
  (plusp (#_bytesAvailable (connection-io-device connection))))

(defun connection-write (data connection)
  (check-type connection io-connection)
  (let ((bytes (filter-connection-output connection data)))
    ;; fixme: with-pointer-to-vector-data isn't portable
    (cffi-sys:with-pointer-to-vector-data (ptr bytes)
      (let ((n-bytes-written
             (#_write (connection-io-device connection)
                      (qt::char* ptr)
                      (length bytes))))
        (when (minusp n-bytes-written)
          (error "error on socket: ~A" connection))
        (unless (eql n-bytes-written (length bytes))
          ;; fixme: buffering
          (error "oops, not implemented"))))))

(defun %read (connection)
  (let* ((io (connection-io-device connection))
         (n-bytes-available (#_bytesAvailable io))
         (buffer (connection-input-buffer connection)))
    (when (< (length buffer) n-bytes-available)
      (setf buffer (make-array n-bytes-available
                               :element-type '(unsigned-byte 8))))
    ;; fixme: with-pointer-to-vector-data isn't portable
    (subseq buffer
            0
            (cffi-sys:with-pointer-to-vector-data (ptr buffer)
              (let ((n-bytes-read
                     (#_read io (qt::char* ptr) n-bytes-available)))
                (when (minusp n-bytes-read)
                  (error "error on socket: ~A" connection))
                (assert (>= n-bytes-read n-bytes-available))
                n-bytes-read)))))

(defun note-connected (connection)
  (connection-note-event connection :connected))

(defun note-disconnected (connection)
  (print :note-disconnected)
  (force-output)
  (connection-note-event connection :disconnected)
  (let ((buffer (connection-buffer connection)))
    (when buffer
      (with-writable-buffer (buffer)
        (insert-string (buffer-point buffer)
                       (format nil "~&* Connection ~S disconnected." connection))))))

(defun note-error (connection)
  (print :note-error)
  (force-output)
  (connection-note-event connection :error)
  (let ((buffer (connection-buffer connection)))
    (when buffer
      (with-writable-buffer (buffer)
        (insert-string (buffer-point buffer)
                       (format nil "~&* Error on connection ~S." connection))))))

(defun filter-incoming-data (connection bytes)
  (funcall (or (connection-filter connection) #'default-filter)
           connection
           bytes))

(defun process-incoming-data (connection)
  (let* ((bytes (%read connection))
         (characters (filter-incoming-data connection bytes))
         (buffer (connection-buffer connection)))
    (when (and characters buffer)
      (with-writable-buffer (buffer)
        (insert-string (buffer-point buffer) characters)))))

(defun default-filter (connection bytes)
  ;; fixme: what about multibyte characters that got split between two
  ;; input events data?
  (babel:octets-to-string bytes :encoding (connection-encoding connection)))

(defun connect-io-device-signals (connection)
  (let ((device (connection-io-device connection)))
    ;; ...
    (connect device
             (QSIGNAL "readyRead()")
             (lambda ()
               (process-incoming-data connection)
               (redraw-needed)))))

(defmethod (setf connection-io-device)
    :after
    ((newval t) (connection io-connection))
  (connect-io-device-signals connection))


;;;;
;;;; PROCESS-CONNECTION
;;;;

#-(or windows mswindows)
(cffi:defcfun (foreign-openpty "openpty") :int
  (amaster :pointer)
  (aslave :pointer)
  (name :pointer)
  (termp :pointer)
  (winsize :pointer))

(defun openpty ()
  (cffi:with-foreign-objects ((amaster :int)
                              (aslave :int)
                              (name :char 256))
    (assert (zerop (foreign-openpty amaster
                                    aslave
                                    name
                                    (cffi-sys:null-pointer)
                                    (cffi-sys:null-pointer))))
    (values (cffi:foreign-string-to-lisp name)
            (cffi:mem-ref amaster :int)
            (cffi:mem-ref aslave :int))))

(defclass process-connection (io-connection)
  ((command :initarg :command
            :accessor connection-command)
   (exit-code :initform nil
              :initarg :exit-code
              :accessor connection-exit-code)
   (exit-status :initform nil
                :initarg :exit-status
                :accessor connection-exit-status)))

#+(or)
(defmethod initialize-instance :after ((instance process-connection) &key)
  (let ((process (#_new QProcess)))
    (multiple-value-bind (pty amaster aslave)
        (openpty)
;;;       (#_setStandardInputFile process pty)
;;;       (#_setStandardOutputFile process pty)
;;;       (#_setStandardErrorFile process pty)
      (setf (connection-io-device instance) process)
      (make-descriptor-connection aslave :buffer t)
      (#_start process
               (format nil
                       "~A ~A ~A"
                       "/home/david/clbuild/source/hemlock/c/setpty"
                       pty
                       (connection-command instance))))))

(defmethod initialize-instance :after ((instance process-connection) &key)
  (let ((process (#_new QProcess)))
    (setf (connection-io-device instance) process)
    (connection-note-event instance :initialized)
    (#_start process (connection-command instance))))

(defmethod (setf connection-io-device)
    :after
    (newval (connection process-connection))
  (connect newval
           (QSIGNAL "finished(int,QProcess::ExitStatus)")
           (lambda (&rest *)
             (note-finished connection)
             (redraw-needed))))

(defun note-finished (connection)
  (let* ((process (connection-io-device connection))
         (code (#_exitCode process))
         (status (#_exitStatus process)))
    (setf (connection-exit-code connection) code)
    (setf (connection-exit-status connection) status)
    (connection-note-event connection :finished)
    (let ((buffer (connection-buffer connection)))
      (when buffer
        (with-writable-buffer (buffer)
          (insert-string
           (buffer-point buffer)
           (format nil "~&* Process ~S finished with code ~A and status ~A."
                   connection
                   code
                   status)))))))

(defun make-process-connection
    (command &rest args &key name buffer filter sentinel)
  (declare (ignore buffer filter sentinel))
  (apply #'make-instance
         'process-connection
         :name (or name command)
         :command command
         args))


;;;;
;;;; TCP-CONNECTION
;;;;

(defclass tcp-connection (io-connection)
  ((host :initarg :host
         :accessor connection-host)
   (port :initarg :port
         :accessor connection-port)))

(defmethod initialize-instance :after ((instance tcp-connection) &key)
  (unless (connection-io-device instance)
    (let ((socket (#_new QTcpSocket)))
      (setf (connection-io-device instance) socket)
      (connection-note-event instance :initialized)
      (#_connectToHost socket
                       (connection-host instance)
                       (connection-port instance)))))

(defmethod print-object ((instance tcp-connection) stream)
  (print-unreadable-object (instance stream :identity nil :type t)
    (format stream "~A, connected to ~A:~D"
            (connection-name instance)
            (connection-host instance)
            (connection-port instance))))

(defmethod (setf connection-io-device)
    :after
    (newval (connection tcp-connection))
  (connect newval
           (QSIGNAL "connected()")
           (lambda ()
             (note-connected connection)
             (redraw-needed)))
  (connect newval
           (QSIGNAL "disconnected()")
           (lambda ()
             (note-disconnected connection)
             (redraw-needed)))
  (connect newval
           (QSIGNAL "error()")
           (lambda ()
             (note-error connection)
             (redraw-needed))))

(defun make-tcp-connection
    (name host port &rest args &key buffer filter sentinel)
  (declare (ignore buffer filter sentinel))
  (apply #'make-instance
         'tcp-connection
         :name name
         :host host
         :port port
         args))

(defun test ()
  (flet ((connected (c event)
           (case event
             (:connected
              (connection-write
               (format nil "GET / HTTP/1.0~C~C~C~C"
                       #\return #\newline
                       #\return #\newline)
               c))
             #+(or)
             (:disconnected
              (delete-connection c)))))
    (make-tcp-connection "test" "localhost" 80
                         :buffer t
                         :sentinel #'connected)))


;;;;
;;;; FILE-CONNECTION
;;;;

(defclass file-connection (io-connection)
  ((filename :initarg :filename
             :accessor connection-filename)))

(defmethod initialize-instance :after ((instance file-connection) &key)
  (let ((socket (#_new QFile (connection-filename instance))))
    (setf (connection-io-device instance) socket)
    (connection-note-event instance :initialized)
    (#_open socket (#_QIODevice::ReadWrite))))

#+(or)
(defmethod (setf connection-io-device)
    :after
    (newval (connection file-connection))
  )

(defun make-file-connection
    (filename &rest args &key name buffer filter sentinel)
  (declare (ignore buffer filter sentinel))
  (apply #'make-instance
         'file-connection
         :filename filename
         :name (or name filename)
         args))



;;;;
;;;; DESCRIPTOR-CONNECTION
;;;;

(defclass descriptor-connection (io-connection)
  ((descriptor :initarg :descriptor
             :accessor connection-descriptor)))

(defmethod initialize-instance :after ((instance descriptor-connection) &key)
  (let ((socket (#_new QFile)))
    (setf (connection-io-device instance) socket)
    (connection-note-event instance :initialized)
    (#_open socket (connection-descriptor instance) (#_QIODevice::ReadWrite))))

#+(or)
(defmethod (setf connection-io-device)
    :after
    (newval (connection descriptor-connection))
  )

(defun make-descriptor-connection
    (descriptor &rest args &key name buffer filter sentinel)
  (declare (ignore buffer filter sentinel))
  (apply #'make-instance
         'descriptor-connection
         :descriptor descriptor
         :name (or name (format nil "descriptor ~D" descriptor))
         args))


;;;;
;;;; LISTENING-CONNECTION
;;;;

(defclass listening-connection (connection)
  ((server :initarg :server
           :initform nil
           :accessor connection-server)
   (acceptor :initarg :acceptor
             :initform nil
             :accessor connection-acceptor)
   (initargs :initarg :initargs
             :initform nil
             :accessor connection-initargs)))

(defmethod initialize-instance :after
    ((instance listening-connection) &key)
  (when (connection-server instance)
    (connect-server-signals instance)))

(defmethod delete-connection :before ((connection listening-connection))
  (when (connection-server connection)
    (#_close (connection-server connection))))

(defmethod (setf connection-server)
    :after
    ((newval t) (connection listening-connection))
  (connect-server-signals connection))

(defun connect-server-signals (connection)
  (let ((server (connection-server connection)))
    ;; ...
    (connect server
             (QSIGNAL "newConnection()")
             (lambda ()
               (process-incoming-connection connection)))))

(defun %tcp-connection-from-io-device (name io-device initargs)
  (apply #'make-instance
         'tcp-connection
         :name name
         :io-device io-device
         :host (#_peerName io-device)
         :port (#_peerPort io-device)
         initargs))

(defgeneric convert-pending-connection (listener))

(defun process-incoming-connection (listener)
  (let ((connection (convert-pending-connection listener)))
    (let ((buffer (connection-buffer listener)))
      (when buffer
        (with-writable-buffer (buffer)
          (insert-string (buffer-point buffer)
                         (format nil "~&* ~A." connection)))))
    (when (connection-acceptor listener)
      (funcall (connection-acceptor listener) connection))))


;;;;
;;;; TCP-LISTENER
;;;;

(defclass tcp-listener (listening-connection)
  ((host :initarg :host
         :accessor connection-host)
   (port :initarg :port
         :accessor connection-port)))

(defmethod initialize-instance :after ((instance tcp-listener) &key)
  (check-type (connection-host instance) string)
  (check-type (connection-port instance)
              (or null (unsigned-byte 16)))
  (let ((server (#_new QTcpServer)))
    (setf (connection-server instance) server)
    (connection-note-event instance :initialized)
    (unless (#_listen server
                      (#_new QHostAddress (connection-host instance))
                      (or (connection-port instance) 0))
      (error "failed to listen on connection ~A" instance))
    (setf (connection-port instance) (#_serverPort server))))

(defmethod print-object ((instance tcp-listener) stream)
  (print-unreadable-object (instance stream :identity nil :type t)
    (format stream "~A ~A:~D"
            (connection-name instance)
            (connection-host instance)
            (connection-port instance))))

(defun make-tcp-listener
    (name host port &rest args &key buffer acceptor sentinel initargs)
  (declare (ignore buffer acceptor sentinel initargs))
  (apply #'make-instance
         'tcp-listener
         :name name
         :host host
         :port port
         args))

(defmethod convert-pending-connection ((connection tcp-listener))
  (%tcp-connection-from-io-device
   (format nil "Accepted for: ~A" (connection-name connection))
   (#_nextPendingConnection (connection-server connection))
   (connection-initargs connection)))


;;; wire interaction

(defstruct (connection-device
             (:include hemlock.wire:device)
           (:conc-name "DEVICE-")
           (:constructor %make-connection-device (connection)))
  (connection (error "missing argument") :type connection)
  (reading 0 :type integer)
  (filter-counter 0 :type integer)
  (original-sentinel nil))

(defmethod print-object ((object connection-device) stream)
  (print-unreadable-object (object stream)
    (format stream "~A" (device-connection object))))

(defun make-connection-device (connection)
  (let ((device (%make-connection-device connection)))
    (setf (connection-filter connection)
          (lambda (connection bytes)
            (connection-device-filter device connection bytes)))
    (setf (device-original-sentinel device)
          (connection-sentinel connection))
    (setf (connection-sentinel connection)
          (lambda (connection event)
            (connection-device-sentinel device connection event)))
    device))

(defun connection-device-filter (device connection bytes)
  (declare (ignore connection))
  (setf bytes (copy-seq bytes))
  (later
   (incf (device-filter-counter device))
   (hemlock.wire:device-append-to-input-buffer device bytes)
   (when (zerop (device-reading device))
     (hemlock.wire:device-serve-requests device)))
  nil)

(defun connection-device-sentinel (device connection event)
  (when (device-original-sentinel device)
    (funcall (device-original-sentinel device) connection event))
  ;; fixme: anything else to do here?
  )

(defmethod hemlock.wire:device-listen
    ((device connection-device))
  (connection-listen (device-connection device)))

(defmethod hemlock.wire:device-write
    ((device connection-device) buffer &optional (end (length buffer)))
  (connection-write (subseq buffer 0 end) (device-connection device)))

(defmethod hemlock.wire:device-read
    ((device connection-device) buffer)
  (unwind-protect
       (let ((previous-counter (device-filter-counter device)))
         (incf (device-reading device))
         (iter (while (eql previous-counter (device-filter-counter device)))
               (process-one-event)))
    (decf (device-reading device)))
  0)
