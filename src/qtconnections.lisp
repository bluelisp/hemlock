;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hi)
(named-readtables:in-readtable :hemlock.qt)

;;;;
;;;; QIODEVICE-CONNECTION
;;;;

(defclass qiodevice-connection (io-connection)
  ((io-device :initarg :io-device
              :initform nil
              :accessor connection-io-device)))

(defmethod initialize-instance :after
    ((instance qiodevice-connection) &key)
  (when (connection-io-device instance)
    (connect-io-device-signals instance)))

(defmethod delete-connection :before ((connection qiodevice-connection))
  (when (connection-io-device connection)
    (#_close (connection-io-device connection))))

(defmethod connection-listen ((connection qiodevice-connection))
  (plusp (#_bytesAvailable (connection-io-device connection))))

(defmethod connection-write (data (connection qiodevice-connection))
  (let ((bytes (filter-connection-output connection data)))
    (check-type bytes (simple-array (unsigned-byte 8) (*)))
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
          (error "error: chunked writes not implemented"))))))

(defmethod %read ((connection qiodevice-connection))
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

(defun connect-io-device-signals (connection)
  (let ((device (connection-io-device connection)))
    ;; ...
    (hemlock.qt::connect device
             (qt:QSIGNAL "readyRead()")
             (lambda ()
               (process-incoming-data connection)))))

(defmethod (setf connection-io-device)
    :after
    ((newval t) (connection qiodevice-connection))
  (connect-io-device-signals connection))


;;;;
;;;; PROCESS-CONNECTION/QT
;;;;

(defclass process-connection/qt (process-connection-mixin qiodevice-connection)
  ((command :initarg :command
            :accessor connection-command)
   (exit-code :initform nil
              :initarg :exit-code
              :accessor connection-exit-code)
   (exit-status :initform nil
                :initarg :exit-status
                :accessor connection-exit-status)))

(defmethod initialize-instance :after ((instance process-connection/qt) &key)
  (with-slots (command slave-pty-name)
      instance
    (when slave-pty-name
      (setf command
            (list* (merge-pathnames "setpty" (installation-directory))
                   slave-pty-name
                   (listify command)))))
  (let ((process (#_new QProcess)))
    (setf (connection-io-device instance) process)
    (connection-note-event instance :initialized)
    (#_start process (format nil "~{ ~A~}" (listify (connection-command instance))))))

(defmethod (setf connection-io-device)
    :after
    (newval (connection process-connection/qt))
  (hemlock.qt::connect newval
           (qt:QSIGNAL "finished(int,QProcess::ExitStatus)")
           (lambda (&rest *)
             (note-finished connection))))

(defun note-finished (connection)
  (let* ((process (connection-io-device connection))
         (code (#_exitCode process))
         (status (#_exitStatus process)))
    (setf (connection-exit-code connection) code)
    (setf (connection-exit-status connection) status)
    (connection-note-event connection :finished)
    (format-to-connection-buffer-or-stream
     connection
     "~&* Process ~S finished with code ~A and status ~A."
     connection
     code
     status)))


;;;;
;;;; TCP-CONNECTION/QT
;;;;

(defclass tcp-connection/qt (tcp-connection-mixin qiodevice-connection)
  ())

(defmethod initialize-instance :after ((instance tcp-connection/qt) &key)
  (unless (connection-io-device instance)
    (let ((socket (#_new QTcpSocket)))
      (setf (connection-io-device instance) socket)
      (connection-note-event instance :initialized)
      (#_connectToHost socket
                       (connection-host instance)
                       (connection-port instance)))))

(defmethod (setf connection-io-device)
    :after
    (newval (connection tcp-connection/qt))
  (hemlock.qt::connect newval
           (qt:QSIGNAL "connected()")
           (lambda ()
             (note-connected connection)))
  (hemlock.qt::connect newval
           (qt:QSIGNAL "disconnected()")
           (lambda ()
             (note-disconnected connection)))
  (hemlock.qt::connect newval
           (qt:QSIGNAL "error()")
           (lambda ()
             (note-error connection))))

;;;
;;; PIPELIKE-CONNECTION/QT
;;;

(defclass pipelike-connection/qt
    (pipelike-connection-mixin qiodevice-connection)
  ())

(defmethod initialize-instance
    :after
    ((instance pipelike-connection/qt) &key read-fd write-fd)
  (assert (eql read-fd write-fd))
  (assert (plusp read-fd))
  (let ((socket
         ;; hack: QFile doesn't work for device files, so use a QLocalSocket
         ;; instead and set its descriptor.  Technically, the file isn't
         ;; a named pipe in our case, but it works.
         (#_new QLocalSocket)))
    (setf (connection-io-device instance) socket)
    (connection-note-event instance :initialized)
    (cffi:with-foreign-object (place :int)
      (setf (cffi:mem-ref place :int) read-fd)
      (#_setSocketDescriptor
       socket
       (qt::quintptr place)
       ;; (#_QLocalSocket::ConnectedState)
       ;; (#_QIODevice::ReadWrite)
       ))))

(defmethod (setf connection-io-device)
    :after
    (newval (connection pipelike-connection/qt))
  (hemlock.qt::connect newval
           (qt:QSIGNAL "connected()")
           (lambda ()
             (note-connected connection)))
  (hemlock.qt::connect newval
           (qt:QSIGNAL "disconnected()")
           (lambda ()
             (note-disconnected connection)))
  (hemlock.qt::connect newval
           (qt:QSIGNAL "error()")
           (lambda ()
             (note-error connection))))


;;;
;;; PROCESS-WITH-PTY-CONNECTION/QT
;;;

(defclass process-with-pty-connection/qt
    (process-with-pty-connection-mixin pipelike-connection/qt)
  ())


;;;;
;;;; LISTENING-CONNECTION/QT
;;;;

(defclass listening-connection/qt (listening-connection)
  ())

(defmethod initialize-instance :after
    ((instance listening-connection/qt) &key)
  (when (connection-server instance)
    (connect-server-signals instance)))

(defmethod delete-connection :before ((connection listening-connection/qt))
  (when (connection-server connection)
    (#_close (connection-server connection))))

(defmethod (setf connection-server)
    :after
    ((newval t) (connection listening-connection/qt))
  (connect-server-signals connection))

(defun connect-server-signals (connection)
  (let ((server (connection-server connection)))
    ;; ...
    (hemlock.qt::connect server
             (qt:QSIGNAL "newConnection()")
             (lambda ()
               (process-incoming-connection connection)))))


;;;;
;;;; TCP-LISTENER/QT
;;;;

(defclass tcp-listener/qt (tcp-listener-mixin listening-connection/qt)
  ())

(defmethod initialize-instance :after ((instance tcp-listener/qt) &key)
  (let ((server (#_new QTcpServer)))
    (setf (connection-server instance) server)
    (connection-note-event instance :initialized)
    (unless (#_listen server
                      (#_new QHostAddress (connection-host instance))
                      (or (connection-port instance) 0))
      (error "failed to listen on connection ~A" instance))
    (setf (connection-port instance) (#_serverPort server))))

(defun %tcp-connection-from-io-device (name io-device initargs)
  (apply #'make-instance
         'tcp-connection/qt
         :name name
         :io-device io-device
         :host (#_peerName io-device)
         :port (#_peerPort io-device)
         initargs))

(defmethod convert-pending-connection ((connection tcp-listener/qt))
  (%tcp-connection-from-io-device
   (format nil "Accepted for: ~A" (connection-name connection))
   (#_nextPendingConnection (connection-server connection))
   (connection-initargs connection)))
