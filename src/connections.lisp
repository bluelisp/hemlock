;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :qt-hemlock)
(named-readtables:in-readtable :qt-hemlock)

(defvar *all-connections* nil)

(defclass connection ()
  ((name :initarg :name
         :accessor connection-name)))

(defmethod initialize-instance :after ((instance connection) &key)
  (push instance *all-connections*))

(defclass process-connection ()
  ())

(defclass tcp-connection (connection)
  ((host :initarg :host
         :accessor host)
   (port :initarg :port
         :accessor port)
   (connected-callback :initarg :connected-callback
                       :initform nil
                       :accessor connected-callback)
   (disconnected-callback :initarg :disconnected-callback
                          :initform nil
                          :accessor disconnected-callback)
   (data-callback :initarg :data-callback
                  :initform nil
                  :accessor data-callback)
   (io-device :initarg :io-device
              :accessor io-device)
   (input-buffer :initform (make-array #x2000 :element-type '(unsigned-byte 8))
                 :accessor input-buffer)))

(defun connection-write (bytes connection)
  ;; fixme: with-pointer-to-vector-data isn't portable
  (cffi-sys:with-pointer-to-vector-data (ptr bytes)
    (let ((n-bytes-written
           (#_write (io-device connection)
                    (qt::char* ptr)
                    (length bytes))))
      (when (minusp n-bytes-written)
        (error "error on socket: ~A" connection))
      (unless (eql n-bytes-written (length bytes))
        ;; fixme: buffering
        (error "oops, not implemented")))))

(defun %read (connection)
  (let* ((io (io-device connection))
         (n-bytes-available (#_bytesAvailable io))
         (buffer (input-buffer connection)))
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

(defmethod initialize-instance :after ((instance tcp-connection) &key)
  (let ((socket (#_new QTcpSocket)))
    (setf (io-device instance) socket)
    (connect socket
             (QSIGNAL "connected()")
             (lambda ()
               (let ((cb (connected-callback instance)))
                 (when cb (funcall cb instance)))))
    (connect socket
             (QSIGNAL "disconnected()")
             (lambda ()
               (let ((cb (disconnected-callback instance)))
                 (when cb (funcall cb instance)))))
    (connect socket
             (QSIGNAL "readyRead()")
             (lambda ()
               (print (list :readyread
                            (babel:octets-to-string (%read instance))))))
    (#_connectToHost socket (host instance) (port instance))))

(defun make-tcp-connection
    (name host port &rest initargs)
  (apply #'make-instance
         'tcp-connection
         :name name
         :host host
         :port port
         initargs))

(defun test ()
  (flet ((connected (c)
           (connection-write
            (babel:string-to-octets
             (format nil "GET / HTTP/1.0~C~C~C~C"
                     #\return #\newline
                     #\return #\newline))
            c)))
    (make-tcp-connection "test" "localhost" 80
                         :connected-callback #'connected)))
