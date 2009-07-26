;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock.wire)

;; HEMLOCK-USE-USOCKET doesn't actually work yet, because usocket doesn't
;; have an :EXTERNAL-FORMAT arg, defaulting to ASCII on SBCL.

(defun ext-create-inet-listener (port)
  #+HEMLOCK-USE-USOCKET
  (usocket:socket-listen "0.0.0.0" port)
  #-HEMLOCK-USE-USOCKET
  (progn
    #+CMU
    (ext:create-inet-listener port)
    #+EXCL
    (socket:make-socket :connect :passive
                        :local-port port
                        :format :text)
    #+CLISP
    (socket:socket-server port)
    #+SBCL
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream
                                 :protocol (sb-bsd-sockets:get-protocol-by-name "tcp"))))
      (sb-bsd-sockets:socket-bind socket (sb-bsd-sockets:make-inet-address "0.0.0.0") port)
      (sb-bsd-sockets:socket-listen socket 2)
      socket)
    #-(OR CMU EXCL CLISP SBCL)
    #.(error "Configure")))

(defun ext-accept-tcp-connection (socket)
  #+HEMLOCK-USE-USOCKET
  (usocket:socket-accept socket)
  (progn
    #+CMU (ext:accept-tcp-connection socket)
    #+EXCL
    (values
     (socket:accept-connection socket :wait t)
     (socket:remote-host socket))
    #+CLISP
    (let ((stream (socket:socket-accept socket)))
      #+NIL (setf (stream-element-type stream) '(unsigned-byte 8))
      (values
       stream
       (multiple-value-list (socket:socket-stream-peer stream))))
    #+SBCL
    (multiple-value-bind (socket peer-host #+(or) peer-port)
        (sb-bsd-sockets:socket-accept socket)
      (values (sb-bsd-sockets:socket-make-stream
               socket
               :element-type 'character
               :input t
               :output t
               :external-format :iso-8859-1)
              peer-host))
    #-(OR CMU EXCL CLISP SBCL)
    #.(error "Configure")
    ))

(defun ext-connect-to-inet-socket (host port)
  #+HEMLOCK-USE-USOCKET
  (usocket:socket-connect host port)
  #-HEMLOCK-USE-USOCKET
  (progn
    #+CMU (ext:connect-to-inet-socket host port)
    #+EXCL
    (progn
      #+(and allegro-version>= (version>= 5))
      (socket:make-socket :remote-host host
                          :remote-port port
                          :format :text)
      #-(and allegro-version>= (version>= 5))
      (ipc:open-network-stream
       :host host :port port
       :element-type 'character
       ;; :class EXCL::BIDIRECTIONAL-BINARY-SOCKET-STREAM
       ))
    #+SBCL
    (sb-bsd-sockets:socket-make-stream
     (let ((host (car (sb-bsd-sockets:host-ent-addresses
                       (sb-bsd-sockets:get-host-by-name host)))))
       (when host
         (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
           (sb-bsd-sockets:socket-connect s host port)
           s)))
     :element-type 'character           ;(unsigned-byte 8)
     :input t
     :output t
     :external-format :iso-8859-1)
    #+CLISP
    (socket:socket-connect port host)
    #-(OR CMU EXCL CLISP SBCL)
    #.(error "Configure")))

(defun ext-close-socket (socket)
  #+HEMLOCK-USE-USOCKET
  (usocket:socket-close socket)
  #-HEMLOCK-USE-USOCKET
  (progn
    #+CMU   (ext:close-socket socket)
    #+EXCL  (close socket)
    #+CLISP (socket:socket-server-close socket)
    #+SBCL  (sb-bsd-sockets:socket-close socket)
    #-(OR CMU EXCL CLISP SBCL)
    #.(error "Configure")))

(defun ext-close-connection (connection)
  #+HEMLOCK-USE-USOCKET
  (close connection)
  #-HEMLOCK-USE-USOCKET
  (progn
    #+CMU   (ext:close-socket connection)
    #+EXCL  (close connection)
    #+CLISP (close connection)
    #+SBCL  (close connection)
    #-(OR CMU EXCL CLISP SBCL)
    #.(error "Configure")))

(defun unix-gethostid ()
  #.(or
     #+CMU '(unix:unix-gethostid)
     398792))

(defun unix-getpid ()
  #.(or
     #+CMU   '(unix:unix-getpid)
     #+SBCL  '(sb-unix:unix-getpid)
     #+ACL   '(excl::getpid)
     #+CLISP '(system::program-id)))

(defun make-process (function &key name)
  (bt:make-thread function
                  :name name
                  :initial-bindings `((*print-readably* .,(constantly nil))
                                      ,@bt:*default-special-bindings*)))

;; !!!
(push (cons '*print-readably* (constantly nil))
      bt:*default-special-bindings*)
