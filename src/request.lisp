;;;; -*- Mode: Lisp; indent-with-tabs: nil -*-

(in-package :hemlock)


;;; REQUEST-SERVER structure
;;;
;;; Just a simple handle on the socket and system:serve-event handler that make
;;; up a request server.
;;;
(defstruct (request-server
            (:print-function %print-request-server))
  listener)

(defun %print-request-server (rs stream depth)
  (declare (ignore depth))
  (print-unreadable-object (rs stream :type t)
    (format stream "for ~D" (request-server-listener rs))))

;;; *request-server-interface*
;;;
;;; Address to listen on for connections from slaves.
;;;
(defvar *request-server-interface* "127.0.0.1")

;;; CREATE-REQUEST-SERVER -- Public.
;;;
;;; Create a TCP/IP listener on the given port.  If anyone tries to connect to
;;; it, call NEW-CONNECTION to do the connecting.
;;;
(defun create-request-server (&optional port)
  "Create a request server on the given port.  Whenever anyone connects to it,
   call the given function with the newly created wire and the address of the
   connector.  If the function returns NIL, the connection is destroyed;
   otherwise, it is accepted.  This returns a manifestation of the server that
   DESTROY-REQUEST-SERVER accepts to kill the request server."
  (let ((listener
         (make-tcp-listener
          "request server"
          *request-server-interface*
          port
          :acceptor (lambda (connection)
                      (hemlock.wire:make-wire
                       (make-connection-device connection)))
          :buffer t)))
    (values (make-request-server :listener listener)
            (connection-port listener))))

;;; DESTROY-REQUEST-SERVER -- Public.
;;;
;;; Removes the request server from SERVER's list of file descriptors and
;;; closes the socket behind it.
;;;
(defun destroy-request-server (server)
  "Quit accepting connections to the given request server."
  (delete-connection (request-server-listener server))
  nil)

;;; CONNECT-TO-REMOTE-SERVER -- Public.
;;;
;;; Just like the doc string says, connect to a remote server. A handler is
;;; installed to handle return values, etc.
;;;
(defun connect-to-remote-server (hostname port on-connected &optional on-death)
  (declare (ignore on-death))           ;fixme?
  "Connect to a remote request server addressed with the given host and port
   pair.  This returns the created wire."
  (let (wire)
    (flet ((sentinel (connection event)
             (ecase event
               (:initialized
                (setf wire (hemlock.wire:make-wire
                            (make-connection-device
                             connection))))
               (:connected
                (funcall on-connected wire)))))
      (make-tcp-connection "Connection to master"
                           hostname
                           port
                           :sentinel #'sentinel))))
