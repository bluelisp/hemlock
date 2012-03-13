;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(in-package :hi)


;;;; PREPL/background buffer integration

(declaim (special hi::*in-hemlock-slave-p*
                  hemlock::*master-machine-and-port*
                  hemlock::*original-terminal-io*))

(defun need-to-redirect-debugger-io (stream)
  (eq stream hemlock::*original-terminal-io*))

(defun call-with-typeout-for-thread-debugger (cont)
  (with-new-event-loop ()
    (let ((prepl:*entering-prepl-debugger-hook* nil)
          (hi::*in-hemlock-slave-p* t)
          (hemlock.wire:*current-wire* :not-yet))
      (hemlock::connect-to-editor-for-background-thread
       (car hemlock::*master-machine-and-port*)
       (cadr hemlock::*master-machine-and-port*))
      (dispatch-events-no-hang)
      (do ()
          ((not (eq hemlock.wire:*current-wire* :not-yet)))
        (dispatch-events)
        (write-line "Thread waiting for connection to master..."
                    hemlock::*original-terminal-io*)
        (force-output hemlock::*original-terminal-io*))
      (with-typeout-pop-up-in-master
          (*terminal-io* (format nil "Slave thread ~A"
                                 (bt:thread-name (bt:current-thread))))
        (call-with-standard-synonym-streams cont)))))

;;; Setup an a connection to the editor for the current thread, and
;;; create an editor buffer for I/O and return the client stream.
(defun typeout-for-thread ()
  (assert (or (not (boundp '*event-base*)) (not *event-base*)))
  (setf *event-base* (make-event-loop *connection-backend*))
  (setf hi::*in-hemlock-slave-p* t)
  (let ((hemlock.wire:*current-wire* :not-yet))
    (hemlock::connect-to-editor-for-background-thread
     (car hemlock::*master-machine-and-port*)
     (cadr hemlock::*master-machine-and-port*))
    (dispatch-events-no-hang)
    (do ()
        ((not (eq hemlock.wire:*current-wire* :not-yet)))
      (dispatch-events)
      (write-line "Thread waiting for connection to master..."
                  hemlock::*original-terminal-io*)
      (force-output hemlock::*original-terminal-io*))
    (let* ((name (format nil "Slave thread ~A"
                         (bt:thread-name (bt:current-thread))))
           (ts-data (hemlock.wire:remote-value hemlock.wire:*current-wire*
                     (hemlock::%make-extra-typescript-buffer name))))
      (hemlock::connect-stream ts-data hemlock.wire:*current-wire*))))

