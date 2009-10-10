;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+NIL
(ext:file-comment
  "$Header: /home/david/phemlock/cvsroot/phemlock/src/wire/remote.lisp,v 1.1 2004-07-09 13:38:24 gbaumann Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file implements a simple remote procedure call mechanism on top
;;; of wire.lisp.
;;;
;;; Written by William Lott.
;;;

(in-package :hemlock.wire)

(defstruct remote-wait
  value1 value2 value3 value4 value5
  abort
  finished)

(defvar *pending-returns* nil
  "AList of wire . remote-wait structs")

;;; MAYBE-NUKE-REMOTE-WAIT -- internal
;;;
;;; If the remote wait has finished, remove the external translation.
;;; Otherwise, mark the remote wait as finished so the next call to
;;; MAYBE-NUKE-REMOTE-WAIT will really nuke it.
;;;
(defun maybe-nuke-remote-wait (remote)
  (cond ((remote-wait-finished remote)
         (forget-remote-translation remote)
         t)
        (t
         (setf (remote-wait-finished remote)
               t)
         nil)))

;;; REMOTE -- public
;;;
;;; Execute the body remotely. Subforms are executed locally in the lexical
;;; environment of the macro call. No values are returned.
;;;
(defmacro remote (wire-form &body forms)
  "Evaluates the given forms remotly. No values are returned, as the remote
evaluation is asyncronous."
  (let ((wire (gensym)))
    `(let ((,wire ,wire-form))
       ,@(mapcar #'(lambda (form)
                     `(wire-output-funcall ,wire
                                           ',(car form)
                                           ,@(cdr form)))
           forms)
       (wire-force-output ,wire)
       (values))))

;;; REMOTE-VALUE-BIND -- public
;;;
;;; Send to remote forms. First, a call to the correct dispatch routine based
;;; on the number of args, then the actual call. The dispatch routine will get
;;; the second funcall and fill in the correct number of arguments.
;;; Note: if there are no arguments, we don't even wait for the function to
;;; return, cause we can kind of guess at what the currect results would be.
;;;
(defmacro remote-value-bind (wire-form vars form &rest body)
  "Bind VARS to the multiple values of FORM (which is executed remotely). The
forms in BODY are only executed if the remote function returned (as apposed
to aborting due to a throw)."
  (cond
   ((null vars)
    `(progn
       (remote ,wire-form ,form)
       ,@body))
   (t
    (let ((remote (gensym))
          (wire (gensym)))
      `(let* ((,remote (make-remote-wait))
              (,wire ,wire-form)
              (*pending-returns* (cons (cons ,wire ,remote)
                                       *pending-returns*)))
         (unwind-protect
             (let ,vars
               (remote ,wire
                 (,(case (length vars)
                     (1 'do-1-value-call)
                     (2 'do-2-value-call)
                     (3 'do-3-value-call)
                     (4 'do-4-value-call)
                     (5 'do-5-value-call)
                     (t 'do-n-value-call))
                  (make-remote-object ,remote))
                 ,form)
               (wire-force-output ,wire)
               (loop
                  (dispatch-events)
                  (when (remote-wait-finished ,remote)
                    (return)))
               (unless (remote-wait-abort ,remote)
                 ,(case (length vars)
                    (1 `(setf ,(first vars) (remote-wait-value1 ,remote)))
                    (2 `(setf ,(first vars) (remote-wait-value1 ,remote)
                              ,(second vars) (remote-wait-value2 ,remote)))
                    (3 `(setf ,(first vars) (remote-wait-value1 ,remote)
                              ,(second vars) (remote-wait-value2 ,remote)
                              ,(third vars) (remote-wait-value3 ,remote)))
                    (4 `(setf ,(first vars) (remote-wait-value1 ,remote)
                              ,(second vars) (remote-wait-value2 ,remote)
                              ,(third vars) (remote-wait-value3 ,remote)
                              ,(fourth vars) (remote-wait-value4 ,remote)))
                    (5 `(setf ,(first vars) (remote-wait-value1 ,remote)
                              ,(second vars) (remote-wait-value2 ,remote)
                              ,(third vars) (remote-wait-value3 ,remote)
                              ,(fourth vars) (remote-wait-value4 ,remote)
                              ,(fifth vars) (remote-wait-value5 ,remote)))
                    (t
                     (do ((remaining-vars vars (cdr remaining-vars))
                          (form (list 'setf)
                                (nconc form
                                       (list (car remaining-vars)
                                             `(pop values)))))
                         ((null remaining-vars)
                          `(let ((values (remote-wait-value1 ,remote)))
                             ,form)))))
                 ,@body))
           (maybe-nuke-remote-wait ,remote)))))))


;;; DEFINE-FUNCTIONS -- internal
;;;
;;;   Defines two functions, one that the client runs in the server, and one
;;; that the server runs in the client:
;;;
;;; DO-n-VALUE-CALL -- internal
;;;
;;;   Executed by the remote process. Reads the next object off the wire and
;;; sends the value back. Unwind-protect is used to make sure we send something
;;; back so the requestor doesn't hang.
;;;
;;; RETURN-n-VALUE -- internal
;;;
;;;   The remote procedure returned the given value, so fill it in the
;;; remote-wait structure. Note, if the requestor has aborted, just throw
;;; the value away.
;;;
(defmacro define-functions (values)
  (let ((do-call (intern (format nil "~:@(do-~D-value-call~)" values)))
        (return-values (intern (format nil "~:@(return-~D-value~:P~)" values)))
        (vars nil))
    (dotimes (i values)
      (push (gensym) vars))
    (setf vars (nreverse vars))
    `(progn
       (defun ,do-call (result)
         (let (worked ,@vars)
           (unwind-protect
               (progn
                 (multiple-value-setq ,vars
                   (wire-get-object *current-wire*))
                 (setf worked t))
             (if worked
               (remote *current-wire*
                 (,return-values result ,@vars))
               (remote *current-wire*
                 (remote-return-abort result)))
             (wire-force-output *current-wire*))))
       (defun ,return-values (remote ,@vars)
         (let ((result (remote-object-value remote)))
           (unless (maybe-nuke-remote-wait result)
             ,@(let ((setf-forms nil))
                 (dotimes (i values)
                   (push `(setf (,(intern (format nil
                                                  "~:@(remote-wait-value~D~)"
                                                  (1+ i)))
                                 result)
                                ,(nth i vars))
                         setf-forms))
                 (nreverse setf-forms))))
         nil))))

(define-functions 1)
(define-functions 2)
(define-functions 3)
(define-functions 4)
(define-functions 5)


;;; REMOTE-VALUE -- public
;;;
;;; Alternate interface to getting the single return value of a remote
;;; function. Works pretty much just the same, except the single value is
;;; returned.
;;;
(defun invoke-with-wire-and-remote (wire fun &optional on-server-unwind)
  "Execute the single form remotly. The value of the form is returned.
  The optional form on-server-unwind is only evaluated if the server unwinds
  instead of returning."
  (let* ((remote (make-remote-wait))
         (*pending-returns* (cons (cons wire remote)
                                  *pending-returns*)))
    (unwind-protect
         (progn
           (funcall fun wire remote)
           (wire-force-output wire)
           (loop
              (wire-get-object wire)
              (when (remote-wait-finished remote)
                (return))))
      (maybe-nuke-remote-wait remote))
    (cond
      ((not (remote-wait-abort remote))
       (remote-wait-value1 remote))
      (on-server-unwind
       (funcall on-server-unwind))
      (t
       (error "Remote server unwound")))))

(defmacro remote-value (wire-form form &optional on-server-unwind)
  "Execute the single form remotely. The value of the form is returned.
  The optional form on-server-unwind is only evaluated if the server unwinds
  instead of returning."
  (let ((wire (gensym))
        (remote (gensym)))
    `(invoke-with-wire-and-remote
      ,wire-form
      (lambda (,wire ,remote)
        (remote ,wire
                (do-1-value-call (make-remote-object ,remote))
                ,form))
      ,@(when on-server-unwind
          `((lambda ()
              ,on-server-unwind))))))


;;; DO-N-VALUE-CALL -- internal
;;;
;;; For more values then 5, all the values are rolled into a list and passed
;;; back as the first value, so we use RETURN-1-VALUE to return it.
;;;
(defun do-n-value-call (result)
  (let (worked values)
    (unwind-protect
        (progn
          (setf values
                (multiple-value-list (wire-get-object *current-wire*)))
          (setf worked t))
      (if worked
        (remote *current-wire*
          (return-1-values result values))
        (remote *current-wire*
          (remote-return-abort result)))
      (wire-force-output *current-wire*))))

;;; REMOTE-RETURN-ABORT -- internal
;;;
;;; The remote call aborted instead of returned.
;;;
(defun remote-return-abort (result)
  (setf result (remote-object-value result))
  (unless (maybe-nuke-remote-wait result)
    (setf (remote-wait-abort result) t)))

;;; SERVE-REQUESTS -- internal
;;;
;;; Serve all pending requests on the given wire.
;;;
;;; On asynchronous connections, the event loop will call this.
;;;
;;; On synchronous connections, it is the caller's responsibility to call it
;;; while waiting for requests.
;;;
(defun serve-requests (wire &optional force)
  (loop
     (unless (or force (wire-listen wire))
       (return))
     (setf force nil)
     (wire-get-object wire))
  (values))

(defun device-serve-requests (device &optional force)
  (serve-requests (device-wire device) force))
