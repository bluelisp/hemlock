;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Lisp Implementation Dependent Stuff for Hemlock
;;;   Created: 2002-11-07
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

(in-package :hemlock-ext)

#+CLISP
(progn
  (setf custom:*FLOATING-POINT-CONTAGION-ANSI* t)
  (setf custom:*WARN-ON-FLOATING-POINT-CONTAGION* nil))

(defun getenv (name)
  (osicat:environment-variable name)))

(defmacro without-interrupts (&body body)
  `(#+EXCL   excl:without-interrupts
    #+CMU    sys:without-interrupts
    #+sbcl   sb-sys:without-interrupts
    #+openmcl ccl:without-interrupts
    #-(or EXCL CMU sbcl openmcl) progn
    ,@body))

#-(or CMU scl)
(defmacro fixnump (object)
  #+EXCL  `(excl:fixnump ,object)
  #+CLISP `(sys::fixnump ,object)
  #-(or EXCL CLISP) `(typep ,object 'fixnum))

#-(or cmu scl)
(defun file-writable (pathname)
  "File-writable accepts a pathname and returns T if the current
  process can write it, and NIL otherwise. Also if the file does
  not exist return T."
  (handler-case (let ((io (open pathname
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist nil)))
                  (if io
                      (close io :abort t)
                      ;; more complicate situation:
                      ;; we want test if we can create the file.
                      (let ((io (open pathname
                                      :direction :output
                                      :if-exists nil
                                      :if-does-not-exist :create)))
                        (if io
                            (progn
                              (close io)
                              (delete-file io))
                            t))))
    (file-error (err)
                (declare (ignore err))
                nil)) )
