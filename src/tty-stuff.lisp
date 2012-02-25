;;;; -*- Mode: Lisp; indent-with-tabs: nil -*-

(in-package :hi)

;;; Terminal hunks.
;;;
(defclass tty-hunk (device-hunk)
  ((text-position :initarg :text-position
                  :accessor tty-hunk-text-position)
   (text-height :initarg :text-height
                :accessor tty-hunk-text-height)))

(defun make-tty-hunk (&rest args
                      &key position height text-position text-height device)
  (declare (ignore position height text-position text-height device))
  (apply #'make-instance 'tty-hunk args))
