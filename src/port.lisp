;;;; -*- Mode: Lisp; indent-with-tabs: nil -*-

(in-package :hemlock.wire)

(defun unix-gethostid ()
  #.(or
     #+CMU '(unix:unix-gethostid)
     398792))

(defun unix-getpid ()
  (conium:getpid))

;; fixme: remove this?
(push (cons '*print-readably* nil)
      bt:*default-special-bindings*)
