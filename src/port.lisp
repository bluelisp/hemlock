;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock.wire)

(defun unix-gethostid ()
  #.(or
     #+CMU '(unix:unix-gethostid)
     398792))

(defun unix-getpid ()
  (conium:getpid))

;; !!!
(push (cons '*print-readably* (constantly nil))
      bt:*default-special-bindings*)
