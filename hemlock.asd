;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage "MK"  (:export "DEFSYSTEM"))
(defpackage :hemlock-system (:use "CL" "ASDF"))
(in-package :hemlock-system)

(with-open-file (in (merge-pathnames "hemlock.system" *load-truename*))
  (loop for form = (read in nil nil)
        while form
        if (eql (car form) 'mk::defsystem)
        do (destructuring-bind (name &key components &allow-other-keys)
               (cdr form)
             (eval `(asdf:defsystem ,name :serial t :depends-on (clx)
                     :components
                     ,(mapcar (lambda (x) `(:file ,x
                                            :pathname
                                            ,(merge-pathnames
                                              (make-pathname
                                               :name x
                                               :directory '(:relative "src")
                                               :type "lisp")
                                              *load-truename*)))

                              components))))
        else do (eval form)))
