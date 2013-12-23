;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock.iolib
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:hemlock.base)
    :components
    ((:module iolib
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src" "iolib"))
                           *hemlock-base-directory*)
              :components
              ((:file "ioconnections")))))
