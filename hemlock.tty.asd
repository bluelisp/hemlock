;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock.tty
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:hemlock.base)
    :components
    ((:module tty-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :serial t
              :components
              ((:file "ioconnections")
               (:file "terminfo")
               (:file "termcap")
               (:file "tty-disp-rt")
               (:file "tty-display")
               (:file "tty-screen")
               (:file "tty-stuff")
               (:file "tty-input")))))
