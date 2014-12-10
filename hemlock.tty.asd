;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock.tty
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:hemlock.base
                  :hemlock.iolib 
                  :terminfo)
    :components
    ((:module tty-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src" "tty"))
                           *hemlock-base-directory*)
              :components
              ((:file "init" :depends-on ("tty-ext"))
               (:file "tty-device")
               (:file "tty-ext" :depends-on ("tty-device"))
               (:file "tty-display" :depends-on ("tty-ext"))
               (:file "tty-screen" :depends-on ("tty-ext"))
               (:file "tty-stuff")
               (:file "tty-input")
               (:file "linedit" :depends-on ("tty-display"))))))
