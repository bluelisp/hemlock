;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock.clx
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:hemlock.base :hemlock.iolib :clx)
    :components
    ((:module clx-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :components
              ((:file "bit-stuff")
               (:file "hunk-draw" :depends-on ("bit-stuff"))
               (:file "bitmap-rompsite")
               (:file "ioconnections")
               (:file "bitmap-input")
               (:file "bit-display" :depends-on ("hunk-draw"))
               (:file "bit-screen")
               (:file "bitmap-ext")))))
