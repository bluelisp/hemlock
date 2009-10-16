;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock.qt
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:hemlock.base :qt :qt-repl)
    :components
    ((:module qt-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :serial t
              :components
              ((:file "qt-package")
               (:file "qt")
               (:file "browser")
               (:file "qtconnections")
               (:file "sugiyama")
               (:file "graphics")))))
