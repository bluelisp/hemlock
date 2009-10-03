;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(proclaim '(optimize (safety 3) (speed 0) (debug 3)))

(defpackage #:hemlock-system
  (:use #:cl)
  (:export #:*hemlock-base-directory* #:iso-8859-1-file))

(in-package #:hemlock-system)

(defclass iso-8859-1-file (asdf:cl-source-file) ())
(defmethod asdf:perform ((o asdf:compile-op) (c iso-8859-1-file))
  ;; Darn.  Can't just CALL-NEXT-METHOD; have to reimplement the
  ;; world.
  (let ((source-file (asdf:component-pathname c))
        (output-file (car (asdf:output-files o c))))
    (multiple-value-bind (output warnings-p failure-p)
        (compile-file source-file :output-file output-file
                      #+sbcl #+sbcl :external-format :iso-8859-1)
      (when warnings-p
        (case (asdf:operation-on-warnings o)
          (:warn (warn
                  "~@<COMPILE-FILE warned while performing ~A on ~A.~@:>"
                  o c))
          (:error (error 'compile-warned :component c :operation o))
          (:ignore nil)))
      (when failure-p
        (case (asdf:operation-on-failure o)
          (:warn (warn
                  "~@<COMPILE-FILE failed while performing ~A on ~A.~@:>"
                  o c))
          (:error (error 'compile-failed :component c :operation o))
          (:ignore nil)))
      (unless output
        (error 'asdf:compile-error :component c :operation o)))))
(defmethod perform ((o asdf:load-source-op) (c iso-8859-1-file))
  ;; likewise, have to reimplement rather than closily extend
  (let ((source (asdf:component-pathname c)))
    (setf (asdf:component-property c 'asdf::last-loaded-as-source)
          (and (load source #+sbcl #+sbcl :external-format :iso-8859-1)
               (get-universal-time)))))

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :ttyhemlock
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:hemlock)
    :components
    ((:module tty-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :components
              ((:file "termcap")
               (:file "terminfo")
               (:file "tty-disp-rt")
               (:file "tty-display")))
     (:module tty-2
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :components
              ((:file "tty-screen")
               (:file "tty-stuff")
               (:file "tty-input")))))
