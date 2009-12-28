;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(proclaim '(optimize (safety 3) (speed 0) (debug 3)))

(defpackage #:hemlock-system
  (:use #:cl)
  (:export #:*hemlock-base-directory* #:hemlock-file))

(in-package #:hemlock-system)

(defclass hemlock-file (asdf:cl-source-file) ())
(defmethod asdf:perform ((o asdf:compile-op) (c hemlock-file))
  ;; Darn.  Can't just CALL-NEXT-METHOD; have to reimplement the
  ;; world.
  (let ((source-file (asdf:component-pathname c))
        (output-file (car (asdf:output-files o c))))
    (multiple-value-bind (output warnings-p failure-p)
        (compile-file source-file :output-file output-file
                      #+sbcl #+sbcl :external-format :iso-8859-1
                      #+clisp #+clisp :external-format (ext:make-encoding :charset 'charset:iso-8859-1 :line-terminator :unix))
      (when warnings-p
        (warn
         "~@<COMPILE-FILE warned while performing ~A on ~A.~@:>"
         o c))
      (when failure-p
        (warn
         "~@<COMPILE-FILE failed while performing ~A on ~A.~@:>"
         o c))
      (unless output
        (error 'asdf:compile-error :component c :operation o)))))

(defmethod perform ((o asdf:load-source-op) (c hemlock-file))
  ;; likewise, have to reimplement rather than closily extend
  (let ((source (asdf:component-pathname c)))
    (setf (asdf:component-property c 'asdf::last-loaded-as-source)
          (and (load source
                     #+sbcl #+sbcl :external-format :iso-8859-1
                     #+clisp #+clisp :external-format (ext:make-encoding :charset 'charset:iso-8859-1 :line-terminator :unix))
               (get-universal-time)))))

(pushnew :command-bits *features*)
(pushnew :buffered-lines *features*)

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(defparameter *binary-pathname*
  (make-pathname :directory
                 (append (pathname-directory *hemlock-base-directory*)
                         (list "bin"
                               #+CLISP "clisp"
                               #+CMU   "cmu"
                               #+EXCL  "acl"
                               #+SBCL  "sbcl"
                               #-(or CLISP CMU EXCL SBCL)
                               (string-downcase (lisp-implementation-type))))
                 :defaults *hemlock-base-directory*))

(asdf:defsystem :hemlock.base
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:bordeaux-threads
                  :trivial-gray-streams
                  :iterate
                  :prepl
                  :osicat
                  :iolib
                  :iolib.os
                  :cl-ppcre)
    :components
    ((:module core-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on (wire)
              :components
              ((:file "package")
               ;; Lisp implementation specific stuff goes into one of the next
               ;; two files.
               (:file "lispdep" :depends-on ("package"))
               (:file "hemlock-ext" :depends-on ("package"))

               (:file "decls" :depends-on ("package")) ; early declarations of functions and stuff
               (:file "struct" :depends-on ("package"))
               #+port-core-struct-ed (:file "struct-ed" :depends-on ("package"))
               (hemlock-system:hemlock-file "charmacs" :depends-on ("package"))
               (:file "key-event" :depends-on ("package" "charmacs"))))
     (:module bitmap-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on (core-1)
              :components
              ((:file "keysym-defs") ; hmm.
               (:file "bit-stuff") ; input depends on it --amb
               (:file "hunk-draw"))) ; window depends on it --amb
     (:module core-2
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on (bitmap-1 core-1)
              :serial t                 ;...
              :components
              ((:file "rompsite")
               (:file "input")
               (:file "macros")
               (:file "line")
               (:file "ring")
               (:file "htext1") ; buffer depends on it --amb
               (:file "buffer")
               (:file "vars")
               (:file "interp")
               (:file "syntax")
               (:file "htext2")
               (:file "htext3")
               (:file "htext4")
               (:file "files")
               (:file "search1")
               (:file "search2")
               (:file "table")

               (:file "winimage")
               (:file "window")
               (:file "screen")
               (:file "linimage")
               (:file "cursor")
               (:file "display")
               (:file "exp-syntax")))
     (:module root-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on (core-2 core-1)
              :components
              ((:file "pop-up-stream")))
     (:module root-2
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on (root-1 core-1 wire)
              :components
              ((:file "font")
               (:file "streams")
               #+port-root-hacks (:file "hacks")
               (:file "main")
               (:file "echo")
               (:file "new-undo")))
     (:module core-3
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on (bitmap-1 core-1)
              :components
              ((:file "typeout")))
     (:module wire
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on ()
              :serial t
              :components
              ((:file "wire-package")
               (:file "port")
               (:file "wire")
               (:file "remote")))
     (:module user-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on (root-2 core-1 wire)
              :components
              ((:file "echocoms")

               (:file "command")
               (:file "kbdmac")
               (:file "undo")
               (:file "killcoms")
               (:file "indent" :depends-on ("filecoms"))
               (:file "searchcoms")
               (:file "filecoms")
               (:file "grep" :depends-on ("filecoms"))
               (:file "apropos" :depends-on ("filecoms"))
               (:file "morecoms")
               (:file "doccoms")
               (:file "srccom")
               (:file "group")
               (:file "fill")
               (:file "text")

               (:file "lispmode")
               (:file "ts-buf")
               (:file "ts-stream")
               (:file "request")
               (:file "eval-server")
               (:file "lispbuf" :depends-on ("filecoms"))
               (:file "lispeval" :depends-on ("eval-server"))
               #+port-user-spell-rt (:file "spell-rt")
               #+port-user-spell-corr (:file "spell-corr")
               #+port-user-spell-aug (:file "spell-aug")
               #+port-user-spellcoms (:file "spellcoms")

               (:file "comments")
               (:file "overwrite")
               (:file "abbrev")
               (:file "icom")
               (:file "defsyn")
               (:file "scribe")
               (:file "pascal")
               (:file "dylan")

               (:file "edit-defs")
               (:file "auto-save")
               (:file "register")
               (:file "xcoms")
               #+port-user-unixcoms (:file "unixcoms")
               #+port-user-mh (:file "mh")
               (:file "highlight")
               (:file "dired")
               (:file "diredcoms" :depends-on ("dired"))
               (:file "bufed")
               (:file "coned")
               (:file "xref")
               #+port-user-lisp-lib (:file "lisp-lib")
               (:file "completion")
               (:file "shell")
               (:file "debug")
               #+port-user-netnews (:file "netnews")
               #+port-user-rcs (:file "rcs")
               (:file "dabbrev")
               (:file "bindings")
               (:file "slave-list")))
     (:module bitmap-2
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on (user-1 core-1)
              :components
              ((:file "bitmap-rompsite")
               (:file "bitmap-input")
               #+hemlock-clx (:file "bit-screen")
               #+hemlock-clx (:file "bit-display")
               (:file "bitmap-pop-up-stream")))
     (:module qthemlock
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on (bitmap-2 core-1)
              :components
              ((:file "connections")
               (:file "clbuild")))))
