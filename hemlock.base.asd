;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(proclaim '(optimize (safety 3) (speed 0) (debug 3)))

(defpackage #:hemlock-system
  (:use #:cl)
  (:export #:*hemlock-base-directory*))

(in-package #:hemlock-system)

(defvar *modern-hemlock* nil)
#+cmu (let ((packages (remove-if-not #'find-package
                                     '(:hemlock :hemlock-internals :wire))))
        (when (and packages (not *modern-hemlock*))
          (cerror "Continue and delete the old packages"
                  "It looks like you're trying to load a modern version of ~
                 Hemlock into a CMUCL image that already has an old Hemlock ~
                 present.  Hit the restart to replace all old packages.")
          (mapc #'delete-package packages)))
(setf *modern-hemlock* t)

(pushnew :command-bits *features*)

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
                               #+scl   "scl"
                               #-(or CLISP CMU EXCL SBCL scl)
                               (string-downcase (lisp-implementation-type))))
                 :defaults *hemlock-base-directory*))

(asdf:defsystem :hemlock.base
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (#-scl :alexandria
                  :bordeaux-threads
                  :conium
                  :trivial-gray-streams
                  :iterate
                  :prepl
                  :osicat
                  :iolib
                  :iolib.os
                  :cl-ppcre
                  #-scl :command-line-arguments)
    :components
    ((:module core-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src" "core"))
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
               (:file "charmacs" :depends-on ("package"))
               (:file "key-event" :depends-on ("package" "charmacs"))
               ))
     (:module bitmap-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src" "bitmap"))
                           *hemlock-base-directory*)
              :depends-on (core-1)
              :components
              ((:file "keysym-defs") ; hmm.
               ))
     (:module core-2
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src" "core"))
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
               (:file "exp-syntax")
               (:file "connections")
               (:file "repl" :depends-on ("macros" "rompsite" "connections"))))
     (:module root-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src" "root"))
                           *hemlock-base-directory*)
              :depends-on (core-2 core-1)
              :components
              ((:file "pop-up-stream")))
     (:module root-2
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src" "root"))
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
                            :directory '(:relative "src" "core"))
                           *hemlock-base-directory*)
              :depends-on (bitmap-1 core-1 core-2)
              :components
              ((:file "typeout")))
     (:module wire
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src" "wire"))
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
                            :directory '(:relative "src" "user"))
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
               (:file "spell-rt")
               (:file "spell-corr" :depends-on ("spell-rt"))
               (:file "spell-aug" :depends-on ("spell-corr"))
               (:file "spellcoms" :depends-on ("spell-aug" "filecoms"))
               (:file "spell-build" :depends-on ("spell-aug"))

               (:file "comments")
               (:file "overwrite")
               #+nil ; port to new event model.
               (:file "abbrev")
               (:file "icom")
               (:file "defsyn")
               (:file "scribe")
               (:file "pascal")
               (:file "dylan" :depends-on ("filecoms"))

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
               (:file "completion" :depends-on ("lispmode"))
               (:file "cpc")
               (:file "fuzzy" :depends-on ("cpc"))
               (:file "shell")
               (:file "debug")
               #+port-user-netnews (:file "netnews")
               #+port-user-rcs (:file "rcs")
               (:file "dabbrev")
               (:file "bindings")
               ;; (:file "vim-bindings")
               ;; (:file "vim-normal")
               ;; (:file "vim-insert")
               ;; (:file "vim-cmdline")
               (:file "slave-list")))
     (:module misc-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src" "misc"))
                           *hemlock-base-directory*)
              :depends-on (core-1)
              :components
              ((:file "clbuild")))))
