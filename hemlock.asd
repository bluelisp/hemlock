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

(asdf:defsystem :hemlock
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:qt :bordeaux-threads :usocket :qt-repl
                      :trivial-gray-streams #+sbcl :sb-posix :iterate
                      :prepl)
    :components
    ((:module core-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :components
              ((:file "package")
               ;; Lisp implementation specific stuff goes into one of the next
               ;; two files.
               (:file "lispdep" :depends-on ("package"))
               (:file "hemlock-ext" :depends-on ("package"))

               (:file "decls" :depends-on ("package")) ; early declarations of functions and stuff
               (:file "struct" :depends-on ("package"))
               #+port-core-struct-ed (:file "struct-ed" :depends-on ("package"))
               (hemlock-system:iso-8859-1-file "charmacs" :depends-on ("package"))
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
               (:file "display")))
     (:module tty-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :components
              (#+want-tty-hemlock (:file "termcap")
               #+want-tty-hemlock (:file "tty-disp-rt")
               #+want-tty-hemlock (:file "tty-display")))
     (:module root-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :depends-on (core-2 core-1)
              :components
              ((:file "pop-up-stream")))
     (:module tty-2
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :components
              (#+want-tty-hemlock (:file "tty-screen")
               #+want-tty-hemlock (:file "tty-stuff")
               #+want-tty-hemlock (:file "tty-input")))
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
               (:file "indent")
               (:file "searchcoms")
               (:file "filecoms")
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
               (:file "diredcoms")
               (:file "bufed")
               (:file "coned")
               #+port-user-lisp-lib (:file "lisp-lib")
               (:file "completion")
               (:file "shell")
               #+port-user-debug (:file "debug")
               #+port-user-netnews (:file "netnews")
               #+port-user-rcs (:file "rcs")
               (:file "dabbrev")
               (:file "bindings")))
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
              ((:file "qt")
               (:file "browser" :depends-on ("qt"))
               (:file "connections" :depends-on ("qt"))
               (:file "sugiyama" :depends-on ("qt"))
               (:file "graphics" :depends-on ("sugiyama"))
               (:file "clbuild" :depends-on ("graphics"))))))
