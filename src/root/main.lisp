;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Hemlock initialization code and random debugging stuff.
;;;
;;; Written by Bill Chiles and Rob MacLachlan
;;;

(in-package :hemlock-internals)

#||
GB
(in-package :extensions)
(export '(save-all-buffers *hemlock-version*))
(in-package :hemlock-internals)
||#



;;;; Definition of *hemlock-version*.

(defparameter *hemlock-version*
  (let* ((system (asdf:find-system :hemlock.base))
         (dir (asdf:component-pathname system))
         (.git (merge-pathnames ".git/" dir))
         (ref (with-open-file (s (merge-pathnames "HEAD" .git)
                                 :if-does-not-exist nil)
                (and s (subseq (read-line s) 5)))))
    (if ref
        (with-open-file (s (merge-pathnames ref .git))
          (subseq (read-line s) 0 8))
        "4.unknown")))


;;; (pushnew :hemlock *features*)


;;;; %INIT-HEMLOCK.

(defvar *hemlock-initialized* nil)

(defun %init-hemlock ()
  "Initialize hemlock's internal data structures."
  ;;
  ;; This function is defined in Buffer.Lisp.  It creates fundamental mode
  ;; and the buffer main.  Until this is done it is not possible to define
  ;; or use Hemlock variables.
  (setup-initial-buffer)
  ;;
  ;; Define some of the system variables.
  (define-some-variables)
  ;;
  ;; Site initializations such as window system variables.
  (site-init)
  ;;
  ;; Set up syntax table data structures.
  (%init-syntax-table)
  ;;
  ;; Define print representations for funny characters.
  (%init-line-image)
  (setq *hemlock-initialized* t))


;;;; Define some globals.

;;; These globals cannot be defined in the appropriate file due to compilation
;;; or load time constraints.
;;;

;;; The following belong in other files, but those files are loaded before
;;; table.lisp which defines MAKE-STRING-TABLE.
;;;
;;; vars.lisp
(defvar *global-variable-names* (make-string-table)
  "A String Table of global variable names, the values are the symbol names.")
;;;
;;; buffer.lisp
(defvar *mode-names* (make-string-table) "A String Table of Mode names.")
(defvar *buffer-names* (make-string-table)
  "A String Table of Buffer names and their corresponding objects.")
;;;
;;; interp.lisp
(defvar *command-names* (make-string-table) "String table of command names.")
;;;
;;; syntax.lisp
(defvar *character-attribute-names* (make-string-table)
 "String Table of character attribute names and their corresponding keywords.")



;;;; DEFINE-SOME-VARIABLES.

;;; This is necessary to define "Default Status Line Fields" which belongs
;;; beside the other modeline variables.  This DEFVAR would live in
;;; Morecoms.Lisp, but it is compiled and loaded after this file.
;;;
(declaim (special hemlock::*recursive-edit-count*))
;;;
(make-modeline-field
 :name :edit-level :width 15
 :function #'(lambda (buffer window)
               (declare (ignore buffer window))
               (if (zerop hemlock::*recursive-edit-count*)
                   ""
                   (format nil "Edit Level: ~2,'0D "
                           hemlock::*recursive-edit-count*))))

;;; This is necessary to define "Default Status Line Fields" which belongs
;;; beside the other modeline variables.  This DEFVAR would live in
;;; Completion.Lisp, but it is compiled and loaded after this file.
;;;
(declaim (special hemlock::*completion-mode-possibility*))
;;; Hack for now until completion mode is added.
(defvar hemlock::*completion-mode-possibility* "")
;;;
(make-modeline-field
 :name :completion :width 40
 :function #'(lambda (buffer window)
               (declare (ignore buffer window))
               hemlock::*completion-mode-possibility*))


(defun define-some-variables ()
  (defhvar "Default Modes"
    "This variable contains the default list of modes for new buffers."
    :value '("Fundamental" "Save"))
  (defhvar "Echo Area Height"
    "Number of lines in the echo area window."
    :value 3)
  (defhvar "Make Buffer Hook"
    "This hook is called with the new buffer whenever a buffer is created.")
  (defhvar "Delete Buffer Hook"
    "This hook is called with the buffer whenever a buffer is deleted.")
  (defhvar "Enter Recursive Edit Hook"
    "This hook is called with the new buffer when a recursive edit is
     entered.")
  (defhvar "Exit Recursive Edit Hook"
    "This hook is called with the value returned when a recursive edit
     is exited.")
  (defhvar "Abort Recursive Edit Hook"
    "This hook is called with the editor-error args when a recursive
     edit is aborted.")
  (defhvar "Buffer Major Mode Hook"
    "This hook is called with the buffer and the new mode when a buffer's
     major mode is changed.")
  (defhvar "Buffer Minor Mode Hook"
    "This hook is called a minor mode is changed.  The arguments are
     the buffer, the mode affected and T or NIL depending on when the
     mode is being turned on or off.")
  (defhvar "Buffer Writable Hook"
    "This hook is called whenever someone sets whether the buffer is
     writable.")
  (defhvar "Buffer Name Hook"
    "This hook is called with the buffer and the new name when the name of a
     buffer is changed.")
  (defhvar "Buffer Pathname Hook"
    "This hook is called with the buffer and the new Pathname when the Pathname
     associated with the buffer is changed.")
  (defhvar "Buffer Modified Hook"
    "This hook is called whenever a buffer changes from unmodified to modified
     and vice versa.  It takes the buffer and the new value for modification
     flag.")
  (defhvar "Set Buffer Hook"
    "This hook is called with the new buffer when the current buffer is set.")
  (defhvar "After Set Buffer Hook"
    "This hook is invoked with the old buffer after the current buffer has
     been changed.")
  (defhvar "Set Window Hook"
    "This hook is called with the new window when the current window
     is set.")
  (defhvar "Make Window Hook"
    "This hook is called with a new window when one is created.")
  (defhvar "Delete Window Hook"
    "This hook is called with a window before it is deleted.")
  (defhvar "Window Buffer Hook"
    "This hook is invoked with the window and new buffer when a window's
     buffer is changed.")
  (defhvar "Delete Variable Hook"
    "This hook is called when a variable is deleted with the args to
     delete-variable.")
  (defhvar "Entry Hook"
    "this hook is called when the editor is entered.")
  (defhvar "Exit Hook"
    "This hook is called when the editor is exited.")
  (defhvar "Redisplay Hook"
    "This is called on the current window from REDISPLAY after checking the
     window display start, window image, and recentering.  After calling the
     functions in this hook, we do the above stuff and call the smart
     redisplay method for the device."
    :value nil)
  (defhvar "Key Echo Delay"
    "Wait this many seconds before echoing keys in the command loop.  This
     feature is inhibited when nil."
    :value 1.0)
  (defhvar "Input Hook"
    "The functions in this variable are invoked each time a character enters
     Hemlock."
    :value nil)
  (defhvar "Abort Hook"
    "These functions are invoked when ^G is typed.  No arguments are passed."
    :value nil)
  (defhvar "Command Abort Hook"
    "These functions get called when commands are aborted, such as with
     EDITOR-ERROR."
    :value nil)
  (defhvar "Character Attribute Hook"
    "This hook is called with the attribute, character and new value
     when the value of a character attribute is changed.")
  (defhvar "Shadow Attribute Hook"
    "This hook is called when a mode character attribute is made.")
  (defhvar "Unshadow Attribute Hook"
    "This hook is called when a mode character attribute is deleted.")
  (defhvar "Default Modeline Fields"
    "The default list of modeline-fields for MAKE-WINDOW."
    :value *default-modeline-fields*)
  (defhvar "Default Status Line Fields"
    "This is the default list of modeline-fields for the echo area window's
     modeline which is used for general information."
    :value (list (make-modeline-field
                  :name :hemlock-banner
                  :function #'(lambda (buffer window)
                                (declare (ignore buffer window))
                                (format nil "Hemlock ~A on ~A "
                                        *hemlock-version*
                                        (lisp-implementation-type))))
                 (modeline-field :edit-level)
                 (modeline-field :completion)))
  (defhvar "Maximum Modeline Pathname Length"
    "When set, this variable is the maximum length of the display of a pathname
     in a modeline.  When the pathname is too long, the :buffer-pathname
     modeline-field function chops off leading directory specifications until
     the pathname fits.  \"...\" indicates a truncated pathname."
    :value 30
    :hooks (list 'maximum-modeline-pathname-length-hook)))

(defvar *background-image* :auto
  "Path to a background image in SVG format, or one of :AUTO, NIL.

   Possible values indicate:
     - STRING or PATHNAME -- Open this file name.

     - The symbol :AUTO --  Try to open ~/.hemlock/background.svg, then
       background.svg in Hemlock's installation directory (in this order).

     - The symbol NIL -- No background image.
       (Not using a background image is faster, especially with remote X.)

   Currently supported only in the Qt backend.")



;;;; ED.

(defvar *editor-has-been-entered* ()
  "True if and only if the editor has been entered.")
(defvar *in-the-editor* ()
  "True if we are inside the editor.  This is used to prevent ill-advised
   \"recursive\" edits.")

(defvar *after-editor-initializations-funs* nil
  "A list of functions to be called after the editor has been initialized upon
   entering the first time.")

(defmacro after-editor-initializations (&rest forms)
  "Causes forms to be executed after the editor has been initialized.
   Forms supplied with successive uses of this macro will be executed after
   forms supplied with previous uses."
  `(push #'(lambda () ,@forms)
         *after-editor-initializations-funs*))

#-(or cmu scl)
(defparameter *command-line-spec*
  (flet ((keywordize (sym value)
           (push (intern (string-upcase value) :keyword)
                 *command-line-options*)
           (push sym *command-line-options*))
         (quick-backend (sym *)
           (push sym *command-line-options*)
           (push :backend-type *command-line-options*)))
    `(("help"
       :type boolean
       :documentation "show this help")
      ("slave"
       ;(undocumented) start a slave?
       :type boolean)
      ("editor"
       ;(undocumented) if slave: connect to this editor
       :type string)
      (("backend" "backend-type")
       :type string
       :documentation "backend to use, one of tty, clx, or qt. If not specified, checks if $DISPLAY is set, and use the first available backend; without a $DISPLAY, falls back to TTY.  See also --tty et al."
       :action ,(alexandria:curry #'keywordize :backend-type))
      ,@(iter:iter (iter:for b in '(:tty :clx :qt))
                   (iter:collect
                    `(,(string-downcase b)
                      :type boolean
                      :documentation ,(format nil "short for --backend ~A" b)
                      :action ,(let ((b b))
                                 (alexandria:curry #'quick-backend b))))))))

#-(or cmu scl)
(defun show-cmd-line-help ()
  (format t "This is hemlock ~A.~%Usage:~%~%" *hemlock-version*)
  (format t "   ~A [OPTIONS] file...~%~%"
          (or #+sbcl (car sb-ext:*posix-argv*)
              "hemlock"))
  (format t "Options are:~%~%")
  (show-option-help *command-line-spec*)  )

;; The following function is copy&paste from the system
;; command-line-arguments.
;;
;; CHANGES:
;;   - do not override *print-right-margin* .
;;
;; Free Software available under an MIT-style license. See LICENSE
;; Copyright (c) 2003-2009 ITA Software, Inc.  All rights reserved.
;; Original author: Francois-Rene Rideau
#-(or cmu scl)
(defun show-option-help
       (specification &key (stream *standard-output*) sort-names)
  ;; TODO: be clever when trying to align stuff horizontally
  (loop :for spec :in specification :do
        (destructuring-bind (names &key negation documentation negation-documentation
                                   type optional list (initial-value nil initial-value-p) &allow-other-keys) spec
          (declare (ignorable negation documentation negation-documentation type optional list))
          (unless (consp names)
            (setf names (list names)))
          (flet ((option-names (names)
                   (let ((n (mapcar 'command-line-arguments::option-name names)))
                     (if sort-names
                       (stable-sort n #'< :key #'length)
                       n))))
            (when documentation
              (format stream "~& ~32A ~8A ~@<~@;~{~A ~}~@:>"
                      (format nil "~{ ~A~}" (option-names names))
                      (string-downcase type)
                      (cl-ppcre:split " " documentation))
              (format stream "~:[~*~; (default: ~S)~]~%" initial-value-p initial-value))
            (when negation-documentation
              (format stream " ~32A ~8A ~@<~@;~{~A ~}~@:>~%"
                      (format nil "~{ ~A~}" (option-names (command-line-arguments::make-negated-names names negation)))
                      (string-downcase type)
                      (cl-ppcre:split " " negation-documentation)))))))


#-(or cmu scl)
(defun main (&optional (arg-list (get-command-line-arguments)))
  (multiple-value-bind (keys rest)
                       (process-command-line-options
                        *command-line-spec*
                        arg-list)
    (destructuring-bind (&key slave help &allow-other-keys)
                        keys
      (cond
       (help
        (show-cmd-line-help)
        (force-output))
       (slave
        (assert (null rest))
        (apply #'hemlock:start-slave keys))
       (t
        (apply #'hemlock rest keys))))))

(defmacro with-editor ((&key (load-user-init t)
                             backend-type
                             display)
                       &body body)
  `(call-with-editor (lambda () ,@body)
                     :load-user-init ,load-user-init
                     :backend-type ,backend-type
                     :display ,display))

;;; This function is the user-visible entry point to the Hemlock editor.
;;; It calls out to CALL-WITH-EDITOR and COMMAND-LOOP to do the hard work.
;;;
;;; In addition to those, it runs entry and exit hooks.
;;;
;;; This function may also be when already in the editor, or when in a slave,
;;; and merely processes the command line argument in that case, allowing ED
;;; to work when already in Hemlock.
;;;
(defun hemlock (&optional x
                &key (load-user-init t)
                     backend-type
                     (display (hemlock-ext:getenv "DISPLAY")))
  "Invokes the editor, Hemlock.  If X is supplied and is a symbol, the
   definition of X is put into a buffer, and that buffer is selected.  If X is
   a pathname, the file specified by X is visited in a new buffer.  If X is not
   supplied or Nil, the editor is entered in the same state as when last
   exited.  When :init is supplied as t (the default), the file
   \"hemlock-init.lisp\", or \".hemlock-init.lisp\" is loaded from the home
   directory, but the Lisp command line switch -hinit can be used to specify a
   different name.  Any compiled version of the source is preferred when
   choosing the file to load.  If the argument is non-nil and not t, then it
   should be a pathname that will be merged with the home directory."
  (cond
    (*in-the-editor*
     (process-command-line-argument x))
    (*in-hemlock-slave-p*
     (hemlock.wire:remote-value (hemlock::ts-stream-wire *terminal-io*)
                                (process-command-line-argument x)))
    (t
     (with-editor (:load-user-init load-user-init
                   :backend-type backend-type
                   :display display)
       (process-command-line-argument x)
       (invoke-hook hemlock::entry-hook)
       (unwind-protect
            (command-loop)
         (invoke-hook hemlock::exit-hook))))))

(defun command-loop ()
  (let ((*standard-input* *illegal-read-stream*)
        (*query-io* *illegal-read-stream*))
    (loop
       (catch 'command-loop-catcher     ;fixme: isn't this redundant?
         (catch 'editor-top-level-catcher
           (handler-bind
               ((error #'(lambda (condition)
                           (lisp-error-error-handler condition :internal))))
             (invoke-hook hemlock::abort-hook)
             (%command-loop)))))))

(defvar *main-event-base* nil)

;;; This function does all the hard work for Hemlock initialization, in
;;; particular backend initialization.  It differs from the main function
;;; in that it lets the caller take control once initialization is done,
;;; instead of entering the command loop.
;;;
(defun call-with-editor
    (fun
     &key (load-user-init t) backend-type (display (hemlock-ext:getenv "DISPLAY")))
  (when *in-the-editor*
    (error "already in the editor"))
  (when (and backend-type (not (validate-backend-type backend-type)))
    (error "Specified backend ~A not loaded" backend-type))
  ;; fixme: pass DISPLAY to WITH-EVENT-LOOP, so that Qt can pick it up
  ;; in case the user wants a DISPLAY != $DISPLAY
  (let* ((backend-type (or (validate-backend-type backend-type)
                           (choose-backend-type display)))
         (*default-backend* (car backend-type)))
    (setf *connection-backend* (cdr backend-type))
    (with-existing-event-loop
        (or *main-event-base*
            (setf *main-event-base*
                  (make-event-loop *connection-backend*)))
      (let* ((*in-the-editor* t)
             (display (unless *editor-has-been-entered*
                        (maybe-load-hemlock-init load-user-init)
                        ;; Device dependent initializaiton.
                        (init-raw-io (car backend-type) display))))
        (catch 'editor-top-level-catcher
          (site-wrapper-macro
            (unless *editor-has-been-entered*
              ;; Make an initial window, and set up redisplay's internal
              ;; data structures.
              (%init-redisplay (car backend-type) display)
              (setq *editor-has-been-entered* t)
              ;; Pick up user initializations to be done after initialization.
              (invoke-hook (reverse *after-editor-initializations-funs*)))
            (catch 'hemlock-exit
              (funcall fun))))))))

(defun process-command-line-argument (x)
  (catch 'editor-top-level-catcher
    (process-command-line-argument/2 x)))

(defun process-command-line-argument/2 (x)
  (typecase x
    (list
     (mapc #'process-command-line-argument/2 x))
    (symbol
     (let* ((name (nstring-capitalize
                   (concatenate 'simple-string "Edit " (string x))))
            (buffer (or (getstring name *buffer-names*)
                        (make-buffer name)))
            (*print-case* :downcase))
       (delete-region (buffer-region buffer))
       (with-output-to-mark
           (*standard-output* (buffer-point buffer))
         (eval `(grindef ,x))   ; hackish, I know...
         (terpri)
         (hemlock::change-to-buffer buffer)
         (buffer-start (buffer-point buffer)))))
    ((or string pathname)
     (hemlock::find-file-command () x))
    (t
     (error
      "~S is not a symbol or pathname.  I can't edit it!" x))))

(defvar *in-hemlock-slave-p* nil)

(defun hemlock-ed-function (&optional x)
  (hemlock x)
  t)

#+sbcl
(pushnew 'hemlock-ed-function sb-ext:*ed-functions*)

#+ccl
(unless (eq ccl:*resident-editor-hook* 'hemlock-ed-function)
  (if ccl:*resident-editor-hook*
      (warn "*resident-editor-hook* already bound, not installing hemlock")
      (setf ccl:*resident-editor-hook* 'hemlock-ed-function)))

#+allegro
(unless (eq scm::*ed-hook* 'hemlock-ed-function)
  (if scm::*ed-hook*
      (warn "*ed-hook* already bound, not installing hemlock")
      (setf scm::*ed-hook* 'hemlock-ed-function)))

(defun maybe-load-hemlock-init (init)
  (when init
    (let ((names
           (if (typep init '(or string pathname))
               (list init)
               (let ((home (user-homedir-pathname)))
                 (list (merge-pathnames ".hemlock.lisp" home)
                       (merge-pathnames ".hemlock/hemlock.lisp" home)
                       ;; Also support one of the traditional pathnames for
                       ;; CMUCL compatibility:
                       (merge-pathnames ".hemlock-init.lisp" home))))))
      (dolist (name names)
        (when (probe-file name)
          (load name :verbose t)
          (return))))))


;;;; SAVE-ALL-BUFFERS.

;;; SAVE-ALL-BUFFERS -- Public.
;;;
(defun save-all-buffers (&optional (list-unmodified-buffers nil))
  "This prompts users with each modified buffer as to whether they want to
   write it out.  If the buffer has no associated file, this will also prompt
   for a file name.  Supplying the optional argument non-nil causes this
   to prompt for every buffer."
  (dolist (buffer *buffer-list*)
    (when (or list-unmodified-buffers (buffer-modified buffer))
      (maybe-save-buffer buffer))))

(defun maybe-save-buffer (buffer)
  (let* ((modified (buffer-modified buffer))
         (pathname (buffer-pathname buffer))
         (name (buffer-name buffer))
         (string (if pathname (namestring pathname))))
    (format t "Buffer ~S is ~:[UNmodified~;modified~], Save it? "
            name modified)
    (force-output)
    (when (y-or-n-p)
      (let ((name (read-line-default "File to write" string)))
        (format t "Writing file ~A..." name)
        (force-output)
        (write-file (buffer-region buffer) name)
        (write-line "write WON")))))

(defun read-line-default (prompt default)
  (format t "~A:~@[ [~A]~] " prompt default)
  (force-output)
  (do ((result (read-line) (read-line)))
      (())
    (declare (simple-string result))
    (when (plusp (length result)) (return result))
    (when default (return default))
    (format t "~A:~@[ [~A]~] " prompt default)
    (force-output)))

;; Hmm, what is this?
;;
;; This is highly bogus, but moving this to runtime fails also since
;; DEFMODE winds up wanting to refer to *current-buffer*. I don't know
;; why, and I don't know if we can repair it.
;; --GB

(unless *hemlock-initialized*
  (%init-hemlock))

(defvar *installation-directory* nil)

(defun installation-directory ()
  (or *installation-directory*
      (asdf:component-pathname (asdf:find-system :hemlock.base))))
