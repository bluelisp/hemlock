;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; **********************************************************************
;;;
;;; "Site dependent" stuff for the editor while on the IBM RT PC machine.
;;;

(in-package :hi)


;;;; SITE-INIT.

;;; *key-event-history* is defined in input.lisp, but it needs to be set in
;;; SITE-INIT, since MAKE-RING doesn't exist at load time for this file.
;;;
(declaim (special *key-event-history*))

;;; SITE-INIT  --  Internal
;;;
;;;    This function is called at init time to set up any site stuff.
;;;

(defun site-init ()
  (defhvar "Beep Border Width"
    "Width in pixels of the border area inverted by beep."
    :value 20)
  (defhvar "Default Window Width"
    "This is used to make a window when prompting the user.  The value is in
     characters."
    :value 80)
  (defhvar "Default Window Height"
    "This is used to make a window when prompting the user.  The value is in
     characters."
    :value 24)
  (defhvar "Default Initial Window Width"
    "This is used when Hemlock first starts up to make its first window.
     The value is in characters."
    :value 80)
  (defhvar "Default Initial Window Height"
    "This is used when Hemlock first starts up to make its first window.
     The value is in characters."
    :value 24)
  (defhvar "Default Initial Window X"
    "This is used when Hemlock first starts up to make its first window.
     The value is in pixels."
    :value nil)
  (defhvar "Default Initial Window Y"
    "This is used when Hemlock first starts up to make its first window.
     The value is in pixels."
    :value nil)
  (defhvar "Bell Style"
    "This controls what beeps do in Hemlock.  Acceptable values are :border-flash
     (which is the default), :feep, :border-flash-and-feep, :flash,
     :flash-and-feep, and NIL (do nothing)."
    :value :border-flash)
  #||
  ;; ###
  (defhvar "Reverse Video"
    "Paints white on black in window bodies, black on white in modelines."
    :value nil
    :hooks '(reverse-video-hook-fun))
  ||#
  (defhvar "Cursor Bitmap File"
    "File to read to setup cursors for Hemlock windows.  The mask is found by
     merging this name with \".mask\"."
    :value (make-pathname :name "hemlock11" :type "cursor"
                          :defaults (merge-pathnames
                                     "resources/"
                                     hemlock-system:*hemlock-base-directory*)))
  (defhvar "Enter Window Hook"
    "When the mouse enters an editor window, this hook is invoked.  These
     functions take the Hemlock Window as an argument."
    :value nil)
  (defhvar "Exit Window Hook"
    "When the mouse exits an editor window, this hook is invoked.  These
     functions take the Hemlock Window as an argument."
    :value nil)
  (defhvar "Set Window Autoraise"
    "When non-nil, setting the current window will automatically raise that
     window via a function on \"Set Window Hook\".  If the value is :echo-only
     (the default), then only the echo area window will be raised
     automatically upon becoming current."
    :value :echo-only)
  (defhvar "Default Font"
    "The string name of the font to be used for Hemlock -- buffer text,
     modelines, random typeout, etc.  The font is loaded when initializing
     Hemlock."
    :value "*-courier-medium-r-normal--*-120-*")
  (defhvar "Active Region Highlighting Font"
    "The string name of the font to be used for highlighting active regions.
     The font is loaded when initializing Hemlock."
    :value "*-courier-medium-o-normal--*-120-*")
  (defhvar "Open Paren Highlighting Font"
    "The string name of the font to be used for highlighting open parens.
     The font is loaded when initializing Hemlock."
    :value "*-courier-bold-r-normal--*-120-*")
  (defhvar "Thumb Bar Meter"
    "When non-nil (the default), windows will be created to be displayed with
     a ruler in the bottom border of the window."
    :value t)

  (setf *key-event-history* (make-ring 60))
  nil)


;;;; Some generally useful file-system functions.

;;; MERGE-RELATIVE-PATHNAMES takes a pathname that is either absolute or
;;; relative to default-dir, merging it as appropriate and returning a definite
;;; directory pathname.
;;;
;;; This function isn't really needed anymore now that merge-pathnames does
;;; this, but the semantics are slightly different.  So it's easier to just
;;; keep this around instead of changing all the uses of it.
;;;
(defun merge-relative-pathnames (pathname default-directory)
  "Merges pathname with default-directory.  If pathname is not absolute, it
   is assumed to be relative to default-directory.  The result is always a
   directory."
  (let ((pathname (merge-pathnames pathname default-directory)))
    (if (directoryp pathname)
        pathname
        (pathname (concatenate 'simple-string
                               (namestring pathname)
                               "/")))))

(defun directoryp (pathname)
  "Returns whether pathname names a directory, that is whether it has no
   name and no type components."
  (not (or (pathname-name pathname) (pathname-type pathname))))



;;;; I/O specials and initialization

;;; This is a hack, so screen can tell how to initialize screen management
;;; without re-opening the display.  It is set in INIT-RAW-IO and referenced
;;; in WINDOWED-MONITOR-P.
;;;
(defvar *editor-windowed-input* nil)

;;; These are used for selecting X events.
(declaim (special *editor-input* *real-editor-input*))

(declaim (special *editor-input* *real-editor-input*))

;;; INIT-RAW-IO  --  Internal
;;;
;;;    This function should be called whenever the editor is entered in a new
;;; lisp.  It sets up process specific data structures.
;;;
(defvar *default-backend* nil) ;if not set, use the CAR of available ones

;;; Each backend type is a cons of the type and the connection backend
;;; This could be turned into a struct or something in the future. NJP
(defvar *available-backends* '())

(defun validate-backend-type (want)
  (find want *available-backends* :key #'car))

(defun choose-backend-type (&optional display)
  (let ((want
         (if display
             ;; $DISPLAY is set, can use the preferred display
             *default-backend*
             ;; $DISPLAY unset, revert to TTY
             :tty)))
    (cond
      ((validate-backend-type want))
      ((car *available-backends*))
      (t (error "no Hemlock backends loaded, giving up")))))

(defgeneric backend-init-raw-io (backend-type display))

(defun init-raw-io (backend-type display)
  (setf *editor-windowed-input* nil)
  (backend-init-raw-io backend-type display))

;;; Stop flaming from compiler due to CLX macros expanding into illegal
;;; declarations.
;;;
(declaim (special *default-font-family*))

;;; font-map-size should be defined in font.lisp, but SETUP-FONT-FAMILY would
;;; assume it to be special, issuing a nasty warning.
;;;
#+clx
(defconstant font-map-size 16
  "The number of possible fonts in a font-map.")
#-clx
(defconstant font-map-size 16)


;;;; HEMLOCK-BEEP.

(defvar *editor-bell* (make-string 1 :initial-element #\bell))

;;; TTY-BEEP is used in Hemlock for beeping when running under a terminal.
;;; Send a #\bell to unix standard output.
;;;
#+NIL
(defun tty-beep (&optional device stream)
  (declare (ignore device stream))
  (when (variable-value 'hemlock::bell-style)
    (unix:unix-write 1 *editor-bell* 0 1)))

(declaim (special *current-window*))

#+clx
(declaim (special *foreground-background-xor*))

(defun hemlock-beep (stream)
  "Using the current window, calls the device's beep function on stream."
  (let ((device (device-hunk-device (window-hunk (current-window)))))
    (device-beep device stream)))


;;; *BEEP-FUNCTION* and BEEP are in SYSTEM package in CMUCL.
;;;
(defvar *beep-function* #'(lambda (&optional stream)
                            (declare (ignorable stream))
                            (print "BEEP!" *trace-output*)
                            (finish-output *trace-output*)))

(defun beep (&optional (stream *terminal-io*))
  (funcall *beep-function* stream))


;;;; GC messages.

;;; HEMLOCK-GC-NOTIFY-BEFORE and HEMLOCK-GC-NOTIFY-AFTER both MESSAGE GC
;;; notifications when Hemlock is not running under X11.  It cannot affect
;;; its window's without using its display connection.  Since GC can occur
;;; inside CLX request functions, using the same display confuses CLX.
;;;

(defun hemlock-gc-notify-before (bytes-in-use)
  (let ((control "~%[GC threshold exceeded with ~:D bytes in use.  ~
                  Commencing GC.]~%"))
    (cond ((not hi::*editor-windowed-input*)
           (beep)
           #|(message control bytes-in-use)|#)
          (t
           ;; Can't call BEEP since it would use Hemlock's display connection.
           #+nil (lisp::default-beep-function *standard-output*)
           (format t control bytes-in-use)
           (finish-output)))))

(defun hemlock-gc-notify-after (bytes-retained bytes-freed trigger)
  (let ((control
         "[GC completed with ~:D bytes retained and ~:D bytes freed.]~%~
          [GC will next occur when at least ~:D bytes are in use.]~%"))
    (cond ((not hi::*editor-windowed-input*)
           (beep)
           #|(message control bytes-retained bytes-freed)|#)
          (t
           ;; Can't call BEEP since it would use Hemlock's display connection.
           #+nil (lisp::default-beep-function *standard-output*)
           (format t control bytes-retained bytes-freed trigger)
           (finish-output)))))



;;;; Site-Wrapper-Macro and standard device init/exit functions.

(defun in-hemlock-standard-input-read (stream &rest ignore)
  (declare (ignore ignore))
  (error "You cannot read off this stream while in Hemlock -- ~S"
         stream))

(defvar *illegal-read-stream*
  #+CMU (lisp::make-lisp-stream :in #'in-hemlock-standard-input-read)
  #-CMU (make-broadcast-stream))

(declaim (special *gc-notify-before*
                  *gc-notify-after*))

;; fixme: this is neither site-specific nor should it be a macro.
(defmacro site-wrapper-macro (&body body)
  `(unwind-protect
     (progn
       (when *editor-has-been-entered*
         (let ((device (device-hunk-device (window-hunk (current-window)))))
            (device-init device)))
       (let ((*beep-function* #'hemlock-beep)
             (*gc-notify-before* #'hemlock-gc-notify-before)
             (*gc-notify-after* #'hemlock-gc-notify-after))
         (cond ((not *editor-windowed-input*)
                ,@body)
               (t
                (hemlock-ext:with-clx-event-handling
                    (*editor-windowed-input* #'hemlock-ext:object-set-event-handler)
                  ,@body)))))
     (let ((device (device-hunk-device (window-hunk (current-window)))))
       (device-exit device))))

(declaim (special *echo-area-window*))


;;;; Line Wrap Char.

(defvar *line-wrap-char* #\!
  "The character to be displayed to indicate wrapped lines.")


;;;; Event scheduling.

;;; The time queue provides a ROUGH mechanism for scheduling events to
;;; occur after a given amount of time has passed, optionally repeating
;;; using the given time as an interval for rescheduling.  When the input
;;; loop goes around, it will check the current time and process all events
;;; that should have happened before or at this time.  The function gets
;;; called on the number of seconds that have elapsed since it was last
;;; called.
;;;
;;; NEXT-SCHEDULED-EVENT-WAIT and INVOKE-SCHEDULED-EVENTS are used in the
;;; editor stream in methods.
;;;
;;; SCHEDULE-EVENT and REMOVE-SCHEDULED-EVENT are exported interfaces.

(defstruct (tq-event (:print-function print-tq-event)
                     (:constructor make-tq-event
                                   (time last-time interval function)))
  time          ; When the event should happen.
  last-time     ; When the event was scheduled.
  interval      ; When non-nil, how often the event should happen.
  function)     ; What to do.

(defun print-tq-event (obj stream n)
  (declare (ignore n))
  (format stream "#<Tq-Event ~S>" (tq-event-function obj)))

(defvar *time-queue* nil
  "This is the time priority queue used in Hemlock input streams for event
   scheduling.")

;;; QUEUE-TIME-EVENT inserts event into the time priority queue *time-queue*.
;;; Event is inserted before the first element that it is less than (which
;;; means that it gets inserted after elements that are the same).
;;; *time-queue* is returned.
;;;
(defun queue-time-event (event)
  (let ((time (tq-event-time event)))
    (if *time-queue*
        (if (< time (tq-event-time (car *time-queue*)))
            (push event *time-queue*)
            (do ((prev *time-queue* rest)
                 (rest (cdr *time-queue*) (cdr rest)))
                ((or (null rest)
                     (< time (tq-event-time (car rest))))
                 (push event (cdr prev))
                 *time-queue*)))
        (push event *time-queue*))))

;;; NEXT-SCHEDULED-EVENT-WAIT returns nil or the number of seconds to wait for
;;; the next event to happen.
;;;
(defun next-scheduled-event-wait ()
  (if *time-queue*
      (let ((wait (round (- (tq-event-time (car *time-queue*))
                            (get-internal-real-time))
                         internal-time-units-per-second)))
        (if (plusp wait) wait 0))))

;;; INVOKE-SCHEDULED-EVENTS invokes all the functions in *time-queue* whose
;;; time has come.  If we run out of events, or there are none, then we get
;;; out.  If we popped an event whose time hasn't come, we push it back on the
;;; queue.  Each function is called on how many seconds, roughly, went by since
;;; the last time it was called (or scheduled).  If it has an interval, we
;;; re-queue it.  While invoking the function, bind *time-queue* to nothing in
;;; case the event function tries to read off *editor-input*.
;;;
(defun invoke-scheduled-events ()
  (let ((time (get-internal-real-time)))
    (loop
      (unless *time-queue* (return))
      (let* ((event (car *time-queue*))
             (event-time (tq-event-time event)))
        (cond ((>= time event-time)
               (let ((*time-queue* nil))
                 (funcall (tq-event-function event)
                          (round (- time (tq-event-last-time event))
                                 internal-time-units-per-second)))
               (hemlock-ext:without-interrupts
                (let ((interval (tq-event-interval event)))
                  (when interval
                    (setf (tq-event-time event) (+ time interval))
                    (setf (tq-event-last-time event) time)
                    (pop *time-queue*)
                    (queue-time-event event)))))
              (t (return)))))))

(defun schedule-event (time function &optional (repeat t))
  "This causes function to be called after time seconds have passed,
   optionally repeating every time seconds.  This is a rough mechanism
   since commands can take an arbitrary amount of time to run; the function
   will be called at the first possible moment after time has elapsed.
   Function takes the time that has elapsed since the last time it was
   called (or since it was scheduled for the first invocation)."
  (let ((now (get-internal-real-time))
        (itime (* internal-time-units-per-second time)))
    (queue-time-event (make-tq-event (+ itime now) now (if repeat itime)
                                     function))))

(defun remove-scheduled-event (function)
  "Removes function queued with SCHEDULE-EVENT."
  (setf *time-queue* (delete function *time-queue* :key #'tq-event-function)))



;;;; Showing a mark.

(defun show-mark (mark window time)
  "Highlights the position of Mark within Window for Time seconds,
   possibly by moving the cursor there.  If Mark is not displayed within
   Window return NIL.  The wait may be aborted if there is pending input."
  (let* ((result t))
    (catch 'redisplay-catcher
      (redisplay-window window)
      (setf result
            (multiple-value-bind (x y) (mark-to-cursorpos mark window)
              (device-show-mark (device-hunk-device (window-hunk window))
                                window x y time))))
    result))


;;;; Function description and defined-from.

;;; FUN-DEFINED-FROM-PATHNAME takes a symbol or function object.  It
;;; returns a pathname for the file the function was defined in.  If it was
;;; not defined in some file, then nil is returned.
;;;
#-(or cmu scl)
(defun fun-defined-from-pathname (function)
  "Takes a symbol or function and returns the pathname for the file the
   function was defined in.  If it was not defined in some file, nil is
   returned."
  (let ((location (conium:find-source-location
                   (if (functionp function)
                       function
                       (fdefinition function)))))
    (when (alexandria:starts-with :location location)
      (let ((file (second location)))
        (when (alexandria:starts-with :file file)
          (pathname (second file)))))))
#+(or cmu scl)
(defun fun-defined-from-pathname (function)
  "Takes a symbol or function and returns the pathname for the file the
   function was defined in.  If it was not defined in some file, nil is
   returned."
  (flet ((frob (code)
              (let ((info (kernel:%code-debug-info code)))
                     (when info
                              (let ((sources (c::debug-info-source info)))
                                 (when sources
                                      (let ((source (car sources)))
                                             (when (eq (c::debug-source-from source) :file)
                                                      (c::debug-source-name source)))))))))
    (typecase function
      (symbol (fun-defined-from-pathname (fdefinition function)))
      (kernel:byte-closure
       (fun-defined-from-pathname (kernel:byte-closure-function function)))
      (kernel:byte-function
       (frob (c::byte-function-component function)))
      (function
       (frob (kernel:function-code-header (kernel:%function-self function))))
      (t nil))))


(defvar *editor-describe-stream*
  (#+CMU system:make-indenting-stream #-CMU progn *standard-output*))

;;; EDITOR-DESCRIBE-FUNCTION has to mess around to get indenting streams to
;;; work.  These apparently work fine for DESCRIBE, for which they were defined,
;;; but not in general.  It seems they don't indent initial text, only that
;;; following a newline, so inside our use of INDENTING-FURTHER, we need some
;;; form before the WRITE-STRING.  To get this to work, I had to remove the ~%
;;; from the FORMAT string, and use FRESH-LINE; simply using FRESH-LINE with
;;; the ~% caused an extra blank line.  Possibly I should not have glommed onto
;;; this hack whose interface comes from three different packages, but it did
;;; the right thing ....
;;;
;;; Also, we have set INDENTING-STREAM-STREAM to make sure the indenting stream
;;; is based on whatever *standard-output* is when we are called.
;;;
(defun editor-describe-function (fun sym)
  "Calls DESCRIBE on fun.  If fun is compiled, and its original name is not sym,
   then this also outputs any 'function documentation for sym to
   *standard-output*."
  sym
  (describe fun)
  #+GBNIL
  (when (and (compiled-function-p fun)
             (not (eq (kernel:%function-name (kernel:%closure-function fun))
                      sym)))
    (let ((doc (documentation sym 'function)))
      (when doc
        (format t "~&Function documentation for ~S:" sym)
        (setf (lisp::indenting-stream-stream *editor-describe-stream*)
              *standard-output*)
        (ext:indenting-further *editor-describe-stream* 2
          (fresh-line *editor-describe-stream*)
          (write-string doc *editor-describe-stream*))))))




;;;; X Stuff.
;;; Setting window cursors ...
;;;


;;;; Some hacks for supporting Hemlock under Mach.

;;; WINDOWED-MONITOR-P is used by the reverse video variable's hook function
;;; to determine if it needs to go around fixing all the windows.
;;;
(defun windowed-monitor-p ()
  "This returns whether the monitor is being used with a window system.  It
   returns the console's CLX display structure."
  *editor-windowed-input*)

(defun process-editor-tty-input (&optional fd)
  (declare (ignore fd))
  ;; no-op
  )

(defvar *tty-translations* (make-hash-table :test #'equal))

(defun register-tty-translation (string keysym &key kludge)
  (when kludge
    ;; FIXME: This is pretty terrible, but for some reason my *terminfo* has
    ;; Esc,O,<foo> for arraw keys, whereas terminal actually sends Esc,[,<foo>
    ;; -- either I don't understand how terminfo stuff is supposed to work,
    ;; Apple ships with a broken terminfo db, or if something is wrong with
    ;; the terminfo code. I'm inclined to blame me...
    (assert (eq #\O (char string 1)))
    (setf string (format nil "~A[~A" (char string 0) (subseq string 2))))
  (setf (gethash (string string) *tty-translations*) keysym))

(defun translate-tty-event (data)
  (let* ((string (coerce data 'string))
         (sym (gethash string *tty-translations*)))
    (if sym
        (etypecase sym
          (hemlock-ext:key-event sym)
          (t (hemlock-ext:make-key-event sym 0)))
        (when (= 1 (length string))
          (hemlock-ext:char-key-event (char string 0))))))

(defun tty-key-event (data)
  (loop with start = 0
        with length = (length data)
        while (< start length)
        do (loop for end from length downto (1+ start)
                 do (let ((event (translate-tty-event (subseq data start end))))
                      (when event
                        (q-event *real-editor-input* event)
                        (setf start end)
                        (return)))))
  #+nil
  (iter:iter (iter:for char in-vector data)
             (let ((sym
                    (cond
                      ((eql char #\newline) ;### hmm
                       (hemlock-ext:key-event-keysym #k"Return"))
                      ((eql char #\tab) ;### hmm
                       (hemlock-ext:key-event-keysym #k"Tab"))
                      ((eql char #\Backspace)
                       (hemlock-ext:key-event-keysym #k"Backspace"))
                      ((eql char #\Escape)
                       (hemlock-ext:key-event-keysym #k"Escape"))
                      ((eql char #\rubout)
                       (hemlock-ext:key-event-keysym #k"delete")))))
               (q-event *real-editor-input*
                        (if sym
                            (hemlock-ext:make-key-event sym 0)
                            (hemlock-ext:char-key-event char))))))

#||
;;; GET-EDITOR-TTY-INPUT reads from stream's Unix file descriptor queuing events
;;; in the stream's queue.
;;;
#+NIL
(defun editor-tty-listen (stream)
  (alien:with-alien ((nc c-call:int))
    (and (unix:unix-ioctl (tty-editor-input-fd stream)
                          unix::FIONREAD
                          (alien:alien-sap (alien:addr nc)))
         (> nc 0))))
||#


;;;;
;;;; PREPL-integration
;;;;

(defun prepl-hemlock-command-integration-hook (cmd override)
  (multiple-value-bind (fun parsing)
                       (find-override-for-prepl cmd (or override t))
    (cond
     (fun (values fun parsing))
     ((not override) (find-command-for-prepl-normally cmd))
     (nil))))

(defparameter *prepl-command-overrides*
  ;; List of (prepl-command-name hemlock-command-name)
  ;;      or (prepl-command-name hemlock-function-name)
  ;;
  ;; Hemlock commands (first form) are run in the master.
  ;; Hemlock functions (second form) are called directly in the slave (!).
  ;;
  '(("apropos" "Slave Apropos Ignoring Point")
    ("bt" hemlock::debug-using-master)
    ("zoom" hemlock::debug-using-master)
    ("help" call-command-with-redirection)))

(defun call-with-typeout-pop-up-in-master (fun buffer-name)
  (let* ((buffer-name (or buffer-name "Unnamed typescript"))
         (ts-data
          (hemlock.wire:remote-value
           hemlock.wire::*current-wire*
           (hemlock::%make-extra-typescript-buffer buffer-name)))
         (stream
          ;; (hemlock::make-ts-stream hemlock.wire::*current-wire* ts-data)
          (hemlock::connect-stream ts-data)))
    (funcall fun stream)))

(defmacro with-typeout-pop-up-in-master
    ((var &optional buffer-name) &body body)
  `(call-with-typeout-pop-up-in-master (lambda (,var) ,@body)
                                       ,buffer-name))

(defun call-with-standard-synonym-streams (fun)
  (let ((*standard-input* (make-synonym-stream '*terminal-io*))
        (*standard-output* (make-synonym-stream '*terminal-io*))
        (*error-output* (make-synonym-stream '*terminal-io*))
        (*debug-io* (make-synonym-stream '*terminal-io*))
        (*query-io* (make-synonym-stream '*terminal-io*)))
    (funcall fun)))

(defun call-command-with-redirection ()
  (with-typeout-pop-up-in-master (*terminal-io* "Command output")
    (call-with-standard-synonym-streams #'prepl:call-next-command)))

(defun find-override-for-prepl (cmd override)
  (let* ((cons (assoc cmd *prepl-command-overrides* :test 'string-equal))
         (mapping (cadr cons)))
    (when cons
      (typecase mapping
        (symbol
         (values (lambda (&rest args) (apply mapping args)) override))
        (t
         (let ((cmd (getstring mapping hi::*command-names*)))
           (when cmd
             (let ((sym (command-function cmd)))
               (check-type sym symbol)
               (values (lambda (&rest args)
                         (hemlock::eval-in-master `(,sym nil ,@args)))
                       override)))))))))

(defun find-command-for-prepl-normally (cmd)
  ;; Note: Using lisp syntax for command names here (with dashes) rather
  ;; than real commands to make tokenization in the REPL easier.  This
  ;; way, the point where arguments start is never dependent on which
  ;; commmands are defined.
  ;;
  (let* ((sym (find-symbol (concatenate 'string
                                        (canonical-case cmd)
                                        (symbol-name '#:-command))
                           :hemlock))
         (fun (and sym (fdefinition sym))))
    (when fun
      (values (lambda (&rest args)
                (hemlock::eval-in-master `(funcall ',sym nil ,@args)))
              t))))

(push 'prepl-hemlock-command-integration-hook prepl:*command-parser-hooks*)
