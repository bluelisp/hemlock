;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(in-package :hemlock-internals)

;;;
;;; **********************************************************************
;;;
;;; This file contains the code that handles input to Hemlock.
;;;


;;;
;;; INPUT-WAITING is exported solely as a hack for the kbdmac definition
;;; mechanism.
;;;


;;; These are public variables users hand to the four basic editor input
;;; routines for method dispatching:
;;;    GET-KEY-EVENT
;;;    UNGET-KEY-EVENT
;;;    LISTEN-EDITOR-INPUT
;;;    CLEAR-EDITOR-INPUT
;;;
(defvar *editor-input* nil
  "A structure used to do various operations on terminal input.")

(defvar *real-editor-input* ()
  "Useful when we want to read from the terminal when *editor-input* is
   rebound.")



;;;; editor-input structure.

(defclass editor-input ()
  ;; Queue of events on this stream.  The queue always contains at least one
  ;; one element, which is the key-event most recently read.  If no event has
  ;; been read, the event is a dummy with a nil key-event.
  ((head :accessor editor-input-head :initarg :head)
   (tail :accessor editor-input-tail :initarg :tail)))

(defmethod initialize-instance :after ((instance editor-input) &key)
  (let ((e (hi::make-input-event)))
    (setf (editor-input-head instance) e)
    (setf (editor-input-tail instance) e)))

;;;; GET-KEY-EVENT, UNGET-KEY-EVENT, LISTEN-EDITOR-INPUT, CLEAR-EDITOR-INPUT.

;;; GET-KEY-EVENT -- Public.
;;;
(defgeneric get-key-event (editor-input &optional ignore-abort-attempts-p)
  (:documentation
   "This function returns a key-event as soon as it is available on
    editor-input.  Editor-input is either *editor-input* or *real-editor-input*.
    Ignore-abort-attempts-p indicates whether #k\"C-g\" and #k\"C-G\" throw to
    the editor's top-level command loop; when this is non-nil, this function
    returns those key-events when the user types them.  Otherwise, it aborts the
    editor's current state, returning to the command loop."))

;;; UNGET-KEY-EVENT -- Public.
;;;
(defgeneric unget-key-event (key-event editor-input)
  (:documentation
   "This function returns the key-event to editor-input, so the next invocation
    of GET-KEY-EVENT will return the key-event.  If the key-event is #k\"C-g\"
    or #k\"C-G\", then whether GET-KEY-EVENT returns it depends on its second
    argument.  Editor-input is either *editor-input* or *real-editor-input*."))

;;; CLEAR-EDITOR-INPUT -- Public.
;;;
(defgeneric clear-editor-input (editor-input)
  (:documentation
   "This function flushes any pending input on editor-input.  Editor-input
    is either *editor-input* or *real-editor-input*."))

;;; LISTEN-EDITOR-INPUT -- Public.
;;;
(defgeneric listen-editor-input (editor-input)
  (:documentation
   "This function returns whether there is any input available on editor-input.
    Editor-input is either *editor-input* or *real-editor-input*."))

;;; These are the elements of the editor-input event queue.
;;;
(defstruct (input-event (:constructor make-input-event ()))
  next          ; Next queued event, or NIL if none.
  hunk          ; Screen hunk event was read from.
  key-event     ; Key-event read.
  x             ; X and Y character position of mouse cursor.
  y
  unread-p)

(defvar *free-input-events* ())

(defun new-event (key-event x y hunk next &optional unread-p)
  (let ((res (if *free-input-events*
                 (shiftf *free-input-events*
                         (input-event-next *free-input-events*))
                 (make-input-event))))
    (setf (input-event-key-event res) key-event)
    (setf (input-event-x res) x)
    (setf (input-event-y res) y)
    (setf (input-event-hunk res) hunk)
    (setf (input-event-next res) next)
    (setf (input-event-unread-p res) unread-p)
    res))

;;; This is a public variable.
;;;
(defvar *last-key-event-typed* ()
  "This variable contains the last key-event typed by the user and read as
   input.")

;;; This is a public variable.  SITE-INIT initializes this.
;;;
(defvar *key-event-history* nil
  "This ring holds the last 60 key-events read by the command interpreter.")

(declaim (special *input-transcript*))

;;; DQ-EVENT is used in editor stream methods for popping off input.
;;; If there is an event not yet read in Stream, then pop the queue
;;; and return the character.  If there is none, return NIL.
;;;
(defun dq-event (stream)
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
          (next (input-event-next head)))
     (if next
         (let ((key-event (input-event-key-event next)))
           (setf (editor-input-head stream) next)
           (shiftf (input-event-next head) *free-input-events* head)
           (ring-push key-event *key-event-history*)
           (setf *last-key-event-typed* key-event)
           (when *input-transcript*
             (vector-push-extend key-event *input-transcript*))
           key-event)))))

;;; Q-EVENT is used in low level input fetching routines to add input to the
;;; editor stream.
;;;
(defun q-event (stream key-event &optional x y hunk)
  (hemlock-ext:without-interrupts
   (let ((new (new-event key-event x y hunk nil))
         (tail (editor-input-tail stream)))
     (setf (input-event-next tail) new)
     (setf (editor-input-tail stream) new))))

(defun un-event (key-event stream)
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
          (next (input-event-next head))
          (new (new-event key-event (input-event-x head) (input-event-y head)
                          (input-event-hunk head) next t)))
     (setf (input-event-next head) new)
     (unless next (setf (editor-input-tail stream) new)))))



;;;; Keyboard macro hacks.

(defvar *input-transcript* ()
  "If this variable is non-null then it should contain an adjustable vector
  with a fill pointer into which all keyboard input will be pushed.")

;;; INPUT-WAITING  --  Internal
;;;
;;;    An Evil hack that tells us whether there is an unread key-event on
;;; *editor-input*.  Note that this is applied to the real *editor-input*
;;; rather than to a kbdmac stream.
;;;
(defun input-waiting ()
  "Returns true if there is a key-event which has been unread-key-event'ed
   on *editor-input*.  Used by the keyboard macro stuff."
  (let ((next (input-event-next
               (editor-input-head *real-editor-input*))))
    (and next (input-event-unread-p next))))



;;;; Input method macro.

(defvar *in-hemlock-stream-input-method* nil
  "This keeps us from undefined nasties like re-entering Hemlock stream
   input methods from input hooks and scheduled events.")

(declaim (special *screen-image-trashed*))

;;; These are the characters GET-KEY-EVENT notices when it pays attention
;;; to aborting input.  This happens via EDITOR-INPUT-METHOD-MACRO.
;;;
(defparameter editor-abort-key-events (list #k"Control-g" #k"Control-G"))

(defmacro abort-key-event-p (key-event)
  `(member ,key-event editor-abort-key-events))

;;; EDITOR-INPUT-METHOD-MACRO  --  Internal.
;;;
;;; WINDOWED-GET-KEY-EVENT and TTY-GET-KEY-EVENT use this.  Somewhat odd stuff
;;; goes on here because this is the place where Hemlock waits, so this is
;;; where we redisplay, check the time for scheduled events, etc.  In the loop,
;;; we call the input hook when we get a character and leave the loop.  If
;;; there isn't any input, invoke any scheduled events whose time is up.
;;; Unless SERVE-EVENT returns immediately and did something, (serve-event 0),
;;; call redisplay, note that we are going into a read wait, and call
;;; SERVE-EVENT with a wait or infinite timeout.  Upon exiting the loop, turn
;;; off the read wait note and check for the abort character.  Return the
;;; key-event we got.  We bind an error condition handler here because the
;;; default Hemlock error handler goes into a little debugging prompt loop, but
;;; if we got an error in getting input, we should prompt the user using the
;;; input method (recursively even).
;;;
(defgeneric %editor-input-method (editor-input ignore-abort-attempts-p))
(defmethod %editor-input-method
    ((editor-input editor-input) ignore-abort-attempts-p)
  (let ((*in-hemlock-stream-input-method* t)
        (device (device-hunk-device (window-hunk (current-window))))
        key-event)
    (loop
     (when (setf key-event (dq-event editor-input))
       (dolist (f (variable-value 'hemlock::input-hook)) (funcall f))
       (return))
     (invoke-scheduled-events)
     (unless (internal-redisplay)
       (internal-redisplay)
       (device-note-read-wait device t)
       (let ((wait (and (not
                         ;; Let's be extra careful here and prepare
                         ;; for the case where key events have been
                         ;; seen in the mean time, in which case we
                         ;; must not wait here:
                         (listen-editor-input editor-input))
                        (next-scheduled-event-wait))))
         (when wait
           (dispatch-events)))))
    (device-note-read-wait device nil)
    (when (and (abort-key-event-p key-event)
               ;; ignore-abort-attempts-p must exist outside the macro.
               ;; in this case it is bound in GET-KEY-EVENT.
               (not ignore-abort-attempts-p))
      (beep)
      (throw 'editor-top-level-catcher nil))
    key-event))



;;;; LAST-KEY-EVENT-CURSORPOS and WINDOW-INPUT-HANDLER.

;;; LAST-KEY-EVENT-CURSORPOS  --  Public
;;;
;;; Just look up the saved info in the last read key event.
;;;
(defun last-key-event-cursorpos ()
  "Return as values, the (X, Y) character position and window where the
   last key event happened.  If this cannot be determined, Nil is returned.
   If in the modeline, return a Y position of NIL and the correct X and window.
   Returns nil for terminal input."
  (let* ((ev (editor-input-head *real-editor-input*))
         (hunk (input-event-hunk ev))
         (window (and hunk (device-hunk-window hunk))))
    (when window
      (values (input-event-x ev) (input-event-y ev) window))))

;;; WINDOW-INPUT-HANDLER  --  Internal
;;;
;;; This is the input-handler function for hunks that implement windows.  It
;;; just queues the events on *real-editor-input*.
;;;
(defun window-input-handler (hunk char x y)
  (check-type *editor-input* editor-input)
  (q-event *editor-input* char x y hunk))



;;;; Random typeout input routines.

#|
(defun wait-for-more (stream)
  (let ((key-event (more-read-key-event)))
    (cond ((logical-key-event-p key-event :yes))
          ((or (logical-key-event-p key-event :do-all)
               (logical-key-event-p key-event :exit))
           (setf (random-typeout-stream-no-prompt stream) t)
           (random-typeout-cleanup stream))
          ((logical-key-event-p key-event :keep)
           (setf (random-typeout-stream-no-prompt stream) t)
           (maybe-keep-random-typeout-window stream)
           (random-typeout-cleanup stream))
          ((logical-key-event-p key-event :no)
           (random-typeout-cleanup stream)
           (throw 'more-punt nil))
          (t
           (unget-key-event key-event *editor-input*)
           (random-typeout-cleanup stream)
           (throw 'more-punt nil)))))

(declaim (special *more-prompt-action*))

(defun maybe-keep-random-typeout-window (stream)
  (let* ((window (random-typeout-stream-window stream))
         (buffer (window-buffer window))
         (start (buffer-start-mark buffer)))
    (when (typep (hi::device-hunk-device (hi::window-hunk window))
                 (the class (class-of 'hi::bitmap-device)))
      (let ((*more-prompt-action* :normal))
        (update-modeline-field buffer window :more-prompt)
        (random-typeout-redisplay window))
      (buffer-start (buffer-point buffer))
      (let* ((xwindow (make-xwindow-like-hwindow window))
             (window (make-window start :window xwindow)))
        (unless window
          #+clx(xlib:destroy-window xwindow)
          (editor-error "Could not create random typeout window."))))))

(defun end-random-typeout (stream)
  (let ((*more-prompt-action* :flush)
        (window (random-typeout-stream-window stream)))
    (update-modeline-field (window-buffer window) window :more-prompt)
    (random-typeout-redisplay window))
  (unless (random-typeout-stream-no-prompt stream)
    (let* ((key-event (more-read-key-event))
           (keep-p (logical-key-event-p key-event :keep)))
      (when keep-p (maybe-keep-random-typeout-window stream))
      (random-typeout-cleanup stream)
      (unless (or (logical-key-event-p key-event :do-all)
                  (logical-key-event-p key-event :exit)
                  (logical-key-event-p key-event :no)
                  (logical-key-event-p key-event :yes)
                  keep-p)
        (unget-key-event key-event *editor-input*)))))
|#

;;; MORE-READ-KEY-EVENT -- Internal.
;;;
;;; This gets some input from the type of stream bound to *editor-input*.  Need
;;; to loop over SERVE-EVENT since it returns on any kind of event (not
;;; necessarily a key or button event).
;;;
;;; Currently this does not work for keyboard macro streams!
;;;
(defun more-read-key-event ()
  (clear-editor-input *editor-input*)
  (let ((key-event (loop
                     (let ((key-event (dq-event *editor-input*)))
                       (when key-event (return key-event))
                       (hemlock-ext:serve-event)))))
    (when (abort-key-event-p key-event)
      (beep)
      (throw 'editor-top-level-catcher nil))
    key-event))
