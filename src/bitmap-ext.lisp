;;;; -*- Mode: Lisp; indent-with-tabs: nil -*-

(in-package :hemlock-ext)

;; these came from RE-INITIALIZE-KEY-EVENTS:
(define-clx-modifier (xlib:make-state-mask :shift) "Shift")
(define-clx-modifier (xlib:make-state-mask :mod-1) "Meta")
(define-clx-modifier (xlib:make-state-mask :control) "Control")
(define-clx-modifier (xlib:make-state-mask :lock) "Lock")

;;; TRANSLATE-KEY-EVENT -- Public.
;;;
(defun translate-key-event (display scan-code bits)
  "Translates the X scan-code and X bits to a key-event.  First this maps
   scan-code to an X keysym using XLIB:KEYCODE->KEYSYM looking at bits and
   supplying index as 1 if the X shift bit is on, 0 otherwise.

   If the resulting keysym is undefined, and it is not a modifier keysym, then
   this signals an error.  If the keysym is a modifier key, then this returns
   nil.

   If the following conditions are satisfied
      the keysym is defined
      the X shift bit is off
      the X lock bit is on
      the X keysym represents a lowercase letter
   then this maps the scan-code again supplying index as 1 this time, treating
   the X lock bit as a caps-lock bit.  If this results in an undefined keysym,
   this signals an error.  Otherwise, this makes a key-event with the keysym
   and bits formed by mapping the X bits to key-event bits.

   If any state bit is set that has no suitable modifier translation, it is
   passed to XLIB:DEFAULT-KEYSYM-INDEX in order to handle Mode_Switch keys
   appropriately.

   Otherwise, this makes a key-event with the keysym and bits formed by mapping
   the X bits to key-event bits."
  (let ((new-bits 0)
        shiftp lockp)
    (dolist (map *modifier-translations*)
      (unless (zerop (logand (car map) bits))
        ;; ignore the bits of the mapping for the determination of a key index
        (setq bits (logxor bits (car map)))
        (cond
         ((string-equal (cdr map) "Shift")
          (setf shiftp t))
         ((string-equal (cdr map) "Lock")
          (setf lockp t))
         (t (setf new-bits
                  (logior new-bits (key-event-modifier-mask (cdr map))))))))
    ;; here pass any remaining modifier bits to clx
    (let* ((index  (and (not (zerop bits))
                        (xlib:default-keysym-index display scan-code bits)))
           (keysym (xlib:keycode->keysym display scan-code (or index (if shiftp 1 0)))))
      (cond ((null (keysym-names keysym))
             nil)
            ((and (not shiftp) lockp (<= 97 keysym 122)) ; small-alpha-char-p
             (let ((keysym (xlib:keycode->keysym display scan-code 1)))
               (if (keysym-names keysym)
                   (make-key-event keysym new-bits)
                   nil)))
            (t
             (make-key-event keysym new-bits))))))

(defun call-with-clx-event-handling (fun display handler)
  "Evaluates body in a context where events are handled for the display
   by calling handler on the display.  This destroys any previously established
   handler for display."
  (unwind-protect
      (progn
        (enable-clx-event-handling display handler)
        (funcall fun))
    (disable-clx-event-handling display)))

;;;; Object set event handling.

;;; This is bound by OBJECT-SET-EVENT-HANDLER, so DISPATCH-EVENT can clear
;;; events on the display before signalling any errors.  This is necessary
;;; since reading on certain CMU Common Lisp streams involves SERVER, and
;;; getting an error while trying to handle an event causes repeated attempts
;;; to handle the same event.
;;;
(defvar *process-clx-event-display* nil)

(defvar *object-set-event-handler-print* nil)

(defun object-set-event-handler (display &optional (timeout 0))
  "This display event handler uses object sets to map event windows cross
   event types to handlers.  It uses XLIB:EVENT-CASE to bind all the slots
   of each event, calling the handlers on all these values in addition to
   the event key and send-event-p.  Describe EXT:SERVE-MUMBLE, where mumble
   is an event keyword name for the exact order of arguments.
   :mapping-notify and :keymap-notify events are ignored since they do not
   occur on any particular window.  After calling a handler, each branch
   returns t to discard the event.  While the handler is executing, all
   errors go through a handler that flushes all the display's events and
   returns.  This prevents infinite errors since the debug and terminal
   streams loop over SYSTEM:SERVE-EVENT.  This function returns t if there
   were some event to handle, nil otherwise.  It returns immediately if
   there is no event to handle."
  (macrolet ((dispatch (event-key &rest args)
               `(multiple-value-bind (object object-set)
                 (lisp--map-xwindow event-window)
                 (unless object
                   (cond ((not (typep event-window 'xlib:window))
                          ;;(xlib:discard-current-event display)
                          (warn "Discarding ~S event on non-window ~S."
                                ,event-key event-window)
                          (return-from object-set-event-handler nil)
                          )
                         (t
                          (flush-display-events display)
                          (error "~S not a known X window.~%~
                                   Received event ~S."
                                 event-window ,event-key))))
                 (handler-bind ((error #'(lambda (condx)
                                           (declare (ignore condx))
                                           (flush-display-events display))))
                   (when *object-set-event-handler-print*
                     (print ,event-key) (force-output))
                   (funcall (gethash ,event-key
                                     (object-set-table object-set)
                                     (object-set-default-handler
                                      object-set))
                            object ,event-key
                            ,@args))
                 (setf result t))))
    (let ((*process-clx-event-display* display)
          (result nil))
      (xlib:event-case (display :timeout timeout)
                       ((:key-press :key-release :button-press :button-release)
                        (event-key event-window root child same-screen-p
                                   x y root-x root-y state time code send-event-p)
                        (dispatch event-key event-window root child same-screen-p
                                  x y root-x root-y state time code send-event-p))
                       (:motion-notify (event-window root child same-screen-p
                                        x y root-x root-y state time hint-p send-event-p)
                        (dispatch :motion-notify event-window root child same-screen-p
                         x y root-x root-y state time hint-p send-event-p))
                       (:enter-notify (event-window root child same-screen-p
                                       x y root-x root-y state time mode kind send-event-p)
                        (dispatch :enter-notify event-window root child same-screen-p
                         x y root-x root-y state time mode kind send-event-p))
                       (:leave-notify (event-window root child same-screen-p
                                       x y root-x root-y state time mode kind send-event-p)
                        (dispatch :leave-notify event-window root child same-screen-p
                         x y root-x root-y state time mode kind send-event-p))
                       (:exposure (event-window x y width height count send-event-p)
                        (dispatch :exposure event-window x y width height count send-event-p))
                       (:graphics-exposure (event-window x y width height count major minor
                                            send-event-p)
                        (dispatch :graphics-exposure event-window x y width height
                         count major minor send-event-p))
                       (:no-exposure (event-window major minor send-event-p)
                        (dispatch :no-exposure event-window major minor send-event-p))
                       (:focus-in (event-window mode kind send-event-p)
                        (dispatch :focus-in event-window mode kind send-event-p))
                       (:focus-out (event-window mode kind send-event-p)
                        (dispatch :focus-out event-window mode kind send-event-p))
                       (:keymap-notify ()
                        (warn "Ignoring keymap notify event.")
                        (when *object-set-event-handler-print*
                          (print :keymap-notify) (force-output))
                        (setf result t))
                       (:visibility-notify (event-window state send-event-p)
                        (dispatch :visibility-notify event-window state send-event-p))
                       (:create-notify (event-window window x y width height border-width
                                        override-redirect-p send-event-p)
                        (dispatch :create-notify event-window window x y width height
                         border-width override-redirect-p send-event-p))
                       (:destroy-notify (event-window window send-event-p)
                        (dispatch :destroy-notify event-window window send-event-p))
                       (:unmap-notify (event-window window configure-p send-event-p)
                        (dispatch :unmap-notify event-window window configure-p send-event-p))
                       (:map-notify (event-window window override-redirect-p send-event-p)
                        (dispatch :map-notify event-window window override-redirect-p
                         send-event-p))
                       (:map-request (event-window window send-event-p)
                        (dispatch :map-request event-window window send-event-p))
                       (:reparent-notify (event-window window parent x y override-redirect-p
                                          send-event-p)
                        (dispatch :reparent-notify event-window window parent x y
                         override-redirect-p send-event-p))
                       (:configure-notify (event-window window x y width height border-width
                                           above-sibling override-redirect-p send-event-p)
                        (dispatch :configure-notify event-window window x y width height
                         border-width above-sibling override-redirect-p
                         send-event-p))
                       (:gravity-notify (event-window window x y send-event-p)
                        (dispatch :gravity-notify event-window window x y send-event-p))
                       (:resize-request (event-window width height send-event-p)
                        (dispatch :resize-request event-window width height send-event-p))
                       (:configure-request (event-window window x y width height border-width
                                            stack-mode above-sibling value-mask send-event-p)
                        (dispatch :configure-request event-window window x y width height
                         border-width stack-mode above-sibling value-mask
                         send-event-p))
                       (:circulate-notify (event-window window place send-event-p)
                        (dispatch :circulate-notify event-window window place send-event-p))
                       (:circulate-request (event-window window place send-event-p)
                        (dispatch :circulate-request event-window window place send-event-p))
                       (:property-notify (event-window atom state time send-event-p)
                        (dispatch :property-notify event-window atom state time send-event-p))
                       (:selection-clear (event-window selection time send-event-p)
                        (dispatch :selection-notify event-window selection time send-event-p))
                       (:selection-request (event-window requestor selection target property
                                            time send-event-p)
                        (dispatch :selection-request event-window requestor selection target
                         property time send-event-p))
                       (:selection-notify (event-window selection target property time
                                           send-event-p)
                        (dispatch :selection-notify event-window selection target property time
                         send-event-p))
                       (:colormap-notify (event-window colormap new-p installed-p send-event-p)
                        (dispatch :colormap-notify event-window colormap new-p installed-p
                         send-event-p))
                       (:mapping-notify (request)
                        (warn "Ignoring mapping notify event -- ~S." request)
                        (when *object-set-event-handler-print*
                          (print :mapping-notify) (force-output))
                        (setf result t))
                       (:client-message (event-window format data send-event-p)
                        (dispatch :client-message event-window format data send-event-p)))
      result)))

(defun default-clx-event-handler (object event-key event-window &rest ignore)
  (declare (ignore ignore))
  (flush-display-events *process-clx-event-display*)
  (error "No handler for event type ~S on ~S in ~S."
         event-key object (lisp--map-xwindow event-window)))

(defun flush-display-events (display)
  "Dumps all the events in display's event queue including the current one
   in case this is called from within XLIB:EVENT-CASE, etc."
  (xlib:discard-current-event display)
  (xlib:event-case (display :discard-p t :timeout 0)
    (t () nil)))

(defvar *display-event-handlers* nil)

;;; ENABLE-CLX-EVENT-HANDLING associates the display with the handler in
;;; *display-event-handlers*.  It also uses SYSTEM:ADD-FD-HANDLER to have
;;; SYSTEM:SERVE-EVENT call CALL-DISPLAY-EVENT-HANDLER whenever anything shows
;;; up from the display. Since CALL-DISPLAY-EVENT-HANDLER is called on a
;;; file descriptor, the file descriptor is also mapped to the display in
;;; *clx-fds-to-displays*, so the user's handler can be called on the display.
;;;
(defvar *clx-fds-to-displays* (make-hash-table))
(defun enable-clx-event-handling (display handler)
  "After calling this, when SYSTEM:SERVE-EVENT notices input on display's
   connection to the X11 server, handler is called on the display.  Handler
   is invoked in a dynamic context with an error handler bound that will
   flush all events from the display and return.  By returning, it declines
   to handle the error, but it will have cleared all events; thus, entering
   the debugger will not result in infinite errors due to streams that wait
   via SYSTEM:SERVE-EVENT for input.  Calling this repeatedly on the same
   display establishes handler as a new handler, replacing any previous one
   for display."
  (check-type display xlib:display)
  (let ((change-handler (assoc display *display-event-handlers*)))
    (if change-handler
        (setf (cdr change-handler) handler)
        (let ((fd (hi::stream-fd (xlib::display-input-stream display))))
          (iolib:set-io-handler
           hi::*event-base* fd :read #'call-display-event-handler)
          (setf (gethash fd *clx-fds-to-displays*) display)
          (push (cons display handler) *display-event-handlers*)))))

;;; CALL-DISPLAY-EVENT-HANDLER maps the file descriptor to its display and maps
;;; the display to its handler.  If we can't find the display, we remove the
;;; file descriptor using SYSTEM:INVALIDATE-DESCRIPTOR and try to remove the
;;; display from *display-event-handlers*.  This is necessary to try to keep
;;; SYSTEM:SERVE-EVENT from repeatedly trying to handle the same event over and
;;; over.  This is possible since many CMU Common Lisp streams loop over
;;; SYSTEM:SERVE-EVENT, so when the debugger is entered, infinite errors are
;;; possible.
;;;
(defun call-display-event-handler (file-descriptor event error)
  (if (eq error :error)
      (iolib:remove-fd-handlers hi::*event-base* file-descriptor :read t)
      (let ((display (gethash file-descriptor *clx-fds-to-displays*)))
        (unless display
          (iolib.multiplex:remove-fd-handlers hi::*event-base* file-descriptor)
          (setf *display-event-handlers*
                (delete file-descriptor *display-event-handlers*
                        :key #'(lambda (d/h)
                                 (hi::stream-fd
                                  (xlib::display-input-stream
                                   (car d/h))))))
          (error "File descriptor ~S not associated with any CLX display.~%~
                  It has been removed from system:serve-event's knowledge."
                 file-descriptor))
        (let ((handler (cdr (assoc display *display-event-handlers*))))
          (unless handler
            (flush-display-events display)
            (error "Display ~S not associated with any event handler."
                   display))
          (handler-bind ((error #'(lambda (condx)
                                    (declare (ignore condx))
                                    (flush-display-events display))))
            (funcall handler display))))))

(defun disable-clx-event-handling (display)
  "Undoes the effect of EXT:ENABLE-CLX-EVENT-HANDLING."
  (setf *display-event-handlers*
        (delete display *display-event-handlers* :key #'car))
  (let ((fd (hi::stream-fd (xlib::display-input-stream display))))
    (remhash fd *clx-fds-to-displays*)
    (iolib:remove-fd-handlers hi::*event-base* fd :read t)))

;;; (defun serve-event (&optional timeout)
;;;   (let ((dps))
;;;     (maphash (lambda (win value)
;;;                (declare (ignore value))
;;;                (pushnew (xlib:window-display win) dps))
;;;              *xwindow-hash*)
;;;     (when dps
;;;       (object-set-event-handler (car dps) timeout))))
