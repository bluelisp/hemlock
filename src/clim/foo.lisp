;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM Phemlock
;;;   Created: 2004-11-20       <- not true!
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2003, 2004 by Gilbert Baumann

(in-package :clim-hemlock)

;;;; ------------------------------------------------------------------------------------------
;;;;  RANDOM NOTES
;;;;

;; Hemlock has this funny notion of "hunks" which are just device-specific
;; descriptions of a (hemlock) window. A hunk then points to some real
;; window system window or somesuch. I really thing that these days thanks
;; to multiple inheritance we should conflate hunks, [hemlock] windows and
;; [window system] windows. This is not how that is currently done though,
;; as i am not 100% certain about the implications.

;; Perhaps Hemlock should function as a frame manager too, so that you can
;; embed say a lister pane into Hemlock's main application frame. Or
;; goodies written by third parties like a side bar. Event processing then
;; becomes tricky and keyboard events are about to be under focus control
;; and mouse events are about to be under pointer control. And: CLIM won't
;; cope with a single application frame been displayed more than once like
;; Emacs can display a buffer more than once. But: This is perhaps even
;; possible thru' some glorious kludges.

;; INTEGRATION OF HEMLOCK AS A GADGET. When we want to have Hemlock as a
;; CLIM gadget as a substitute for the text-field gadget we need to tackle
;; the following problems:

;;  - CLIM has an event loop which just passes single events to
;;    HANDLE-EVENT [in case of gadgets] and expects the event handler to
;;    return after done with the event. But Hemlock really has a kind of
;;    recursive event, where one event can trigger going down into another
;;    event loop. The easiest solution to solve this, is to have two
;;    threads, the CLIM thread and for each text-field gadget another
;;    Hemlock process.

;;  - In a single line text entry field there really is no room for a
;;    modeline or the echo buffer. The modeline isn't that important here,
;;    but you'd still want to be able to enter stuff to the echo buffer.
;;    The solution perhaps is an echo buffer, which pops up on demand and
;;    is placed near the cursor and goes away after you are finished
;;    providing some arguments.

;;  - When using Hemlock as a text-field gadget, you'd really want to avoid
;;    creating windows or switching buffers.

;; HEMLOCK AS THE LINE EDITOR. Not really much thought about that one yet
;; besides that all the restriction of above apply too.

;; MULTIPLE HEMLOCKS. There is the question about how much state should be
;; shared between mutiple instances of Hemlock.

;; - DEVICE-HUNKS doesn't seem to be used anywhere beyond device
;;   implementations.

;; - DEVICE-BOTTOM-WINDOW-BASE seems to be only used from
;;   tty-screen.lisp.

;;;; ------------------------------------------------------------------------------------------
;;;;  TODO / BUGS
;;;;

;; - fix this random CLIM bug with :y-align in draw-text*.

;; - where is the modeline?
;;   well, there now is a modeline, but it isn't up to date :(
;;   also the echo area now has one.

;; - new need a new composite pane.
;;   Or we use a different strategy.

;; - c-x 0, c-x 3, c-x 5.

;; - c-up and c-down

;; - something steals c-g

;; - pop up streams.

;; - BABA needs a real name.

;; - can't we merge a hunk with its stream thanks to multiple inheritance?

;; - we really need to get input working. I can't type umlauts and these
;;   dead keys aren't working either.

;;;; ------------------------------------------------------------------------------------------

(defparameter *gutter* 10
  "The gutter to place between between the matter in a hemlock pane and its
   margin to improve legibility (sp?, damn i miss ispell).")

(defclass clim-device (device)
  ;; cursor
  ((cursor-hunk
    :initform nil :documentation "The hunk that has the cursor.")
   (windows :initform nil) ))

(defclass clim-hunk-pane (CLIM:APPLICATION-PANE)
  ((hunk)
   ))

(defmethod device-init ((device clim-device))
  )

(defmethod device-exit ((device clim-device)))

(defmethod device-smart-redisplay ((device clim-device) window)
  ;; We aren't smart by any margin.
  (device-dumb-redisplay device window))

(defmethod device-after-redisplay ((device clim-device))
  )

(defmethod device-clear ((device clim-device))
  )

(defmethod device-note-read-wait ((device clim-device) on-off)
  )

(defmethod device-force-output ((device clim-device))
  )

(defmethod device-finish-output ((device clim-device) window)
  )

(defmethod device-put-cursor ((device clim-device) hunk x y)
  (with-slots (cursor-hunk) device
    (when cursor-hunk
      (clim-drop-cursor cursor-hunk)
      (with-slots (cx cy) cursor-hunk
        (setf cx nil cy nil)))
    (when hunk
      (with-slots (cx cy) hunk
        (setf cx x cy y))
      (clim-put-cursor hunk))
    (setf cursor-hunk hunk)))

(defmethod device-show-mark ((device clim-device) window x y time)
  )

;;;; Windows

;; In CLIM Hemlock each window is a single pane, which should keep
;; things simple. We do not yet have the notion of window groups.

(defmethod device-next-window ((device clim-device) window)
  (with-slots (windows) device
    (elt windows (mod (1+ (position window windows))
                      (length windows)))))

(defmethod device-previous-window ((device clim-device) window)
  (with-slots (windows) device
    (elt windows (mod (1- (position window windows))
                      (length windows)))))

(defmethod device-delete-window ((device clim-device) window)
  (let* ((hunk (window-hunk window))
         (stream (clim-hunk-stream hunk))
         (parent (clim:sheet-parent stream)))
    (clim:sheet-disown-child parent stream)
    (setf (slot-value device 'windows)
          (remove window (slot-value device 'windows)))
    (let ((buffer (window-buffer window)))
      (setf (buffer-windows buffer) (delete window (buffer-windows buffer)))) ) )

(defclass clim-hunk-pane (CLIM:APPLICATION-PANE)
  ((hunk)
   ))

(defmethod device-make-window ((device clim-device) start modelinep window font-family
                               ask-user x y width-arg height-arg proportion
                               &aux res)
  (let* ((hunk (window-hunk *current-window*))
         (stream (clim-hunk-stream hunk))
         (parent (clim:sheet-parent stream)))
    (clim:with-look-and-feel-realization
     ((clim:frame-manager clim:*application-frame*)
      clim:*application-frame*)
     (let ((new (clim:make-pane 'clim-hunk-pane
                                :incremental-redisplay t
                                :width 100 :height 200 #|:min-height 200|# :background clim:+white+)))
       (let* ((window (hi::internal-make-window))
              (hunk (make-instance 'clim-hunk :stream new)))
         (setf res window)
         (baba-aux device window hunk *current-buffer*)
         (let ((p (position *current-window* (slot-value device 'windows))))
           (setf (slot-value device 'windows)
                 (append (subseq (slot-value device 'windows) 0 p)
                         (list window)
                         (subseq (slot-value device 'windows) p)))) )
       ;; since we still can't draw on ungrafted windows ...
       (clim:sheet-adopt-child parent new)
       ;; Put it just before current window, only that this has no
       ;; effect with a vbox pane.
       (let* ((q (remove new (clim:sheet-children parent)))
              (p (position stream q)))
         (clim:reorder-sheets parent
                              (append (subseq q 0 (1+ p))
                                      (list new)
                                      (subseq q (1+ p))))
         (setf (clim:sheet-enabled-p new) t))))
    (finish-output *trace-output*))
  res)

(defmethod clim:handle-repaint :around ((pane clim-hunk-pane) region)
  (let ((w (clim:bounding-rectangle-width pane))
        (h (clim:bounding-rectangle-height pane)))
    (let ((device (device-hunk-device (slot-value pane 'hunk))))
      (with-slots (cursor-hunk) device
        (when cursor-hunk
          (clim-drop-cursor cursor-hunk)))
      (call-next-method)
      (with-slots (cursor-hunk) device
        (when cursor-hunk
          (clim-put-cursor cursor-hunk))))
    '(clim:draw-rectangle* (clim:sheet-medium pane)
                          3 3 (- w 3) (- h 20) :filled nil) ))


;;;;

(defmethod device-random-typeout-full-more ((device clim-device) stream)
  )

(defmethod device-random-typeout-line-more ((device clim-device) stream n)
  )

(defmethod device-random-typeout-setup ((device clim-device) stream n)
  )

(defmethod device-random-typeout-cleanup ((device clim-device) stream degree)
  )

(defmethod device-beep ((device clim-device) stream)
  )

;;; Hunks

(defclass clim-hunk (device-hunk)
  ((stream :initarg :stream
           :reader clim-hunk-stream
           :documentation "Extended output stream this hunk is displayed on.")
   (cx :initarg :cx :initform nil)
   (cy :initarg :cy :initform nil)
   (cw)
   (ch)
   (ts)))

;;; Input

(defclass clim-editor-input (editor-input)
  ((stream :initarg :stream
           :reader clim-editor-input-stream
           :documentation "Extended input stream we read from.")) )

(defmethod get-key-event ((stream clim-editor-input) &optional ignore-abort-attempts-p)
  (declare (ignorable ignore-abort-attempts-p))
  (or (hi::dq-event stream)
      ;;
      (progn                            ;###
        (hi::internal-redisplay)
        nil)
      ;;
      (do ((e (gesture-key-event (clim:read-gesture :stream (clim-editor-input-stream stream)))
              (gesture-key-event (clim:read-gesture :stream (clim-editor-input-stream stream)))))
          ((not (null e))
           ;; we queue and dequeue by the recursive call, because
           ;; dq-event has some extra logic.
           (hi::q-event stream e)
           (get-key-event stream)))))

(defmethod unget-key-event (key-event (stream clim-editor-input))
  (hi::un-event key-event stream))

(defmethod clear-editor-input ((stream clim-editor-input))
  ;; hmm?
  )

(defmethod listen-editor-input ((stream clim-editor-input))
  (or (hi::input-event-next (hi::editor-input-head stream))
      (listen (clim-editor-input-stream stream))))

;;;; There is awful lot to do to boot a device.

(defmethod clim:note-sheet-region-changed :after ((sheet clim-hunk-pane))
  (when (slot-boundp sheet 'hunk)
    (clim-window-changed (slot-value sheet 'hunk))
    (hi::internal-redisplay))
  (print 'hi-there *trace-output*)
  (finish-output *trace-output*))

(defmethod clim:change-space-requirements :around
    ((pane clim-hunk-pane)
     &key (max-height nil) (height nil)
          (max-width nil) (width nil) &allow-other-keys)
  nil)

#||
(clim:make-command-table 'hemlock-menu
                         :errorp nil
                         :menu '(("File" :menu hemlock-file-menu)))
(clim:make-command-table 'hemlock-file-menu
                         :errorp nil
                         :menu '(("Open" :command com-open-file)
                                 ("Open in another window" :command com-open-file-other-window)
                                 ("" :divider t)
                                 ("Exit Hemlock" :command com-exit-hemlock)
                                 ))
||#

(clim:define-application-frame hemlock ()
    ()
  (:pointer-documentation t)
  #||(:menu-bar hemlock-menu)||#
  (:panes
   (main clim-hunk-pane :display-function nil :scroll-bars nil
    ;; :background (clim:make-rgb-color 0 0 1/10)
    ;; :foregounrd clim:+white+
    :incremental-redisplay t
    ;; :background (clim:make-rgb-color 1 1 9/10)
    :min-height 30
    :min-width 30)
   (another clim-hunk-pane :display-function nil :scroll-bars nil
    ;; :background (clim:make-rgb-color 0 0 1/10)
    ;; :foregounrd clim:+white+
    :incremental-redisplay t
    :min-height 30
    :min-width 30
    )
   ;; (echo :application :display-function nil :scroll-bars nil)
   (echo clim-hunk-pane :scroll-bars nil :display-function nil :incremental-redisplay t
    :min-height 30))
  (:layouts
   (default
       (clim:vertically ()
         (1/2 main)
         ;; (clim:make-pane 'CLIM-EXTENSIONS:BOX-ADJUSTER-GADGET)
         ;; (1/2 another)
         (50 echo))))
  (:geometry :width 600 :height 600))

(defvar *clim-hemlock-process* nil)

(defun clim-hemlock ()
  (when *clim-hemlock-process*
    (clim-sys:destroy-process *clim-hemlock-process*))
  (setf *clim-hemlock-process*
        (clim-sys:make-process
         (lambda ()
           (clim:run-frame-top-level
            (clim:make-application-frame 'hemlock))))))

;; *editor-windowed-input* is hack and points to the display in CLX hemlock
;; *editor-input* is the real input stream.
;; who sets up *real-editor-input* ?

(defmethod clim:default-frame-top-level ((frame hemlock)
                                         &key
                                         (command-parser 'command-line-command-parser)
                                         (command-unparser 'command-line-command-unparser)
                                         (partial-command-parser 'command-line-read-remaining-arguments-for-partial-command)
                                         (prompt "Command: "))
  (declare (ignorable command-parser
                      command-unparser
                      partial-command-parser
                      prompt))
  (let ((clim:*application-frame* frame))
    (let ((*window-list* *window-list*)
          (*editor-input*
           (let ((e (hi::make-input-event)))
             (make-instance 'clim-editor-input
                            :stream (clim:frame-standard-input frame)
                            :head e :tail e))))
      (setf hi::*real-editor-input* *editor-input*) ;###
      (baba (clim:get-frame-pane frame 'main) ;; (clim:frame-standard-output frame)
            (clim:get-frame-pane frame 'echo)
            nil ;;(clim:get-frame-pane frame 'another)
            )
      ;;(eval '(trace device-put-cursor))
      ;;(eval '(trace clim:draw-text*))
      ;;(eval '(trace device-smart-redisplay device-dumb-redisplay hi::redisplay))
      (print (clim:get-frame-pane frame 'main) *trace-output*)
      (hi::%command-loop) )))

;;; Keysym translations

(defun clim-character-keysym (gesture)
  (cond
    ((eql gesture #\newline)            ;### hmm
     (hemlock-ext:key-event-keysym #k"Return"))
    ((eql gesture #\tab)            ;### hmm
     (hemlock-ext:key-event-keysym #k"Tab"))
    ((eql gesture #\Backspace)
     (hemlock-ext:key-event-keysym #k"Backspace"))
    ((eql gesture #\Escape)
     (hemlock-ext:key-event-keysym #k"Escape"))
    ((eql gesture #\rubout)
     (hemlock-ext:key-event-keysym #k"delete"))
    (t
     (char-code gesture))))

(defun clim-modifier-state-modifier-mask (state)
  (logior (if (not (zerop (logand clim:+control-key+ state)))
              (hemlock-ext:key-event-bits #k"control-a")
              0)
          (if (not (zerop (logand clim:+meta-key+ state)))
              (hemlock-ext:key-event-bits #k"meta-a")
              0)
          (if (not (zerop (logand clim:+super-key+ state)))
              (hemlock-ext:key-event-bits #k"super-a")
              0)
          (if (not (zerop (logand clim:+hyper-key+ state)))
              (hemlock-ext:key-event-bits #k"hyper-a")
              0)
          ;; hmm, these days there also is ALT.
          ))

(defparameter *keysym-map*
  '((:down "Downarrow")
    (:up   "Uparrow")
    (:left "Leftarrow")
    (:right "Rightarrow")
    (:next  "pagedown")
    (:prior "pageup")
    (:f1    "f1")
    (:escape "escape") ))

(defun gesture-key-event (gesture)
  "Given a CLIM gesture returns a Hemlock key-event or NIL, if there is none."
  (multiple-value-bind (char bits)
      (cond ((characterp gesture)
             (values (clim-character-keysym gesture) 0))
            ((typep gesture 'CLIM:KEY-PRESS-EVENT)
             (cond ((clim:keyboard-event-character gesture)
                    (values
                     (clim-character-keysym
                      (clim:keyboard-event-character gesture))
                     (clim:event-modifier-state gesture)))
                   (t
                    (let ((e (cadr (assoc (clim:keyboard-event-key-name gesture) *keysym-map*))))
                      (if e
                          (values (hemlock-ext:key-event-keysym (hemlock-ext:make-key-event e 0))
                                  (clim:event-modifier-state gesture))
                          (values nil nil))))))
            (t
             (values nil nil)))
    (cond ((and char bits)
           (hemlock-ext:make-key-event char
                                       (clim-modifier-state-modifier-mask bits)))
          (t
           '(describe gesture *trace-output*)
           nil))))

;;;;

(defun clim-window-changed (hunk)
  (let ((window (device-hunk-window hunk)))
    ;;
    ;; Nuke all the lines in the window image.
    (unless (eq (cdr (window-first-line window)) the-sentinel)
      (shiftf (cdr (window-last-line window))
              (window-spare-lines window)
              (cdr (window-first-line window))
              the-sentinel))
    ;### (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))
    ;;
    ;; Add some new spare lines if needed.  If width is greater,
    ;; reallocate the dis-line-chars.
    (let* ((res (window-spare-lines window))
           (new-width (max 5 (floor (- (clim:bounding-rectangle-width (clim-hunk-stream hunk))
                                       (* 2 *gutter*))
                                    (slot-value hunk 'cw))))
           (new-height (max 2 (1-
                               (floor (- (clim:bounding-rectangle-height (clim-hunk-stream hunk))
                                         (* 2 *gutter*))
                                      (slot-value hunk 'ch)))))
           (width (length (the simple-string (dis-line-chars (car res))))))
      (declare (list res))
      (when (> new-width width)
        (setq width new-width)
        (dolist (dl res)
          (setf (dis-line-chars dl) (make-string new-width))))
      (setf (window-height window) new-height (window-width window) new-width)
      (do ((i (- (* new-height 2) (length res)) (1- i)))
          ((minusp i))
        (push (make-window-dis-line (make-string width)) res))
      (setf (window-spare-lines window) res)
      ;;
      ;; Force modeline update.
      (let ((ml-buffer (window-modeline-buffer window)))
        (when ml-buffer
          (let ((dl (window-modeline-dis-line window))
                (chars (make-string new-width))
                (len (min new-width (window-modeline-buffer-len window))))
            (setf (dis-line-old-chars dl) nil)
            (setf (dis-line-chars dl) chars)
            (replace chars ml-buffer :end1 len :end2 len)
            (setf (dis-line-length dl) len)
            (setf (dis-line-flags dl) changed-bit)))))
    ;;
    ;; Prepare for redisplay.
    (setf (window-tick window) (tick))
    (update-window-image window)
    (when (eq window *current-window*) (maybe-recenter-window window))
    hunk))

(defun baba (stream echo-stream another-stream)
  (let* (
         (device (make-instance 'clim-device))
         (buffer *current-buffer*)
         (start (buffer-start-mark buffer))
         (first (cons dummy-line the-sentinel)) )
    (declare (ignorable start first))
    (setf (buffer-windows buffer) nil
          (buffer-windows *echo-area-buffer*) nil)
    (setf
     (device-name device) "CLIM"
     (device-bottom-window-base device) nil)
    (let* ((window (hi::internal-make-window))
           (hunk (make-instance 'clim-hunk :stream stream)))
      (baba-aux device window hunk buffer)
      (setf *current-window* window)
      (push window (slot-value device 'windows))
      (setf (device-hunks device) (list hunk)) )
    (when another-stream
      (let* ((window (hi::internal-make-window))
             (hunk (make-instance 'clim-hunk :stream another-stream)))
        (baba-aux device window hunk buffer)
        (push window (slot-value device 'windows))
        (push hunk (device-hunks device))))
    ;;
    (when echo-stream                   ;hmm
      (let ((echo-window (hi::internal-make-window))
            (echo-hunk (make-instance 'clim-hunk :stream echo-stream)))
        (baba-aux device echo-window echo-hunk *echo-area-buffer*)
        (setf *echo-area-window* echo-window)
        ;; why isn't this on the list of hunks?
        ;; List of hunks isn't used at all.
        ))
    ;;
    ))

(defun baba-aux (device window hunk buffer)
  (setf (slot-value (clim-hunk-stream hunk) 'hunk)
        hunk)
  (let* ((start (buffer-start-mark buffer))
         (first (cons dummy-line the-sentinel))
         width height)
    (setf
     (slot-value hunk 'ts) (clim:make-text-style :fix :roman 11.5)
     (slot-value hunk 'cw) (+ 0 (clim:text-size (clim-hunk-stream hunk) "m"
                                                    :text-style (slot-value hunk 'ts)))
     (slot-value hunk 'ch) (+ 2 (clim:text-style-height (slot-value hunk 'ts)
                                                        (clim-hunk-stream hunk)))
     width (max 5 (floor (- (clim:bounding-rectangle-width (clim-hunk-stream hunk))
                            (* 2 *gutter*))
                         (slot-value hunk 'cw)))
     height (max 2 (floor (- (clim:bounding-rectangle-height (clim-hunk-stream hunk))
                             (* 2 *gutter*))
                          (slot-value hunk 'ch)))
     (device-hunk-window hunk) window
     (device-hunk-position hunk) 0
     (device-hunk-height hunk) height
     (device-hunk-next hunk) nil
     (device-hunk-previous hunk) nil
     (device-hunk-device hunk) device

     (window-tick window) -1  ; The last time this window was updated.
     (window-%buffer window) buffer ; buffer displayed in this window.
     (window-height window) height      ; Height of window in lines.
     (window-width window) width  ; Width of the window in characters.

     (window-old-start window) (copy-mark start :temporary) ; The charpos of the first char displayed.
     (window-first-line window) first ; The head of the list of dis-lines.
     (window-last-line window) the-sentinel ; The last dis-line displayed.
     (window-first-changed window) the-sentinel ; The first changed dis-line on last update.
     (window-last-changed window) first ; The last changed dis-line.
     (window-spare-lines window) nil ; The head of the list of unused dis-lines

     (window-hunk window) hunk ; The device hunk that displays this window.

     (window-display-start window) (copy-mark start :right-inserting) ; first character position displayed
     (window-display-end window) (copy-mark start :right-inserting) ; last character displayed

     (window-point window) (copy-mark (buffer-point buffer)) ; Where the cursor is in this window.

     (window-modeline-dis-line window) nil ; Dis-line for modeline display.
     (window-modeline-buffer window) nil ; Complete string of all modeline data.
     (window-modeline-buffer-len window) nil ; Valid chars in modeline-buffer.

     (window-display-recentering window) nil ;
     )

    (loop for i from 32 below 126 do
          (let ((s (string (code-char i))))
            (let ((w (clim:text-size (clim-hunk-stream hunk) s
                                         :text-style (slot-value hunk 'ts))))
              (unless (= w 7)
                (print s *trace-output*)))))
    (finish-output *trace-output*)

    (baba-make-dis-lines window width height)

    (when t ;;modelinep
        (setup-modeline-image buffer window)
        #+NIL
        (setf (bitmap-hunk-modeline-dis-line hunk)
              (window-modeline-dis-line window)))

    (push window (buffer-windows buffer))
    (push window *window-list*)
    (hi::update-window-image window)))

(defun baba-make-dis-lines (window width height)
  (do ((i (- height) (1+ i))
       (res ()
            (cons (make-window-dis-line (make-string width)) res)))
      ((= i height)
       (setf (window-spare-lines window) res))))

;;;; Redisplay

(defvar *tick* 0)

(defmethod device-dumb-redisplay ((device clim-device) window)
  (clim-drop-cursor (window-hunk window))
  (let ((*standard-output* (clim-hunk-stream (window-hunk window))))
    (let ((w (clim:bounding-rectangle-width *standard-output*))
          (h (clim:bounding-rectangle-height *standard-output*)))
      (clim:updating-output (t :unique-id :static :cache-value h)
        (clim:draw-rectangle* *standard-output*
                              1 1
                              (- w 2) (- h 2)
                              :ink clim:+black+
                              :filled nil) ))
    (clim:with-text-style (*standard-output* (slot-value (window-hunk window) 'ts))
      (clim:updating-output (*standard-output*)
        (let* ((hunk (window-hunk window))
               (first (window-first-line window)))
          ;; (hunk-reset hunk)
          (do ((i 0 (1+ i))
               (dl (cdr first) (cdr dl)))
              ((eq dl the-sentinel)
               (setf (window-old-lines window) (1- i)))
            (clim-dumb-line-redisplay hunk (car dl)))
          (setf (window-first-changed window) the-sentinel
                (window-last-changed window) first)
          #+NIL                         ;###
          (when (window-modeline-buffer window)
            ;;(hunk-replace-modeline hunk)
            (clim:with-text-style (*standard-output* (clim:make-text-style :serif :italic 12))
              (clim-dumb-line-redisplay hunk
                                        (window-modeline-dis-line window)
                                        t))
            (setf (dis-line-flags (window-modeline-dis-line window))
                  unaltered-bits))
          #+NIL
          (setf (bitmap-hunk-start hunk) (cdr (window-first-line window))))))
    (clim:redisplay-frame-pane clim:*application-frame* *standard-output*)
    (clim-put-cursor (window-hunk window))
    ;;(force-output *standard-output*)
    (clim:medium-finish-output (clim:sheet-medium *standard-output*))
    ))

(defun clim-dumb-line-redisplay (hunk dl &optional modelinep)
  (let* ((stream (clim-hunk-stream hunk))
         (h (slot-value hunk 'ch))
         (w (slot-value hunk 'cw))
         (xo *gutter*)
         (yo *gutter*))
    (declare (ignorable stream))
    ;; (print dl *trace-output*)(finish-output *trace-output*)
    (unless (zerop (dis-line-flags dl))
      (setf (hi::dis-line-tick dl) (incf *tick*)))
    (let ((chrs (dis-line-chars dl)))
      (clim:updating-output (*standard-output* ;###
                             :unique-id (if modelinep :modeline (dis-line-position dl))
                             :id-test #'eq ;###
                             :cache-value (hi::dis-line-tick dl)
                             :cache-test #'eql)
        (let ((y (+ yo (* (dis-line-position dl) h))))
          (when modelinep
            (setf y (- (clim:bounding-rectangle-height *standard-output*)
                       h
                       2)))
          (clim:draw-rectangle* *standard-output*
                                (+ xo 0) y
                                (clim:bounding-rectangle-width *standard-output*) (+ y h)
                                :ink clim:+white+)
          ;; font changes
          (let ((font 0)                ;###
                (start 0)
                (end (dis-line-length dl))
                (changes (dis-line-font-changes dl)))
            (loop
                (cond ((null changes)
                       (clim-draw-text *standard-output* chrs
                                       (+ xo (* w start))
                                       (+ 1 y)
                                       start end font)
                       (return))
                      (t
                       (clim-draw-text *standard-output* chrs
                                       (+ xo (* w start))
                                       (+ 1 y)
                                       start (font-change-x changes) font)
                       (setf font (font-change-font changes)
                             start (font-change-x changes)
                             changes (font-change-next changes)))))) ))))
  (setf (dis-line-flags dl) unaltered-bits (dis-line-delta dl) 0))

(defun clim-draw-text (stream string x y start end font)
  (let ((ch (clim:text-style-height (clim:medium-text-style stream)
                                    stream))
        (dx (clim:stream-string-width stream string :start start :end end)))
    (clim:draw-rectangle* stream
                          x (1- y)
                          (+ x dx) (+ y ch 1) :ink (hemlock-font-background font)))
  (clim:draw-text* stream string x (+ y (clim:text-style-ascent (clim:medium-text-style stream)
                                                                stream))
                   :start start :end end
                   ;; :align-y :top ### :align-y is borken.
                   :ink (hemlock-font-foreground font))
  (when (= font 5)
    (let ((ch (clim:text-style-height (clim:medium-text-style stream)
                                      stream))
          (dx (clim:stream-string-width stream string :start start :end end)))
      (clim:draw-line* stream x (+ y ch -1) (+ x dx) (+ y ch -1)))) )

(defun clim-drop-cursor (hunk)
  (with-slots (cx cy cw ch) hunk
    (when (and cx cy)
      (clim:draw-rectangle* (clim:sheet-medium (clim-hunk-stream hunk))
                            (+ *gutter* (* cx cw))
                            (+ *gutter* (* cy ch))
                            (+ *gutter* (* (1+ cx) cw))
                            (+ *gutter* (* (1+ cy) ch))
                            :ink clim:+flipping-ink+))))

(defun clim-put-cursor (hunk)
  (with-slots (cx cy cw ch) hunk
    (when (and cx cy)
      (clim:draw-rectangle* (clim:sheet-medium (clim-hunk-stream hunk))
                            (+ *gutter* (* cx cw))
                            (+ *gutter* (* cy ch))
                            (+ *gutter* (* (1+ cx) cw))
                            (+ *gutter* (* (1+ cy) ch))
                            :ink clim:+flipping-ink+))))

(defun hi::editor-sleep (time)
  "Sleep for approximately Time seconds."
  (setf time 0)                         ;CLIM event processing still is messy.
  (unless (or (zerop time) (listen-editor-input *editor-input*))
    (hi::internal-redisplay)
    (hi::sleep-for-time time)
    nil))

(defun hi::sleep-for-time (time)
  (let ((device (device-hunk-device (window-hunk (current-window))))
        (end (+ (get-internal-real-time)
                (truncate (* time internal-time-units-per-second)))))
    (loop
      (when (listen-editor-input *editor-input*)
        (return))
      (let ((left (- end (get-internal-real-time))))
        (unless (plusp left) (return nil))
        (device-note-read-wait device t)
        (sleep .1)))
    (device-note-read-wait device nil)))

;;;

(defun hemlock-font-foreground (font)
  (case font
    (1 clim:+blue4+)
    (3 clim:+black+)
    (2 clim:+cyan4+)
    (4 clim:+green4+)
    (5 clim:+red4+)
    (6 clim:+gray50+)
    (otherwise clim:+black+)))

(defun hemlock-font-background (font)
  (case font
    (3 (clim:make-rgb-color 1 .9 .8))
    (otherwise clim:+white+)))

(defun hi::invoke-with-pop-up-display (cont buffer-name height)
  (funcall cont *trace-output*)
  (finish-output *trace-output*))

;;;;

(clim:define-application-frame layout-test ()
    ()
  (:panes
   (foo :application)
   (bar :application)
   (baz :interactor))
  (:layouts
   (default
       (clim:vertically ()
         foo
         bar
         baz))
   (dada
    (clim:vertically ()
      (10 foo)
      (10 bar)
      baz))))

#+NIL
(define-layout-test-command (com-foo :name t) ()
  (let* ((foo (CLIM-INTERNALS::FIND-PANE-FOR-LAYOUT 'foo clim:*application-frame*))
         (bar (CLIM-INTERNALS::FIND-PANE-FOR-LAYOUT 'bar clim:*application-frame*))
         (baz (CLIM-INTERNALS::FIND-PANE-FOR-LAYOUT 'baz clim:*application-frame*))
         (vbox (clim:sheet-parent foo))
         (vbox.parent (clim:sheet-parent vbox)))
    (clim:sheet-disown-child vbox.parent vbox)
    (clim:sheet-disown-child vbox foo)
    (clim:sheet-disown-child vbox bar)
    (clim:sheet-disown-child vbox baz)
    (clim:with-look-and-feel-realization
        ((clim:frame-manager clim:*application-frame*)
         clim:*application-frame*)
      (let ((vb (clim:make-pane 'clim:vrack-pane
                                :contents (list (list 100 foo)
                                                (list 100 bar)
                                                baz))))

        (clim:sheet-adopt-child vbox.parent vb)
        (setf (clim:sheet-enabled-p vb) t)
        (setf (clim:sheet-region vb)
              (clim:sheet-region vbox))
        (clim:allocate-space vb
                             (1- (clim:bounding-rectangle-width vb))
                             (clim:bounding-rectangle-height vb))
        (eval '(trace clim:allocate-space))
        (clim:layout-frame clim:*application-frame*
                           (clim:bounding-rectangle-width (clim:frame-top-level-sheet clim:*application-frame*))
                           (clim:bounding-rectangle-height (clim:frame-top-level-sheet clim:*application-frame*)))
        (eval '(untrace clim:allocate-space))
        ))))

(defparameter *app-process-hash* (make-hash-table))

(defun run (app)
  (when (gethash app *app-process-hash*)
    (clim-sys:destroy-process (gethash app *app-process-hash*)))
  (setf (gethash app *app-process-hash*)
        (clim-sys:make-process (lambda ()
                                 (clim:run-frame-top-level
                                  (clim:make-application-frame app))))))


#+NIL
(defparameter mcclim-freetype::*families/faces*
  '(;; ((:fix :roman) . "/var/lib/defoma/fontconfig.d/B/Bitstream-Vera-Serif.ttf")
    ((:fix :roman) . "/usr/share/fonts/truetype/ttf-bitstream-vera/VeraMono.ttf")
    ((:fix :italic) . "/usr/share/fonts/truetype/ttf-bitstream-vera/VeraMoIt.ttf")
    ((:fix :bold-italic) . "/usr/share/fonts/truetype/ttf-bitstream-vera/VeraMoBI.ttf")
    ((:fix :italic-bold) . "/usr/share/fonts/truetype/ttf-bitstream-vera/VeraMoBI.ttf")
    ((:fix :bold) . "/usr/share/fonts/truetype/ttf-bitstream-vera/VeraMoBd.ttf")

    ((:serif :roman) . "/usr/share/fonts/truetype/freefont/FreeSerif.ttf")
    ((:serif :italic) . "/usr/share/fonts/truetype/freefont/FreeSerifItalic.ttf")
    ((:serif :bold-italic) . "/usr/share/fonts/truetype/freefont/FreeSerifBoldItalic.ttf")
    ((:serif :italic-bold) . "/usr/share/fonts/truetype/freefont/FreeSerifBoldItalic.ttf")
    ((:serif :bold) . "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf")

    ((:sans-serif :roman) . "/usr/share/fonts/truetype/ttf-bitstream-vera/Vera.ttf")
    ((:sans-serif :italic) . "/usr/share/fonts/truetype/ttf-bitstream-vera/VeraIt.ttf")
    ((:sans-serif :bold-italic) . "/usr/share/fonts/truetype/ttf-bitstream-vera/VeraBI.ttf")
    ((:sans-serif :italic-bold) . "/usr/share/fonts/truetype/ttf-bitstream-vera/VeraBI.ttf")
    ((:sans-serif :bold) . "/usr/share/fonts/truetype/ttf-bitstream-vera/VeraBd.ttf")
    ))



;; $Log: foo.lisp,v $
;; Revision 1.6  2004-12-27 18:53:20  gbaumann
;; half-way working undo
;;
;; Revision 1.5  2004/12/15 12:16:43  crhodes
;; Make clim-hemlock basically work on sbcl -- mostly build fixes from Hannu
;; Koivisto.
;;
;; * don't declaim or declare stuff in CL special;
;; * classes come before methods specializing on them;
;; * clim-sys: not mp:
;;
;; Revision 1.4  2004/11/21 01:03:51  gbaumann
;; Basic support for c-x 1 and c-x 2.
;;



