;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM Phemlock
;;;   Created: 2004-11-20       <- not true!
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2003, 2004 by Gilbert Baumann

(in-package :clim-hemlock)

;;;; RANDOM NOTES

;; Perhaps Hemlock should function as a frame manager too, so that you can
;; embed say a listen into Hemlocks main application frame. Or goodies
;; written by third parties like a side bar. Event processing then becomes
;; tricky and keyboard events are about to under focus control and mouse
;; event are about to be under pointer control. And: CLIM won't cope with a
;; single application frame been displayed more than once like Emacs can
;; display a buffer more than once. But: This is perhaps even possible
;; thru' some glorious kludges.

;; How exactly Hemlock can be integrated as the McCLIM line editor is still
;; an open question. Also: If Hemlock functions as a line editor or as a
;; text-field gadget, we surely want to operate in some restricted mode
;; where we can't switch buffers. And line editing buffers and text-field
;; buffers should be hidden. => Notion of a session.

;; - DEVICE-HUNKS doesn't seem to be used anywhere beyond device
;;   implementations.
;; - DEVICE-BOTTOM-WINDOW-BASE seems to be only used from
;;   tty-screen.lisp.

;;;; HEMLOCK AS GADGET

;; - creating new windows can easily been forbidden by just making
;;   DEVICE-MAKE-WINDOW fail.
;; - How can switching buffers be forbidden?

(defclass clim-device (device)
  (;; cursor
   (cursor-hunk :initform nil
                :documentation "The hunk that has the cursor.")
   (windows :initform nil
            )
   ))

(defmethod device-init ((device clim-device))
  )

(defmethod device-exit ((device clim-device)))

(defmethod device-smart-redisplay ((device clim-device) window)
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
      (setf (buffer-windows buffer) (delete window (buffer-windows buffer))))
    )
  )

(defmethod device-make-window ((device clim-device) start modelinep window font-family
                               ask-user x y width-arg height-arg proportion
                               &aux res)
  (print (list start modelinep window font-family ask-user x y width-arg height-arg proportion)
         *trace-output*)
  (finish-output *trace-output*)
  (let* ((hunk (window-hunk *current-window*))
         (stream (clim-hunk-stream hunk))
         (parent (clim:sheet-parent stream)))
    (print parent *trace-output*)
    (print (clim:sheet-children parent) *trace-output*)
    (clim:with-look-and-feel-realization ((clim:frame-manager clim:*application-frame*)
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
                          (subseq (slot-value device 'windows) p))))
          )
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
          (print (clim:sheet-children parent) *trace-output*)
          (print (append (subseq q 0 p)
                         (list new)
                         (subseq q p))
                 *trace-output*)
          (setf (clim:sheet-enabled-p new) t)
          ))
      )
    (finish-output *trace-output*))
  res)

(defmethod clim:handle-repaint :around ((pane clim-hunk-pane) region)
  (let ((device (device-hunk-device (slot-value pane 'hunk))))
    (with-slots (cursor-hunk) device
      (when cursor-hunk
        (clim-drop-cursor cursor-hunk)))
    (call-next-method)
    (with-slots (cursor-hunk) device
      (when cursor-hunk
        (clim-put-cursor cursor-hunk))))
  (clim:draw-line* (clim:sheet-medium pane)
                   0 (- (clim:bounding-rectangle-height pane) 1)
                   (clim:bounding-rectangle-width pane)
                   (- (clim:bounding-rectangle-height pane) 1)) )


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

(defclass clim-hunk-pane (CLIM:APPLICATION-PANE)
  ((hunk)
   ))

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

(clim:define-application-frame hemlock ()
    ()
  (:pointer-documentation t)
  (:menu-bar nil)
  (:panes
   (main clim-hunk-pane :display-function nil :scroll-bars nil
    ;; :background (clim:make-rgb-color 0 0 1/10)
    ;; :foregounrd clim:+white+
    :incremental-redisplay t
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
  (:geometry :width 600 :height 800))

(defvar *clim-hemlock-process* nil)

(defun clim-hemlock ()
  (when *clim-hemlock-process*
    (mp:destroy-process *clim-hemlock-process*))
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
                                       10)
                                    (slot-value hunk 'cw))))
           (new-height (max 2 (floor (- (clim:bounding-rectangle-height (clim-hunk-stream hunk))
                                        10)
                                     (slot-value hunk 'ch))))
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
    (let ((echo-window (hi::internal-make-window))
          (echo-hunk (make-instance 'clim-hunk :stream echo-stream)))
      (baba-aux device echo-window echo-hunk *echo-area-buffer*)
      (setf *echo-area-window* echo-window)
      ;; why isn't this on the list of hunks?
      ;; List of hunks isn't used at all.
      )
    ;;
    ))

(defun baba-aux (device window hunk buffer)
  (setf (slot-value (clim-hunk-stream hunk) 'hunk)
        hunk)
  (let* ((start (buffer-start-mark buffer))
         (first (cons dummy-line the-sentinel))
         width height)
    (setf
     (slot-value hunk 'ts) (clim:make-text-style :fix :roman 12)
     (slot-value hunk 'cw) (clim:text-style-width (slot-value hunk 'ts) (clim-hunk-stream hunk))
     (slot-value hunk 'ch) (+ 2 (clim:text-style-height (slot-value hunk 'ts)
                                                        (clim-hunk-stream hunk)))
     width (max 5 (floor (- (clim:bounding-rectangle-width (clim-hunk-stream hunk))
                            10)
                         (slot-value hunk 'cw)))
     height (max 2 (floor (- (clim:bounding-rectangle-height (clim-hunk-stream hunk))
                             10)
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

    (baba-make-dis-lines window width height)

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
          #+NIL
          (when (window-modeline-buffer window)
            (hunk-replace-modeline hunk)
            (setf (dis-line-flags (window-modeline-dis-line window))
                  unaltered-bits))
          #+NIL
          (setf (bitmap-hunk-start hunk) (cdr (window-first-line window))))))
    (clim:redisplay-frame-pane clim:*application-frame* *standard-output*))
  (clim-put-cursor (window-hunk window))
  (force-output *standard-output*) )

(defun clim-dumb-line-redisplay (hunk dl)
  (let* ((stream (clim-hunk-stream hunk))
         (h (slot-value hunk 'ch))
         (w (slot-value hunk 'cw))
         (xo 5)
         (yo 5))
    (declare (ignorable stream))
    ;; (print dl *trace-output*)(finish-output *trace-output*)
    (unless (zerop (dis-line-flags dl))
      (setf (hi::dis-line-tick dl) (incf *tick*)))
    (let ((chrs (dis-line-chars dl)))
      (clim:updating-output (*standard-output* ;###
                             :unique-id (dis-line-position dl)
                             :cache-value (hi::dis-line-tick dl)
                             :cache-test #'eql)
        (clim:draw-rectangle*
         *standard-output*
         (+ xo 0)
         (+ yo (* (dis-line-position dl) h))
         (+ xo 800)
         (+ yo (* (1+ (dis-line-position dl)) h))
         :ink clim:+white+)
        ;; font changes
        (let ((font 0)                 ;###
              (start 0)
              (end (dis-line-length dl))
              (changes (dis-line-font-changes dl)))
          (loop
              (cond ((null changes)
                     (clim-draw-text *standard-output* chrs
                                     (+ xo (* w start))
                                     (+ yo 1 (* (dis-line-position dl) h))
                                     start end font)
                     (return))
                    (t
                     (clim-draw-text *standard-output* chrs
                                     (+ xo (* w start))
                                     (+ yo 1 (* (dis-line-position dl) h))
                                     start (font-change-x changes) font)
                     (setf font (font-change-font changes)
                           start (font-change-x changes)
                           changes (font-change-next changes)))))) )))
  (setf (dis-line-flags dl) unaltered-bits (dis-line-delta dl) 0))

(defun clim-draw-text (stream string x y start end font)
  (let ((ch (clim:text-style-height (clim:medium-text-style stream)
                                    stream))
        (dx (clim:stream-string-width stream string :start start :end end)))
    (clim:draw-rectangle* stream
                          x (1- y)
                          (+ x dx) (+ y ch 1) :ink (hemlock-font-background font)))
  (clim:draw-text* stream string x y
                   :start start :end end
                   :align-y :top
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
                            (+ 5 (* cx cw))
                            (+ 5 (* cy ch))
                            (+ 5 (* (1+ cx) cw))
                            (+ 5 (* (1+ cy) ch))
                            :ink clim:+flipping-ink+))))

(defun clim-put-cursor (hunk)
  (with-slots (cx cy cw ch) hunk
    (when (and cx cy)
      (clim:draw-rectangle* (clim:sheet-medium (clim-hunk-stream hunk))
                            (+ 5 (* cx cw))
                            (+ 5 (* cy ch))
                            (+ 5 (* (1+ cx) cw))
                            (+ 5 (* (1+ cy) ch))
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

;; $Log: foo.lisp,v $
;; Revision 1.4  2004-11-21 01:03:51  gbaumann
;; Basic support for c-x 1 and c-x 2.
;;

