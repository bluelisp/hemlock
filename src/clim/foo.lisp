;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

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

(defclass clim-device (device)
  (;; cursor
   (cursor-hunk :initform nil
                :documentation "The hunk that has the cursor.")))

(defmethod device-init ((device clim-device))
  )

(defmethod device-make-window ((device clim-device) start modelinep window font-family
                                ask-user x y width-arg height-arg proportion))

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

(defmethod device-next-window ((device clim-device) window)
  )

(defmethod device-previous-window ((device clim-device) window)
  )

(defmethod device-delete-window ((device clim-device) window)
  )

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
   (ts)
   ))

;;; Input

(defclass clim-editor-input (editor-input)
  ((stream :initarg :stream
           :reader clim-editor-input-stream
           :documentation "Extended input stream we read from.")) )

(defmethod get-key-event ((stream clim-editor-input) &optional ignore-abort-attempts-p)
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

;; For now a hemlock window and hunk is paralleled in a pane.

(clim:define-application-frame hemlock ()
    ()
  (:pointer-documentation t)
  (:menu-bar nil)
  (:panes
   (main :application :display-function nil :scroll-bars nil
    ;; :background (clim:make-rgb-color 0 0 1/10)
    ;; :foregounrd clim:+white+
    :incremental-redisplay t)
   ;; (echo :application :display-function nil :scroll-bars nil)
   (io   :interactor))
  (:layouts
   (default
       (clim:vertically (:width 815)
         (510 main)
         ;; (100 echo)
         (100 io))))
  (:geometry :width 600 :height 800))

(defun clim-hemlock ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'hemlock)))

(defparameter *sheet* nil)

;; *editor-windowed-input* is hack and points to the display in CLX hemlock
;; *editor-input* is the real input stream.
;; who sets up *real-editor-input* ?


(defmethod clim:default-frame-top-level ((frame hemlock)
                                         &key
                                         (command-parser 'command-line-command-parser)
                                         (command-unparser 'command-line-command-unparser)
                                         (partial-command-parser 'command-line-read-remaining-arguments-for-partial-command)
                                         (prompt "Command: "))
  (let ((clim:*application-frame* frame))
    (setf *sheet* (clim:frame-standard-output frame))
    (let ((*window-list* *window-list*)
          (*editor-input*
           (let ((e (hi::make-input-event)))
             (make-instance 'clim-editor-input
                            :stream (clim:frame-standard-input frame)
                            :head e :tail e))))
      (setf hi::*real-editor-input* *editor-input*) ;###
      (baba (clim:frame-standard-output frame)
            (clim:frame-query-io frame))
      (print *current-window*)
      (print *current-buffer*)
      (finish-output)
      ;;(eval '(trace device-put-cursor))
      ;;(eval '(trace clim:draw-text*))
      ;;(eval '(trace device-smart-redisplay device-dumb-redisplay hi::redisplay))
      #+NIL
      (loop
          (print (clim:read-gesture :stream (clim:frame-standard-input frame))
                 (clim:frame-standard-output frame)))
      (hi::%command-loop)
      )))

(defun clim-character-keysym (gesture)
  (cond
    ((eql gesture #\newline)            ;### hmm
     (hemlock-ext:KEY-EVENT-KEYSYM #k"Return"))
    ((eql gesture #\tab)            ;### hmm
     (hemlock-ext:KEY-EVENT-KEYSYM #k"Tab"))
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
    (:escape "escape")
    ))

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


;;;;;;;;;;;;;

#+NIL
(defun window-for-hunk (hunk start modelinep)
  (check-type start mark)
  (setf (bitmap-hunk-changed-handler hunk) #'window-changed)
  (let ((buffer (line-buffer (mark-line start)))
        (first (cons dummy-line the-sentinel))
        (width (bitmap-hunk-char-width hunk))
        (height (bitmap-hunk-char-height hunk)))
    (when (or (< height minimum-window-lines)
              (< width minimum-window-columns))
      (error "Window too small."))
    (unless buffer (error "Window start is not in a buffer."))
    (let ((window
           (internal-make-window
            :hunk hunk
            :display-start (copy-mark start :right-inserting)
            :old-start (copy-mark start :temporary)
            :display-end (copy-mark start :right-inserting)
            :%buffer buffer
            :point (copy-mark (buffer-point buffer))
            :height height
            :width width
            :first-line first
            :last-line the-sentinel
            :first-changed the-sentinel
            :last-changed first
            :tick -1)))
      (push window *window-list*)
      (push window (buffer-windows buffer))
      ;;
      ;; Make the dis-lines.
      (do ((i (- height) (1+ i))
           (res ()
                (cons (make-window-dis-line (make-string width)) res)))
          ((= i height) (setf (window-spare-lines window) res)))
      ;;
      ;; Make the image up to date.
      (update-window-image window)
      (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))
      ;;
      ;; If there is a modeline, set it up.
      (when modelinep
        (setup-modeline-image buffer window)
        (setf (bitmap-hunk-modeline-dis-line hunk)
              (window-modeline-dis-line window)))
      window)))

#||
(defun window-changed (hunk)
  (let ((window (bitmap-hunk-window hunk)))
    ;;
    ;; Nuke all the lines in the window image.
    (unless (eq (cdr (window-first-line window)) the-sentinel)
      (shiftf (cdr (window-last-line window))
              (window-spare-lines window)
              (cdr (window-first-line window))
              the-sentinel))
    (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))
    ;;
    ;; Add some new spare lines if needed.  If width is greater,
    ;; reallocate the dis-line-chars.
    (let* ((res (window-spare-lines window))
           (new-width (bitmap-hunk-char-width hunk))
           (new-height (bitmap-hunk-char-height hunk))
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
||#

(defun clim-window-changed (hunk)
  (let ((window (device-hunk-window hunk)))
    ;;
    ;; Nuke all the lines in the window image.
    (unless (eq (cdr (window-first-line window)) the-sentinel)
      (shiftf (cdr (window-last-line window))
              (window-spare-lines window)
              (cdr (window-first-line window))
              the-sentinel))
    ;; (setf (device-hunk-start hunk) (cdr (window-first-line window)))
    #||
    ;;
    ;; Add some new spare lines if needed.  If width is greater,
    ;; reallocate the dis-line-chars.
    (let* ((res (window-spare-lines window))
           (new-width (bitmap-hunk-char-width hunk))
           (new-height (bitmap-hunk-char-height hunk))
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
    ||#
    ;;
    ;; Prepare for redisplay.
    (setf (window-tick window) (tick))
    (update-window-image window)
    (when (eq window *current-window*) (maybe-recenter-window window))
    hunk))

(defun baba (stream echo-stream)
  (let* ((window (hi::internal-make-window))
         (hunk (make-instance 'clim-hunk :stream stream))
         (echo-window (hi::internal-make-window))
         (echo-hunk (make-instance 'clim-hunk :stream echo-stream))
         (device (make-instance 'clim-device))
         (buffer *current-buffer*)
         (start (buffer-start-mark buffer))
         (first (cons dummy-line the-sentinel)) )

    (setf (slot-value hunk 'ts) (clim:make-text-style :fixed :roman :normal))
    '(setf (slot-value hunk 'ts) (clim:make-device-font-text-style
                                 (clim:port stream)
                                 "-*-lucidatypewriter-medium-r-*-*-*-120-*-*-*-*-iso8859-1"))
    (setf (slot-value hunk 'cw) (clim:text-style-width (slot-value hunk 'ts)
                                                           (clim-hunk-stream hunk)))
    (setf (slot-value hunk 'ch) (+ 2 (clim:text-style-height (slot-value hunk 'ts)
                                                             (clim-hunk-stream hunk))))
    (setf (slot-value echo-hunk 'ts) (clim:make-text-style :fix :roman 12))
    (setf (slot-value echo-hunk 'cw) (clim:text-style-width (slot-value echo-hunk 'ts)
                                                       (clim-hunk-stream echo-hunk)))
    (setf (slot-value echo-hunk 'ch) (+ 2 (clim:text-style-height (slot-value echo-hunk 'ts)
                                                             (clim-hunk-stream echo-hunk))))

    (setf
     (device-name device) "CLIM"
     (device-bottom-window-base device) nil
     (device-hunks device) (list hunk))

    (baba-aux device window hunk buffer
              ;;(floor 800 (slot-value hunk 'cw))
              120
              (floor 500 (slot-value hunk 'ch)))
    (baba-aux device echo-window echo-hunk *echo-area-buffer* 80 2)
    (setf *echo-area-window* echo-window)

    (setf *current-window* window) ))

(defun baba-aux (device window hunk buffer width height)
  (let* ((start (buffer-start-mark buffer))
         (first (cons dummy-line the-sentinel)))
    (setf
     (device-hunk-window hunk) window
     (device-hunk-position hunk) 0
     (device-hunk-height hunk) height
     (device-hunk-next hunk) nil
     (device-hunk-previous hunk) nil
     (device-hunk-device hunk) device

     (window-tick window) -1            ; The last time this window was updated.
     (window-%buffer window) buffer     ; buffer displayed in this window.
     (window-height window) height      ; Height of window in lines.
     (window-width window) width        ; Width of the window in characters.

     (window-old-start window) (copy-mark start :temporary) ; The charpos of the first char displayed.
     (window-first-line window) first   ; The head of the list of dis-lines.
     (window-last-line window) the-sentinel ; The last dis-line displayed.
     (window-first-changed window) the-sentinel ; The first changed dis-line on last update.
     (window-last-changed window) first ; The last changed dis-line.
     (window-spare-lines window) nil    ; The head of the list of unused dis-lines

     (window-hunk window) hunk          ; The device hunk that displays this window.

     (window-display-start window) (copy-mark start :right-inserting) ; first character position displayed
     (window-display-end window) (copy-mark start :right-inserting) ; last character displayed

     (window-point window) (copy-mark (buffer-point buffer)) ; Where the cursor is in this window.

     (window-modeline-dis-line window) nil ; Dis-line for modeline display.
     (window-modeline-buffer window) nil ; Complete string of all modeline data.
     (window-modeline-buffer-len window) nil ; Valid chars in modeline-buffer.

     (window-display-recentering window) nil ;
     )

    ;;
    ;; Make the dis-lines.
    (do ((i (- height) (1+ i))
         (res ()
              (cons (make-window-dis-line (make-string width)) res)))
        ((= i height) (setf (window-spare-lines window) res)))

    (setf (buffer-windows buffer)
          (list window))
    (push window *window-list*)
    (hi::update-window-image window)))

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
    (clim:redisplay-frame-pane clim:*application-frame* *standard-output*)
    )
  (clim-put-cursor (window-hunk window))
  (force-output *standard-output*)
  )

(defun clim-dumb-line-redisplay (hunk dl)
  (let* ((stream (clim-hunk-stream hunk))
         (h (slot-value hunk 'ch))
         (w (slot-value hunk 'cw))
         (xo 5)
         (yo 5))
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
                           changes (font-change-next changes))))))
        ;;
        )))
  (setf (dis-line-flags dl) unaltered-bits (dis-line-delta dl) 0))

(defun clim-draw-text (stream string x y start end font)
  (clim:draw-text* stream string x y
                   :start start :end end
                   :align-y :top
                   :ink (case font
                          (1 clim:+blue4+)
                          (3 clim:+blue4+)
                          (2 clim:+cyan4+)
                          (4 clim:+green4+)
                          (5 clim:+red4+)
                          (6 clim:+gray50+)
                          (otherwise clim:+black+)))
  (when (= font 5)
    (let ((ch (clim:text-style-height (clim:medium-text-style stream)
                                      stream))
          (dx (clim:stream-string-width stream string :start start :end end)))
    (clim:draw-line* stream x (+ y ch -1) (+ x dx) (+ y ch -1))))
  )

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

(defun line-syntax-info (line)
  (getf (line-plist line) 'syntax-info-4))

(defun (setf line-syntax-info) (value line)
  (setf (getf (line-plist line) 'syntax-info-4) value))

(defun hi::ensure-syntax-marks (line)
  (let ((si (line-syntax-info line)))
    (cond ((null si)
           (setf si
                 (setf (line-syntax-info line)
                       (cons :frob nil)))))
    (setf (line-syntax-info line)
          (ensure-syntax-marks-2 line si))))

(defun ensure-syntax-marks-2 (line si)
  (destructuring-bind (signature . font-marks) si
    (cond ((eq signature (line-signature line))
           si)
          (t
           ;; work to do
           ;; 1. remove font marks
           (dolist (fm font-marks)
             (hi::delete-font-mark fm))
           (setf font-marks nil)
           (let ((in-string-p nil)
                 (in-comment-p nil))
             (loop for p from 0 below (line-length line) do
                   (cond ((char= (line-character line p) #\")
                          (unless in-comment-p
                            (if in-string-p
                                (push (hi::font-mark line p 0) font-marks)
                                (push (hi::font-mark line (1+ p) 2) font-marks))
                            (setf in-string-p (not in-string-p))))
                         ((char= (line-character line p) #\;)
                          (unless (or in-string-p in-comment-p)
                            (setf in-comment-p t)
                            (push (hi::font-mark line p 1) font-marks))))))
           (cons (line-signature line) font-marks)
           ))))

;; second approach:
;; syntax-info: (signature start-state end-state font-marks)
;;

(defun empty-syntax-info ()
  (list :frob nil nil nil))

(defun hi::ensure-syntax-marks (line)
  (let ((si (line-syntax-info line)))
    (cond ((null si)
           (setf si
                 (setf (line-syntax-info line) (empty-syntax-info)))))
    (setf (line-syntax-info line)
          (ensure-syntax-marks-2 line si))))

(defun line-syntax-info* (line)
  (cond ((null line)
         (list :frob nil (list nil) nil))
        (t
         (hi::ensure-syntax-marks line))))

(defun ensure-syntax-marks-2 (line si)
  (destructuring-bind (signature start end font-marks) si
    (let ((prev-end (third (line-syntax-info* (line-previous line)))))
      (cond ((and (eq signature (line-signature line))
                  (equal start prev-end))
             ;; no work
             si)
            (t
             ;; work to do, but first remove old font marks
             (dolist (fm font-marks)
               (hi::delete-font-mark fm))
             (setf font-marks nil)
             ;; now do the highlighting
             (let ((in-string-p (first prev-end))
                   (in-comment-p nil))
               (when in-string-p
                 (push (hi::font-mark line 0 2) font-marks))
               (loop for p from 0 below (line-length line) do
                     (unless (and (> p 0)
                                  (char= (line-character line (1- p)) #\\))
                       (cond ((char= (line-character line p) #\")
                              (unless in-comment-p
                                (if in-string-p
                                    (push (hi::font-mark line p 0) font-marks)
                                    (push (hi::font-mark line (1+ p) 2) font-marks))
                                (setf in-string-p (not in-string-p))))
                             ((char= (line-character line p) #\;)
                              (unless (or in-string-p in-comment-p)
                                (setf in-comment-p t)
                                (push (hi::font-mark line p 1) font-marks))))))
               (print (list :ending :with (list in-string-p)) *trace-output*)
               ;; return new info
               (list (line-signature line)
                     prev-end
                     (list in-string-p)
                     font-marks) ))))))


;;;; ------------------------------------------------------------------------------------------
;;;; Syntax Highlighting
;;;;

;; This still is only proof of concept.

;; We define highlighting by parsing the buffer content with a simple
;; recursive descend parser. The font attributes for each character are
;; then derived from the parser state. Each line remembers the start and
;; end parser state for caching. If the start parser state is the same as
;; the end parser state of the previous line no reparsing needs to be done.
;; Lines can change and if a line changes its end parser state is
;; considered to be unknown. So if you change a line syntax highlighting of
;; all following lines is potentially invalid. We avoid reparsing all of
;; the rest of the buffer by three means: First we access syntax markup in
;; a lazy fashion; if a line isn't displayed we don't need its syntax
;; markup. Second when while doing reparsing the newly computed end state
;; is the same as the old end state reparsing stops, because this end state
;; then matches the start state of the next line. Third when seeing an open
;; paren in the very first column, we assume that a new top-level
;; expression starts.

;; These recursive descend parsers are written in a mini language which
;; subsequently is compiled to some "byte" code and interpreted by a
;; virtual machine. For now we don't allow for parameters or return values
;; of procedures and so a state boils down to the current procedure, the
;; instruction pointer and the stack of saved activations.

;; This mini language allows to define procedures. Within a body of a
;; procedure the following syntax applies:

;; stmt -> (IF <cond> <tag>)     If <cond> evaluates to true, goto <tag>.
;;                               <cond> can be any lisp expression and has
;;                               the current look-ahead character available
;;                               in the variable 'ch'.
;;         <tag>                 A symbol serving as the target for GOs.
;;         (GO <tag>)            Continue execution at the indicated label.
;;         (CONSUME)             Consume the current lookahead character and
;;                               read the next one putting it into 'ch'.
;;         (CALL <proc>)         Call another procedure
;;         (RETURN)              Return from the current procedure

;; What the user sees is a little different. The function ME expands its
;; input to the above language. Added features are:

;; (IF <cond> <cons> [<alt>])    IF is modified to take statements instead
;;                               of branch targets
;; (PROGN {<stmt>}*)             Mainly because of IF, PROGN is introduced.
;;                               Note that the body can defined new branch
;;                               targets, which also are available from outside
;;                               of it.
;; (WHILE <cond> {<stmt>}*)
;; (COND {(<cond> {<stmt>}*)}*)

;; This mini-language for now is enough to write interesting recursive
;; descend parsers.

(eval-when (compile eval load)
  (defun me (form)
    (cond ((atom form)
           (list form))
          (t
           (ecase (car form)
             ((IF)
              (destructuring-bind (cond cons &optional alt) (cdr form)
                (let ((L1 (gensym "L."))
                      (L2 (gensym "L.")))
                  (append (list `(IF (not ,cond) ,L1))
                          (me cons)
                          (list `(GO ,L2))
                          (list L1)
                          (and alt (me alt))
                          (list L2)))))
             ((WHILE)
              (destructuring-bind (cond &rest body) (cdr form)
                (let ((exit (gensym "EXIT."))
                      (loop (gensym "LOOP.")))
                  (append (list loop)
                          (list `(if (not ,cond) ,exit))
                          (me `(progn ,@body))
                          (list `(go ,loop))
                          (list exit)))))
             ((COND)
              (cond ((null (cdr form)) nil)
                    (t
                     (me
                      `(if ,(caadr form) (progn ,@(cdadr form))
                        (cond ,@(cddr form)))))))
             ((CONSUME RETURN) (list form))
             ((PROGN) (mapcan #'me (cdr form)))
             ((GO) (list form))
             ((CALL) (list form))))))

  (defun ass (stmts)
    (let ((ip 0)
          (labels nil)
          (fixups nil)
          (code (make-array 0 :fill-pointer 0 :adjustable t)))
      (loop for stmt in stmts
            do
            (cond ((atom stmt)
                   (push (cons stmt ip) labels))
                  ((eq (car stmt) 'go)
                   (vector-push-extend :go code) (incf ip)
                   (push ip fixups)
                   (vector-push-extend (cadr stmt) code) (incf ip))
                  ((eq (car stmt) 'if)
                   (vector-push-extend :if code) (incf ip)
                   (vector-push-extend `(lambda (ch) (declare (ignorable ch)) ,(cadr stmt))
                                       code)
                   (incf ip)
                   (push ip fixups)
                   (vector-push-extend (caddr stmt) code) (incf ip))
                  ((eq (car stmt) 'call)
                   (vector-push-extend :call code) (incf ip)
                   (vector-push-extend `',(cadr stmt) code) (incf ip))
                  ((eq (car stmt) 'consume)
                   (vector-push-extend :consume code) (incf ip))
                  ((eq (car stmt) 'return)
                   (vector-push-extend :return code) (incf ip))
                  (t
                   (incf ip)
                   (vector-push-extend stmt code))))
      (loop for fixup in fixups do
            (let ((q (cdr (assoc (aref code fixup) labels))))
              (unless q
                (error "Undefined label ~S." (aref code fixup)))
              (setf (aref code fixup) q)))
      code)))

(defmacro defstate (name stuff &rest body)
  stuff
  `(setf (gethash ',name *parsers*)
         (vector ,@(coerce (ass (append (me `(progn ,@body))
                                        (list '(return))))
                           'list))))

(defvar *parsers* (make-hash-table))

(defstate initial ()
  (while t
    (call sexp)))

(defstate comment ()
  loop
  (cond ((char= ch #\newline)
         (consume)
         (return))
        (t
         (consume)
         (go loop))))

(defstate bq ()
  (consume)                             ;consume `
  (call sexp))

(defstate uq ()
  (consume)                             ;consume `
  (call sexp))

(defstate sexp ()
  loop
  (call skip-white*)                    ;skip possible white space and comments
  (cond ((char= ch #\() (call list))
        ((char= ch #\`) (call bq))
        ((char= ch #\') (call bq))
        ((char= ch #\,) (call uq))
        ((char= ch #\;) (call comment))
        ((char= ch #\") (call string))
        ((char= ch #\#) (call hash))
        ((or (alphanumericp ch) (find ch "-+*/"))
         (call atom))
        (t
         ;; hmm
         (consume)
         (go loop))))

(defstate hash ()
  (consume)
  (cond ((char= ch #\\) (call char-const))
        ((char= ch #\+) (call hash-plus))
        ((char= ch #\')
         (consume)
         (call sexp))
        (t
         (call sexp))))

(defstate char-const ()
  (consume)                             ;\\
  (cond ((or (alphanumericp ch) (find ch "-+*/"))
         (call atom))
        (t
         (consume))))

(defstate string ()
  (consume)
  (while t
    (cond ((char= ch #\\)
           (consume)
           (consume))
          ((char= ch #\")
           (consume)
           (return))
          (t
           (consume)))))

(defstate atom ()
  (while (or (alphanumericp ch) (find ch "-+*/"))
    (consume)))

(defstate list ()
  (consume)                             ;consume open-paren
  (while t
    (call skip-white*)                  ;skip possible white space
    (cond ((char= ch #\))
           (consume)
           (return))
          (t
           (call sexp)))))

(defstate skip-white* ()
  loop
  (while (member ch '(#\space #\tab #\newline #\return #\page))
    (consume))
  (cond ((char= ch #\;)
         (call comment)
         (go loop))
        (t
         (return))))

(defstate hash-plus ()
  (consume)                             ;#\+
  (call sexp)                                ;cond
  (call sexp)                                ;form
  )

;; --------------------

(defun step** (state char)
  (let* (fun ip code)
    (labels ((fetch ()
               (prog1 (aref code ip) (incf ip)))
             (sync (fun* ip*)
               (setf fun fun*
                     ip  ip*
                     code (or (gethash fun *parsers*)
                              (error "No such fun: ~S." fun))))
             (exit ()
               (sync (pop state) (pop state)))
             (save ()
               (push ip state)
               (push fun state)))
      (exit)
      (loop
          (ecase (fetch)
            (:IF
             (let ((cond (fetch))
                   (target (fetch)))
               (when (funcall cond char)
                 (setf ip target))))
            (:CONSUME
             (save)
             (return-from step** state))
            (:RETURN
             '(print (list :return state) *trace-output*)
             (exit)
             ;;(print (list :dada state))
             )
            (:CALL
             (let ((new-fun (fetch)))
               '(print (list :call new-fun) *trace-output*)
               (save)
               (sync new-fun 0)))
            (:GO
             (setf ip (fetch))))))))

(defun dodo (string)
  (let ((state (list 'initial 0)))
    (loop for c across string do
          (setf state (step** state c))
          (let ((q (member-if (lambda (x) (member x '(string bq uq comment))) state)))
            (case (car q)
              (comment (format t "/~A" c))
              (bq (princ (char-upcase c)))
              (uq (princ c))
              ((nil) (princ c)))))
    state))

;;;;;;;;;;;;;

(defun empty-syntax-info ()
  (list :frob nil (list 'initial 0) nil))

(defun hi::ensure-syntax-marks (line)
  (let ((si (line-syntax-info line)))
    (cond ((null si)
           (setf si
                 (setf (line-syntax-info line) (empty-syntax-info)))))
    (setf (line-syntax-info line)
          (ensure-syntax-marks-2 line si))))

(defun line-syntax-info* (line)
  (cond ((null line)
         (empty-syntax-info))
        (t
         (hi::ensure-syntax-marks line))))

(defun ensure-syntax-marks-2 (line si)
  (destructuring-bind (signature start end font-marks) si
    (let ((prev-end (third (line-syntax-info* (line-previous line)))))
      (cond ((and (eq signature (line-signature line))
                  (equal start prev-end))
             ;; no work
             si)
            (t
             ;; work to do, but first remove old font marks
             (dolist (fm font-marks)
               (hi::delete-font-mark fm))
             (setf font-marks nil)
             ;; now do the highlighting
             (let ((state prev-end)
                   (last-font 0))
               ;;(print `(:begin ,state) *trace-output*)
               (loop for p from 0 below (line-length line) do
                     (let ((ch (line-character line p)))
                       (setf state (step** state ch))
                       (let ((font (state-font state)))
                         (unless (eq font last-font)
                           (push (hi::font-mark line p font) font-marks)
                           (setf last-font font)))))
               (setf state (step** state #\newline))
               ;; hack
               (let ((s (line-string line)) p1 p2)
                 (when (and (eql 0 (search "(def" s))
                            (setf p1 (position #\space s))
                            (setf p2 (position #\space s :start (1+ p1))))
                   (push (hi::font-mark line (1+ p1) 5) font-marks)
                   (push (hi::font-mark line p2 0) font-marks)))
               ;;(print (list prev-end state) *trace-output*)
               ;; return new info
               (list (line-signature line)
                     prev-end
                     state
                     font-marks) ))))))

(defun state-font (state)
  (cond ((member 'hash-plus state)
         6)
        (t
         (let ((q (member-if (lambda (x) (member x '(string bq uq comment hash-plus))) state)))
           (case (car q)
             (comment 1)
             (bq 2)
             (uq 3)
             (string 4)
             (hash-plus 6)
             ((nil) 0))))))


;;;

#+NIL
(defparameter mcclim-freetype::*families/faces*
  '(
    #||
    ((:fix :roman) . "/usr/X11R6/lib/X11/fonts/microsoft/lucon.ttf")
    ;;((:fix :roman) . "/usr/X11R6/lib/X11/fonts/microsoft/cour.ttf")
    ((:fix :italic) . "/usr/X11R6/lib/X11/fonts/microsoft/couri.ttf")
    ((:fix :bold-italic) . "/usr/X11R6/lib/X11/fonts/microsoft/courbi.ttf")
    ((:fix :italic-bold) . "/usr/X11R6/lib/X11/fonts/microsoft/courbi.ttf")
    ((:fix :bold) . "/usr/X11R6/lib/X11/fonts/microsoft/courbd.ttf")
    ||#


    ((:fix :roman) . "/usr/local/OpenOffice.org1.1.0/share/fonts/truetype/VeraMono.ttf")
    ((:fix :roman) . "/usr/share/texmf/fonts/type1/bluesky/cm/cmtt8.pfb")
    ((:fix :italic) . "/usr/share/texmf/fonts/type1/bluesky/cm/cmtt12.pfb")
    ((:fix :italic-bold) . "/usr/local/OpenOffice.org1.1.0/share/fonts/truetype/VeraMoBI.ttf")
    ((:fix :bold-italic) . "/usr/local/OpenOffice.org1.1.0/share/fonts/truetype/VeraMoBI.ttf")
    ((:fix :bold) . "/usr/local/OpenOffice.org1.1.0/share/fonts/truetype/VeraMoBd.ttf")

    ((:sans-serif :roman) . "/usr/share/texmf/fonts/type1/bluesky/cm/cmss12.pfb")
    ((:sans-serif :italic) . "/usr/share/texmf/fonts/type1/bluesky/cm/cmssi12.pfb")
    ((:sans-serif :bold-italic) . "/usr/share/texmf/fonts/type1/bluesky/cm/cmssi12.pfb")
    ((:sans-serif :italic-bold) . "/usr/share/texmf/fonts/type1/bluesky/cm/cmssi12.pfb")
    ((:sans-serif :bold) . "/usr/share/texmf/fonts/type1/bluesky/cm/cmssbx10.pfb")

    ((:serif :roman) . "/usr/X11R6/lib/X11/fonts/microsoft/verdana.ttf")
    ((:serif :italic) . "/usr/X11R6/lib/X11/fonts/microsoft/verdanai.ttf")
    ((:serif :bold-italic) . "/usr/X11R6/lib/X11/fonts/microsoft/verdanaz.ttf")
    ((:serif :italic-bold) . "/usr/X11R6/lib/X11/fonts/microsoft/verdanaz.ttf")
    ((:serif :bold) . "/usr/X11R6/lib/X11/fonts/microsoft/verdanab.ttf")))










