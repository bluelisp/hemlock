;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :qt-hemlock)

(named-readtables:defreadtable :qt-hemlock
    (:merge :qt)
  (:dispatch-macro-char #\# #\k 'hemlock-ext::parse-key-fun))

(named-readtables:in-readtable :qt-hemlock)

(defparameter *gutter* 10
  "The gutter to place between between the matter in a hemlock pane and its
   margin to improve legibility (sp?, damn i miss ispell).")

(defclass qt-device (device)
  ((cursor-hunk
    :initform nil :documentation "The hunk that has the cursor.")
   (windows :initform nil)))

(defclass qt-hunk-pane ()
  ((hunk))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("paintEvent" paint-event)
             ("resizeEvent" resize-event)
             ("keyPressEvent" key-press-event)
             #+nil ("mousePressEvent" mouse-press-event)
             #+nil ("mouseMoveEvent" mouse-move-event)
             #+nil ("mouseReleaseEvent" mouse-release-event)))

(defmethod initialize-instance :after ((instance qt-hunk-pane) &key)
  (new instance)
  (#_setFocusPolicy instance (#_Qt::StrongFocus)))

(defmethod device-init ((device qt-device))
  )

(defmethod device-exit ((device qt-device)))

(defmethod device-smart-redisplay ((device qt-device) window)
  ;; We aren't smart by any margin.
  (device-dumb-redisplay device window))

(defmethod device-after-redisplay ((device qt-device))
  )

(defmethod device-clear ((device qt-device))
  )

(defmethod device-note-read-wait ((device qt-device) on-off)
  )

(defmethod device-force-output ((device qt-device))
  )

(defmethod device-finish-output ((device qt-device) window)
  )

(defmethod device-put-cursor ((device qt-device) hunk x y)
  (with-slots (cursor-hunk) device
    (when cursor-hunk
      (qt-drop-cursor cursor-hunk)
      (with-slots (cx cy) cursor-hunk
        (setf cx nil cy nil)))
    (when hunk
      (with-slots (cx cy) hunk
        (setf cx x cy y))
      (qt-put-cursor hunk))
    (setf cursor-hunk hunk)))

(defmethod device-show-mark ((device qt-device) window x y time)
  )

;;;; Windows

;; CLIM Hemlock comment:
;;
;; each window is a single pane, which should keep
;; things simple. We do not yet have the notion of window groups.

(defmethod device-next-window ((device qt-device) window)
  (with-slots (windows) device
    (elt windows (mod (1+ (position window windows))
                      (length windows)))))

(defmethod device-previous-window ((device qt-device) window)
  (with-slots (windows) device
    (elt windows (mod (1- (position window windows))
                      (length windows)))))

(defmethod device-delete-window ((device qt-device) window)
  (let* ((hunk (window-hunk window))
         (stream (qt-hunk-stream hunk)))
    (#_close stream)
    (setf (slot-value device 'windows)
          (remove window (slot-value device 'windows)))
    (let ((buffer (window-buffer window)))
      (setf (buffer-windows buffer) (delete window (buffer-windows buffer))))))

(defmethod device-make-window ((device qt-device) start modelinep window font-family
                               ask-user x y width-arg height-arg proportion
                               &aux res)
  (let* ((hunk (window-hunk *current-window*))
         (stream (qt-hunk-stream hunk)))
    (let ((new (make-instance 'qt-hunk-pane)))
      (let* ((window (hi::internal-make-window))
             (hunk (make-instance 'qt-hunk :stream new)))
        (setf res window)
        (baba-aux device window hunk *current-buffer*)
        (let ((p (position *current-window* (slot-value device 'windows))))
          (setf (slot-value device 'windows)
                (append (subseq (slot-value device 'windows) 0 p)
                        (list window)
                        (subseq (slot-value device 'windows) p)))))
      ;; since we still can't draw on ungrafted windows ...
      (#_show new))
    (finish-output *trace-output*))
  res)

(defmethod resize-event ((instance qt-hunk-pane) resize-event)
  (call-next-qmethod)
  (note-sheet-region-changed instance))

(defmethod paint-event ((instance qt-hunk-pane) paint-event)
  (let* ((painter (#_new QPainter instance)))
    (#_setPen painter (#_Qt::NoPen))
    (#_setBrush painter (#_new QBrush (#_new QColor 0 255 255 64)))
    (#_fillRect painter
                (#_new QRectF (#_rect instance))
                (#_new QBrush (#_new QColor 255 255 255)))
    (#_end painter))
  (let* ((hunk (slot-value instance 'hunk))
         (device (device-hunk-device hunk)))
    #+(or)
    (with-slots (cursor-hunk) device
      (when cursor-hunk
        (qt-drop-cursor cursor-hunk)))
    (dumb-repaint device (device-hunk-window hunk))
    ;; draw contents here
    (with-slots (cursor-hunk) device
      (when cursor-hunk
        (qt-put-cursor cursor-hunk)))))


;;;;

(defmethod device-random-typeout-full-more ((device qt-device) stream)
  )

(defmethod device-random-typeout-line-more ((device qt-device) stream n)
  )

(defmethod device-random-typeout-setup ((device qt-device) stream n)
  )

(defmethod device-random-typeout-cleanup ((device qt-device) stream degree)
  )

(defmethod device-beep ((device qt-device) stream)
  )

;;; Hunks

(defclass qt-hunk (device-hunk)
  ((stream :initarg :stream
           :reader qt-hunk-stream
           :documentation "Extended output stream this hunk is displayed on.")
   (cx :initarg :cx :initform nil)
   (cy :initarg :cy :initform nil)
   (cw)
   (ch)
   (ts)))

;;; Input

(defclass qt-editor-input (editor-input)
  () )

(defvar *alt-is-meta* t)

(defvar *qapp*)

(defmethod key-press-event ((instance qt-hunk-pane) event)
  (call-next-qmethod)
  (hi::q-event *editor-input* (qevent-to-key-event event)))

(defun parse-modifiers (event)
  (let ((mods (qt::primitive-value (#_modifiers event))))
    (logior (if (logtest mods
                         (qt::primitive-value (#_Qt::ControlModifier)))
                (hemlock-ext:key-event-bits #k"control-a")
                0)
            (if (or (logtest mods
                             (qt::primitive-value (#_Qt::MetaModifier)))
                    (and *alt-is-meta*
                         (logtest (qt::primitive-value (#_modifiers event))
                                  (qt::primitive-value (#_Qt::AltModifier)))))
                (hemlock-ext:key-event-bits #k"meta-a")
                0))))

(defun parse-key (event)
  (let ((k (#_key event)))
    (cond
      ((or (eql k (primitive-value (#_Qt::Key_Return)))
           (eql k (primitive-value (#_Qt::Key_Enter))))
       (hemlock-ext:key-event-keysym #k"Return"))
      ((eql k (primitive-value (#_Qt::Key_Tab)))
       (hemlock-ext:key-event-keysym #k"Tab"))
      ((eql k (primitive-value (#_Qt::Key_Escape)))
       (hemlock-ext:key-event-keysym #k"Escape"))
      ((eql k (primitive-value (#_Qt::Key_Backspace)))
       (hemlock-ext:key-event-keysym #k"Backspace"))
      ((eql k (primitive-value (#_Qt::Key_Delete)))
       (hemlock-ext:key-event-keysym #k"delete"))
      ((eql k (primitive-value (#_Qt::Key_Space)))
       (hemlock-ext:key-event-keysym #k"space"))
      (t
       nil))))

(defun qevent-to-key-event (event)
  (let* ((text (map 'string
                    (lambda (c)
                      (if (< (char-code c) 32)
                          (code-char (+ 96 (char-code c)))
                          c))
                    (#_text event)))
         (mask (parse-modifiers event))
         (keysym (or (parse-key event)
                     (hemlock-ext::name-keysym text))))
    (when keysym
      (hemlock-ext:make-key-event keysym mask))))

(defmethod get-key-event ((stream qt-editor-input) &optional ignore-abort-attempts-p)
  (declare (ignorable ignore-abort-attempts-p))
  (hi::internal-redisplay)
  (loop
     (let ((event (hi::dq-event stream)))
       (when event
         (return event)))
     #+nil (hi::internal-redisplay)
     #+nil (print :execute)
     (force-output)
     (#_processEvents *qapp*)))

(defmethod unget-key-event (key-event (stream qt-editor-input))
  (hi::un-event key-event stream))

(defmethod clear-editor-input ((stream qt-editor-input))
  ;; hmm?
  )

(defmethod listen-editor-input ((stream qt-editor-input))
  (hi::input-event-next (hi::editor-input-head stream)))

;;;; There is awful lot to do to boot a device.

(defun note-sheet-region-changed (hunk-pane)
  (when (slot-boundp hunk-pane 'hunk)
    (clim-window-changed (slot-value hunk-pane 'hunk))
    (hi::internal-redisplay)))

(defun qt-hemlock ()
  (setf *qapp* (make-qapplication))
  (let ((window (#_new QWidget))
        (layout (#_new QVBoxLayout))
        (main (make-instance 'qt-hunk-pane))
        (echo (make-instance 'qt-hunk-pane))
        (*window-list* *window-list*)
        (*editor-input*
         (let ((e (hi::make-input-event)))
           (make-instance 'qt-editor-input :head e :tail e))))
    (#_setWindowTitle window "Hemlock")
    (#_addWidget layout main)
    (#_addWidget layout echo)
    (#_setLayout window layout)
    (setf hi::*real-editor-input* *editor-input*)
    (#_setGeometry window 100 100 500 355)
    (#_setMaximumHeight echo 100)
    (baba main echo nil)
    (insert-string (current-point) (format nil "line1~%line2~%"))
    (#_show window)
    (unwind-protect
         (catch 'hi::hemlock-exit
           (hi::%command-loop))
      (#_hide window))))

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
           (new-width
            (max 5 (floor (- (#_width (qt-hunk-stream hunk))
                             (* 2 *gutter*))
                          (slot-value hunk 'cw))))
           (new-height
            (max 2 (1- (floor (- (#_height (qt-hunk-stream hunk))
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
         (device (make-instance 'qt-device))
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
           (hunk (make-instance 'qt-hunk :stream stream)))
      (baba-aux device window hunk buffer)
      (setf *current-window* window)
      (push window (slot-value device 'windows))
      (setf (device-hunks device) (list hunk)) )
    (when another-stream
      (let* ((window (hi::internal-make-window))
             (hunk (make-instance 'qt-hunk :stream another-stream)))
        (baba-aux device window hunk buffer)
        (push window (slot-value device 'windows))
        (push hunk (device-hunks device))))
    ;;
    (when echo-stream                   ;hmm
      (let ((echo-window (hi::internal-make-window))
            (echo-hunk (make-instance 'qt-hunk :stream echo-stream)))
        (baba-aux device echo-window echo-hunk *echo-area-buffer*)
        (setf *echo-area-window* echo-window)
        ;; why isn't this on the list of hunks?
        ;; List of hunks isn't used at all.
        ))))

(defun baba-aux (device window hunk buffer)
  (setf (slot-value (qt-hunk-stream hunk) 'hunk)
        hunk)
  (let* ((start (buffer-start-mark buffer))
         (first (cons dummy-line the-sentinel))
         (font (#_new QFont "Courier" 10))
         (metrics (#_new QFontMetrics font))
         width height)
    (setf
     ;; (slot-value hunk 'ts) (clim:make-text-style :fix :roman 11.5)
     (slot-value hunk 'cw) (+ 0 (#_width metrics "m"))
     (slot-value hunk 'ch) (+ 2 (#_height metrics))
     width (max 5 (floor (- (#_width (qt-hunk-stream hunk))
                                     (* 2 *gutter*))
                                  (slot-value hunk 'cw)))
     height (max 2 (floor (- (#_height (qt-hunk-stream hunk))
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

    #+(or)
    (loop for i from 32 below 126 do
          (let ((s (string (code-char i))))
            (let ((w (clim:text-size (qt-hunk-stream hunk) s
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

(defmethod dumb-repaint ((device qt-device) window)
  (qt-drop-cursor (window-hunk window))
  (let* ((widget (qt-hunk-stream (window-hunk window)))
         (w (#_width widget))
         (h (#_height widget)))
    #+(or)
    (clim:updating-output (t :unique-id :static :cache-value h)
                          (clim:draw-rectangle* *standard-output*
                                                1 1
                                                (- w 2) (- h 2)
                                                :ink clim:+black+
                                                :filled nil) ))
  (progn ;clim:with-text-style (*standard-output* (slot-value (window-hunk window) 'ts))
    (progn                     ;clim:updating-output (*standard-output*)
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
        #+NIL                           ;###
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
  #+nil (clim:redisplay-frame-pane clim:*application-frame* *standard-output*)
  (qt-put-cursor (window-hunk window))
  ;;(force-output *standard-output*)
  #+nil (clim:medium-finish-output (clim:sheet-medium *standard-output*)))

(defmethod device-dumb-redisplay ((device qt-device) window)
  (#_update (qt-hunk-stream (window-hunk window))))

(defun clim-dumb-line-redisplay (hunk dl &optional modelinep)
  (let* ((h (slot-value hunk 'ch))
         (w (slot-value hunk 'cw))
         (xo *gutter*)
         (yo *gutter*))
    (unless (zerop (dis-line-flags dl))
      (setf (hi::dis-line-tick dl) (incf *tick*)))
    (let ((chrs (dis-line-chars dl)))
      (progn ;clim:updating-output
        #+nil (*standard-output*        ;###
               :unique-id (if modelinep :modeline (dis-line-position dl))
               :id-test #'eq            ;###
               :cache-value (hi::dis-line-tick dl)
               :cache-test #'eql)
        (let ((y (+ yo (* (dis-line-position dl) h))))
          (when modelinep
            (setf y (- (#_height (qt-hunk-stream hunk)) h 2)))
          #+nil (clim:draw-rectangle* *standard-output*
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
                       (clim-draw-text hunk chrs
                                       (+ xo (* w start))
                                       (+ 1 y)
                                       start end font)
                       (return))
                      (t
                       (clim-draw-text hunk chrs
                                       (+ xo (* w start))
                                       (+ 1 y)
                                       start (font-change-x changes) font)
                       (setf font (font-change-font changes)
                             start (font-change-x changes)
                             changes (font-change-next changes)))))) ))))
  (setf (dis-line-flags dl) unaltered-bits (dis-line-delta dl) 0))

(defun clim-draw-text (hunk string x y start end font)
  #+(or)
  (let ((ch (clim:text-style-height (clim:medium-text-style stream)
                                    stream))
        (dx (clim:stream-string-width stream string :start start :end end)))
    (clim:draw-rectangle* stream
                          x (1- y)
                          (+ x dx) (+ y ch 1) :ink (hemlock-font-background font)))
  #+(or)
  (clim:draw-text* stream string x (+ y (clim:text-style-ascent (clim:medium-text-style stream)
                                                                stream))
                   :start start :end end
                   ;; :align-y :top ### :align-y is borken.
                   :ink (hemlock-font-foreground font))
  (let* ((instance (qt-hunk-stream hunk))
         (painter (#_new QPainter instance)))
    (#_setPen painter (#_black "Qt"))
    (#_setFont painter (#_new QFont "Courier" 10))
    (incf y (#_ascent (#_fontMetrics painter)))
    (#_drawText painter x y (subseq string start end))
    (#_end painter))
  #+(or)
  (when (= font 5)
    (let ((ch (clim:text-style-height (clim:medium-text-style stream)
                                      stream))
          (dx (clim:stream-string-width stream string :start start :end end)))
      (clim:draw-line* stream x (+ y ch -1) (+ x dx) (+ y ch -1)))) )

(defun qt-drop-cursor (hunk)
  hunk
  nil)

(defun qt-put-cursor (hunk)
  (with-slots (cx cy cw ch) hunk
    (when (and cx cy)
      (let* ((instance (qt-hunk-stream hunk))
             (painter (#_new QPainter instance)))
        (#_setPen painter (#_Qt::NoPen) #+nil (#_new QColor 0 0 255 16))
        (#_setBrush painter (#_new QBrush (#_new QColor 0 255 255 64)))
        (#_drawRect painter
                    (+ *gutter* (* cx cw))
                    (+ *gutter* (* cy ch))
                    cw ch)
        #+(or)
        (#_setPen painter (#_new QColor 0 0 255))
        #+(or)
        (#_drawLine painter
                    (+ *gutter* (* cx cw))
                    (+ *gutter* (* cy ch))
                    (+ *gutter* (* cx cw) 0)
                    (+ *gutter* (* cy ch) ch))
        (#_end painter)))))

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

#+(or)
(defun hemlock-font-foreground (font)
  (case font
    (1 clim:+blue4+)
    (3 clim:+black+)
    (2 clim:+cyan4+)
    (4 clim:+green4+)
    (5 clim:+red4+)
    (6 clim:+gray50+)
    (otherwise clim:+black+)))

#+(or)
(defun hemlock-font-background (font)
  (case font
    (3 (clim:make-rgb-color 1 .9 .8))
    (otherwise clim:+white+)))

(defun hi::invoke-with-pop-up-display (cont buffer-name height)
  (funcall cont *trace-output*)
  (finish-output *trace-output*))
