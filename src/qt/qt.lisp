;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :qt-hemlock)

(defvar *settings-organization* "Hemlock")
(defvar *settings-application* "Hemlock")

(defun qsettings ()
  (#_new QSettings
         *settings-organization*
         *settings-application*))

(defun save-window-geometry (window)
  (#_setValue (qsettings)
              "geometry"
              (#_new QVariant (#_saveGeometry window))))

(defcommand "Save Window Geometry" (p)
  "Save current window's geometry in Qt settings." ""
  (declare (ignore p))
  (save-window-geometry
   (#_window (qt-hunk-widget (window-hunk (current-window))))))

(defun restore-window-geometry (window)
  (#_restoreGeometry window
                     (#_toByteArray (#_value (qsettings) "geometry"))))

(defcommand "Restore Window Geometry" (p)
  "Restore the current window's geometry from Qt settings." ""
  (declare (ignore p))
  (restore-window-geometry
   (#_window (qt-hunk-widget (window-hunk (current-window))))))

(defcommand "Select Font" (p)
  "Open a font dialog and change the current display font." ""
  (declare (ignore p))
  (setf *font*
        (cffi:with-foreign-object (arg :char)
          (#_QFontDialog::getFont (qt::bool* arg)))))

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

(defclass qt-hunk (device-hunk)
  ((widget :initarg :widget
           :reader qt-hunk-widget)
   (cx :initarg :cx
       :initform nil)
   (cy :initarg :cy
       :initform nil)
   (cw)
   (ch)
   (ts)))

(defclass hunk-widget ()
    ((hunk :accessor widget-hunk)
     (modeline :initarg :modeline
               :accessor widget-modeline))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("paintEvent" paint-event)
             ("resizeEvent" resize-event)
             ("keyPressEvent" key-press-event)
             #+nil ("mousePressEvent" mouse-press-event)
             #+nil ("mouseMoveEvent" mouse-move-event)
             #+nil ("mouseReleaseEvent" mouse-release-event)))

(defmethod initialize-instance :after ((instance hunk-widget) &key)
  (new instance)
  (#_setFocusPolicy instance (#_Qt::StrongFocus)))

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
         (stream (qt-hunk-widget hunk)))
    (#_close stream)
    (setf (slot-value device 'windows)
          (remove window (slot-value device 'windows)))
    (let ((buffer (window-buffer window)))
      (setf (buffer-windows buffer) (delete window (buffer-windows buffer))))))

(defmethod device-make-window ((device qt-device) start modelinep window font-family
                               ask-user x y width-arg height-arg proportion
                               &aux res)
  (let* ((hunk (window-hunk *current-window*))
         (stream (qt-hunk-widget hunk)))
    (let ((new (make-instance 'hunk-widget)))
      (let* ((window (hi::internal-make-window))
             (hunk (make-instance 'qt-hunk :widget new)))
        (setf res window)
        (redraw-widget device window hunk *current-buffer* t)
        (let ((p (position *current-window* (slot-value device 'windows))))
          (setf (slot-value device 'windows)
                (append (subseq (slot-value device 'windows) 0 p)
                        (list window)
                        (subseq (slot-value device 'windows) p)))))
      ;; since we still can't draw on ungrafted windows ...
      (#_show new))
    (finish-output *trace-output*))
  res)

(defmethod resize-event ((instance hunk-widget) resize-event)
  (call-next-qmethod)
  (note-sheet-region-changed instance))

(defvar *standard-column-width* 80)

(defun standard-width-in-pixels ()
  (* *standard-column-width* (#_width (#_new QFontMetrics *font*) "m")))

(defun offset-on-each-side (widget)
  (let ((white-width (standard-width-in-pixels))
        (full-width (#_width widget)))
    (max 0.0d0 (/ (- full-width white-width) 2.0d0))))

(defun invoke-with-hunk-painter (fun widget)
  (let ((painter (#_new QPainter widget)))
    (#_translate painter (offset-on-each-side widget) 0.0d0)
    (funcall fun painter)
    (#_end painter)))

(defmethod paint-event ((instance hunk-widget) paint-event)
  (let* ((painter (#_new QPainter instance)))
    (#_setPen painter (#_Qt::NoPen))
    (#_fillRect painter
                (#_new QRectF (#_rect instance))
                (#_new QBrush (#_new QColor 220 255 255)))
    (#_end painter))
  (let ((left (#_new QRectF (#_rect instance))))
    (invoke-with-hunk-painter
     (lambda (painter)
       (#_setWidth left (standard-width-in-pixels))
       (#_fillRect painter left (#_new QBrush (#_new QColor 255 255 255))))
     instance))
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

;;; Input

(defclass qt-editor-input (editor-input)
  ())

(defvar *alt-is-meta* t)

(defvar *qapp*)

(defmethod key-press-event ((instance hunk-widget) event)
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
       (hemlock-ext:key-event-keysym #k"tab"))
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
     (#_processEvents *qapp* (#_QEventLoop::WaitForMoreEvents))))

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
    (qt-window-changed (slot-value hunk-pane 'hunk))
    (hi::internal-redisplay)))

(defvar *font*)
(defvar *tabs*)
(defvar *buffers-to-tabs*)

(defun make-hemlock-widget ()
  (let* ((wrapper (#_new QSplitter))
         (vbox (#_new QVBoxLayout))
         (tabs (#_new QTabBar))
         (main (make-instance 'hunk-widget))
         (echo (make-instance 'hunk-widget))
         (font
          (let ((font (#_new QFont)))
            (#_fromString font *font-family*)
            (#_setPointSize font *font-size*)
            font))
         (*font* font)
         (metrics (#_new QFontMetrics font)))
    (#_addWidget vbox tabs)
    (#_addWidget vbox main)
    (let ((x (#_new QWidget)))
      (#_setLayout x vbox)
      (#_addWidget wrapper x))
    (#_addWidget wrapper echo)
    (#_setOrientation wrapper (#_Qt::Vertical))
    (#_setFocusPolicy tabs (#_Qt::NoFocus))
    (#_setSpacing vbox 0)
    (#_setMargin vbox 0)
    ;; fixme: should be a default, not a strict minimum:
    (#_setMinimumSize wrapper
                      (standard-width-in-pixels)
                      (* 25 (#_height metrics)))
    (#_setMaximumWidth tabs (#_width wrapper))
    (#_setMaximumHeight echo 100)
    (values main echo font wrapper tabs)))

(defun add-buffer-tab-hook (buffer)
  (#_addTab *tabs* (buffer-name buffer)))

(defun buffer-tab-index (buffer)
  (dotimes (i (#_count *tabs*) (error "buffer tab missing"))
    (when (equal (#_tabText *tabs* i) (buffer-name buffer))
      (return i))))

(defun delete-buffer-tab-hook (buffer)
  (#_removeTab *tabs* (buffer-tab-index buffer)))

(defun update-buffer-tab-hook (buffer new-name)
  (#_setTabText *tabs*
                (buffer-tab-index buffer)
                new-name))

(defun set-buffer-tab-hook (buffer)
  (#_setCurrentIndex *tabs* (buffer-tab-index buffer)))

(add-hook hemlock::make-buffer-hook 'add-buffer-tab-hook)
(add-hook hemlock::delete-buffer-hook 'delete-buffer-tab-hook)
(add-hook hemlock::buffer-name-hook 'update-buffer-tab-hook)
(add-hook hemlock::set-buffer-hook 'set-buffer-tab-hook)

(defun signal-receiver (function)
  (make-instance 'signal-receiver :function function))

(defun connect (source signal cont)
  (let ((receiver (signal-receiver cont)))
    (push receiver *do-not-gc-list*)
    (#_QObject::connect source signal receiver (QSLOT "invoke(int)"))))

(defclass signal-receiver ()
  ((function :initarg :function
             :accessor signal-receiver-function))
  (:metaclass qt-class)
  (:qt-superclass "QObject")
  (:slots ("invoke()" (lambda (this &rest args)
                        (apply (signal-receiver-function this)
                               args)))
          ("invoke(int)" (lambda (this &rest args)
                           (apply (signal-receiver-function this)
                                  args)))))

(defmethod initialize-instance :after ((instance signal-receiver) &key)
  (new instance))

(defclass command-action-receiver ()
    ((command :initarg :command
              :accessor command-action-receiver-command))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("triggered()" (lambda (this)
                           (funcall (command-function
                                     (command-action-receiver-command
                                      this))
                                    nil)
                           (hi::internal-redisplay)))))

(defmethod initialize-instance :after ((instance command-action-receiver) &key)
  (new instance))

(defvar *do-not-gc-list*)

(defun add-command-action (menu command &optional suffix)
  (let* ((receiver
          (make-instance 'command-action-receiver
                         :command (getstring command hi::*command-names*)))
         (action
          (#_addAction
           menu
           (concatenate 'string command suffix)
           receiver
           (qslot "triggered()"))))
    (push action *do-not-gc-list*)
    (push receiver *do-not-gc-list*)))

(defun find-buffer (name)
  (getstring name hi::*buffer-names*))

(defun qt-hemlock (init-fun command-loop-fun)
  (setf *qapp* (make-qapplication))
  (multiple-value-bind (main echo *font* widget *tabs*)
      (make-hemlock-widget)
    (let* ((window (#_new QMainWindow))
           (*window-list* *window-list*)
           (*editor-input*
            (let ((e (hi::make-input-event)))
              (make-instance 'qt-editor-input :head e :tail e)))
           (*do-not-gc-list* '())
           (*buffers-in-tab-order* '()))
      (#_setWindowTitle window "Hemlock")
      (#_setCentralWidget window widget)
      (let ((menu (#_addMenu (#_menuBar window) "File")))
        (add-command-action menu "Find File")
        (add-command-action menu "Save File")
        (#_addSeparator menu)
        (add-command-action menu "Write File")
        (#_addSeparator menu)
        (add-command-action menu "Exit Hemlock"))
      (let ((menu (#_addMenu (#_menuBar window) "Eval")))
        (add-command-action menu "Select Eval Buffer"))
      (let ((menu (#_addMenu (#_menuBar window) "Buffer")))
        (add-command-action menu "Bufed")
        (add-command-action menu "Select Buffer"))
      (let ((menu (#_addMenu (#_menuBar window) "Preferences")))
        (add-command-action menu "Select Font")
        (#_addSeparator menu)
        (add-command-action menu "Save Window Geometry")
        (add-command-action menu "Restore Window Geometry"))
      (setf hi::*real-editor-input* *editor-input*)
      (redraw-all-widgets main echo nil)
      (when init-fun
        (funcall init-fun))
      (setf (widget-modeline main) (#_statusBar window))
      (dolist (buffer hi::*buffer-list*)
        (unless (eq buffer *echo-area-buffer*)
          (add-buffer-tab-hook buffer)))
      (connect *tabs*
               (qsignal "currentChanged(int)")
               (lambda (index)
                 (change-to-buffer (find-buffer (#_tabText *tabs* index)))
                 (hi::internal-redisplay)))
      (restore-window-geometry window)
      (#_show window)
      ;; undo the minimum set before, so that it's only a default
      ;; (fixme: it still overrides a saved geometry):
      (#_setMinimumSize widget 0 0)
      (unwind-protect
           (progn                       ;catch 'hi::hemlock-exit
             (funcall command-loop-fun))
        (#_hide window)))))

;;; Keysym translations

(defun qt-character-keysym (gesture)
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

(defun effective-hunk-widget-width (widget)
  (- (#_width widget) (offset-on-each-side widget)))

(defun qt-window-changed (hunk)
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
            (max 5 (floor (- (effective-hunk-widget-width (qt-hunk-widget hunk))
                             (* 2 *gutter*))
                          (slot-value hunk 'cw))))
           (new-height
            (max 2 (1- (floor (- (#_height (qt-hunk-widget hunk))
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

(defun redraw-all-widgets (main-widget echo-widget another-widget)
  (let* ((device (make-instance 'qt-device))
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
           (hunk (make-instance 'qt-hunk :widget main-widget)))
      (redraw-widget device window hunk buffer t)
      (setf *current-window* window)
      (push window (slot-value device 'windows))
      (setf (device-hunks device) (list hunk)) )
    (when another-widget
      (let* ((window (hi::internal-make-window))
             (hunk (make-instance 'qt-hunk :widget another-widget)))
        (redraw-widget device window hunk buffer t)
        (push window (slot-value device 'windows))
        (push hunk (device-hunks device))))
    ;;
    (when echo-widget                   ;hmm
      (let ((echo-window (hi::internal-make-window))
            (echo-hunk (make-instance 'qt-hunk :widget echo-widget)))
        (redraw-widget device echo-window echo-hunk *echo-area-buffer* nil)
        (setf *echo-area-window* echo-window)
        ;; why isn't this on the list of hunks?
        ;; List of hunks isn't used at all.
        ))))

(defvar *font-family*
  "Fixed [Sony]"
  #+nil "Courier New")

(defvar *font-size*
  11)

(defun redraw-widget (device window hunk buffer modelinep)
  (setf (slot-value (qt-hunk-widget hunk) 'hunk)
        hunk)
  (let* ((start (buffer-start-mark buffer))
         (first (cons dummy-line the-sentinel))
         (font *font*)
         (metrics (#_new QFontMetrics font))
         width height)
    (setf
     (slot-value hunk 'cw) (+ 0 (#_width metrics "m"))
     (slot-value hunk 'ch) (+ 2 (#_height metrics))
     width (max 5 (floor (- (#_width (qt-hunk-widget hunk))
                                     (* 2 *gutter*))
                                  (slot-value hunk 'cw)))
     height (max 2 (floor (- (#_height (qt-hunk-widget hunk))
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

    (setup-dis-lines window width height)

    (when modelinep
      (setup-modeline-image buffer window))

    (push window (buffer-windows buffer))
    (push window *window-list*)
    (hi::update-window-image window)))

(defun setup-dis-lines (window width height)
  (do ((i (- height) (1+ i))
       (res ()
            (cons (make-window-dis-line (make-string width)) res)))
      ((= i height)
       (setf (window-spare-lines window) res))))

;;;; Redisplay

(defvar *tick* 0)

(defmethod dumb-repaint ((device qt-device) window)
  (qt-drop-cursor (window-hunk window))
  (let* ((widget (qt-hunk-widget (window-hunk window)))
         (w (#_width widget))
         (h (#_height widget))
         (hunk (window-hunk window))
         (first (window-first-line window)))
    (do ((i 0 (1+ i))
         (dl (cdr first) (cdr dl)))
        ((eq dl the-sentinel)
         (setf (window-old-lines window) (1- i)))
      (qt-dumb-line-redisplay hunk (car dl)))
    (setf (window-first-changed window) the-sentinel
          (window-last-changed window) first)
    (when (window-modeline-buffer window)
      (update-modeline-fields (window-buffer window) window)
      (#_showMessage (widget-modeline widget)
                     (subseq (window-modeline-buffer window)
                             0
                             (window-modeline-buffer-len window)))
      (#_update (widget-modeline widget))
      #+(or)
      (qt-dumb-line-redisplay hunk
                              (window-modeline-dis-line window)
                              t)
      (setf (dis-line-flags (window-modeline-dis-line window))
            unaltered-bits))
    (qt-put-cursor (window-hunk window))))

(defmethod device-dumb-redisplay ((device qt-device) window)
  (#_update (qt-hunk-widget (window-hunk window))))

(defun qt-dumb-line-redisplay (hunk dl &optional modelinep)
  (let* ((h (slot-value hunk 'ch))
         (w (slot-value hunk 'cw))
         (xo *gutter*)
         (yo *gutter*))
    (unless (zerop (dis-line-flags dl))
      (setf (hi::dis-line-tick dl) (incf *tick*)))
    (let ((chrs (dis-line-chars dl)))
      (let ((y (+ yo (* (dis-line-position dl) h))))
        (when modelinep
          (setf y (- (#_height (qt-hunk-widget hunk)) h 2)))
        ;; font changes
        (let ((font 0)                  ;###
              (start 0)
              (end (dis-line-length dl))
              (changes (dis-line-font-changes dl)))
          (loop
             (cond ((null changes)
                    (qt-draw-text hunk chrs
                                  (+ xo (* w start))
                                  (+ 1 y)
                                  start end font)
                    (return))
                   (t
                    (qt-draw-text hunk chrs
                                  (+ xo (* w start))
                                  (+ 1 y)
                                  start (font-change-x changes) font)
                    (setf font (font-change-font changes)
                          start (font-change-x changes)
                          changes (font-change-next changes)))))) )))
  (setf (dis-line-flags dl) unaltered-bits (dis-line-delta dl) 0))

(defun qt-draw-text (hunk string x y start end font)
  (declare (ignore font))
  (let ((instance (qt-hunk-widget hunk)))
    (invoke-with-hunk-painter
     (lambda (painter)
       (#_setPen painter (#_black "Qt"))
       (#_setFont painter *font*)
       (incf y (#_ascent (#_fontMetrics painter)))
       (#_setRenderHint painter (#_QPainter::Antialiasing) t)
       (#_drawText painter x y (subseq string start end)))
     instance)))

(defun qt-drop-cursor (hunk)
  hunk
  nil)

(defun qt-put-cursor (hunk)
  (with-slots (cx cy cw ch) hunk
    (when (and cx cy)
      (let* ((instance (qt-hunk-widget hunk)))
        (invoke-with-hunk-painter
         (lambda (painter)
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
                       (+ *gutter* (* cy ch) ch)))
         instance)))))

(defun hi::editor-sleep (time)
  "Sleep for approximately Time seconds."
  (unless (zerop time)
    (hi::internal-redisplay)
    (hi::sleep-for-time time)
    nil))

(defun hi::sleep-for-time (time)
  #+(or)
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
    (device-note-read-wait device nil))
  (format t "ignoring hi::sleep-for-time ~A~%" time))

#+(or)
(defun hi::invoke-with-pop-up-display (cont buffer-name height)
  (funcall cont *trace-output*)
  (finish-output *trace-output*))
