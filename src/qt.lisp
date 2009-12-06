;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock.qt)

(pushnew :qt hi::*available-backends*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (named-readtables:defreadtable :hemlock.qt
      (:merge :qt)
    (:dispatch-macro-char #\# #\k 'hemlock-ext::parse-key-fun)))

(named-readtables:in-readtable :hemlock.qt)

(defun enable-syntax ()
  (named-readtables:in-readtable :hemlock.qt)
  nil)

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
  (cffi:with-foreign-object (arg :char)
    (let ((font (#_QFontDialog::getFont (qt::bool* arg) *font*)))
      (when (zerop (cffi:mem-ref arg :char))
        (editor-error "Font dialog cancelled"))
      (setf *font* font))))

(defparameter *gutter* 10
  "The gutter to place between between the matter in a hemlock pane and its
   margin to improve legibility (sp?, damn i miss ispell).")

(defclass qt-device (device)
  ((cursor-hunk :initform nil
                :documentation "The hunk that has the cursor."
                :accessor device-cursor-hunk)
   (cursor-item :initform nil
                :accessor device-cursor-item)
   (windows :initform nil)
   (main-window :initform nil
                :accessor device-main-window)))

(defun current-device ()
  (device-hunk-device (window-hunk (current-window))))

(defclass qt-hunk (device-hunk)
  ((widget :initarg :widget
           :reader qt-hunk-widget)
   (item :initarg :item
         :accessor qt-hunk-item)
   (want-background-p :initarg :want-background-p
                      :initform nil
                      :accessor qt-hunk-want-background-p)
   (itab :initform (make-array 0 :adjustable t :initial-element nil))
   (cw)
   (ch)
   (ts)))

(defun line-items (hunk i)
  (with-slots (itab) hunk
    (unless (< i (length itab))
      (adjust-array itab (* 2 (max 1 i)) :initial-element nil))
    (elt itab i)))

(defun (setf line-items) (newval hunk i)
  (with-slots (itab) hunk
    (unless (< i (length itab))
      (adjust-array itab (* 2 (max 1 i))))
    (setf (elt itab i) newval)))

(defvar *steal-focus-out* t)

(defclass hunk-widget ()
    ((hunk :accessor widget-hunk)
     (modeline :initarg :modeline
               :accessor widget-modeline)
     (centerize :initform nil
                :initarg :centerize
                :accessor centerize-widget-p)
     (paint-margin :initform nil
                   :initarg :paint-margin
                   :accessor paint-widget-margin-p)
     (background-pixmap :initform nil
                        :accessor hunk-widget-background-pixmap)
     (background-pixmap-item :initform nil
                             :accessor hunk-widget-background-pixmap-item)
     (white-item-1 :initform nil
                   :accessor hunk-widget-rect-pixmap-item)
     (white-item-2 :initform nil
                   :accessor hunk-widget-rect-pixmap-item))
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsView")
  (:override ("resizeEvent" resize-event)
             ("keyPressEvent" key-press-event)
             ("focusOutEvent" focus-out-event)
             ("event" intercept-event)
             #+nil ("mousePressEvent" mouse-press-event)
             #+nil ("mouseMoveEvent" mouse-move-event)
             #+nil ("mouseReleaseEvent" mouse-release-event)))

(defun focus-out-event (this event)
  (declare (ignore event))
  (when *steal-focus-out*
    (let ((*steal-focus-out* nil))
      (#_setFocus this))))

(defvar *interesting-event-received* nil
  "Did Qt just receive an event that matters to Hemlock?

  Anyone using :OVERRIDE or #_connect for code that might affect
  Hemlock state is required to set this variable to true.  See the
  comment in DISPATCH-EVENTS for details.")

(defun intercept-event (instance event)
  ;;
  ;; Rather than have this separately in each hunk-widget method, do
  ;; it centrally here:
  (setf *interesting-event-received* t)
  ;;
  ;; Qt consumes Tab key events for its own purposes.  Using this event
  ;; interceptor, we can steal them in time.
  (let (casted)
    (cond
     ((and (enum= (#_type event) (#_QEvent::KeyPress))
           (setf casted (make-instance 'qobject
                                  :class (qt:find-qclass "QKeyEvent")
                                  :pointer (qt::qobject-pointer event)))
           (eql (#_key casted) (primitive-value (#_Qt::Key_Tab))))
      (key-press-event instance casted)
      t)
     (t
      (call-next-qmethod)))))

(defmethod initialize-instance :after ((instance hunk-widget) &key)
  (new instance)
  (#_setFocusPolicy instance (#_Qt::StrongFocus))
  (#_setScene instance (#_new QGraphicsScene instance)))

(defmethod device-init ((device qt-device))
  ;; (redisplay-all)
  )

(defmethod device-exit ((device qt-device)))

(defmethod device-smart-redisplay ((device qt-device) window)
  (dumb-or-smart-redisplay device window nil))

(defmethod device-dumb-redisplay ((device qt-device) window)
  (dumb-or-smart-redisplay device window t))

(defmethod device-after-redisplay ((device qt-device))
  )

(defmethod device-clear ((device qt-device))
  )

(defmethod device-note-read-wait ((device qt-device) on-off)
  )

(defun exhaustively-dispatch-events-no-hang ()
  ;; Must dispatch all remaining events here (but not actually block).
  ;;
  ;; Redisplay can lead to events being posted, and Hemlock's event loop
  ;; is built to alternate between INTERNAL-REDISPLAY and
  ;; DISPATCH-EVENTS, with the expectation that only meaningful events
  ;; like keyboard or socket interaction will make event dispatching
  ;; return.  So if redisplay exited with a pending event,
  ;; editor-input-method would degenerate into a busy loop.
  (let ((ev (#_QAbstractEventDispatcher::instance)))
    (iter (while (#_processEvents ev (#_QEventLoop::AllEvents))))))

(defmethod device-force-output ((device qt-device))
  (exhaustively-dispatch-events-no-hang))

(defmethod device-finish-output ((device qt-device) window)
  )

(defmethod device-put-cursor ((device qt-device) hunk x y)
  (with-slots (cursor-item cursor-hunk) device
    (when (and cursor-item (not (eq hunk cursor-hunk)))
      (let ((*steal-focus-out* nil))
        (#_removeItem (#_scene cursor-item) cursor-item))
      (setf cursor-item nil))
    (setf cursor-hunk hunk)
    (with-slots (cw ch) hunk
      (unless cursor-item
        (setf cursor-item
              (#_addPath (#_scene (qt-hunk-widget hunk))
                         (let ((path (#_new QPainterPath)))
                           (#_addRect path 0 0 cw ch)
                           path)
                         (#_new QPen (#_Qt::NoPen))
                         (#_new QBrush (#_new QColor 0 180 180 64)))))
      (#_setPos cursor-item
                (+ *gutter*
                   (truncate (offset-on-each-side (qt-hunk-widget hunk)))
                   (* x cw))
                (+ *gutter* (* y ch))))
    #+nil (#_setZValue cursor-item 1)))

(defmethod device-show-mark ((device qt-device) window x y time)
  )

;;;; Windows

(defmethod device-next-window ((device qt-device) window)
  (with-slots (windows) device
    (elt windows (mod (1+ (or (position window windows) 0))
                      (length windows)))))

(defmethod device-previous-window ((device qt-device) window)
  (with-slots (windows) device
    (elt windows (mod (1- (or (position window windows) 0))
                      (length windows)))))

(defmethod device-delete-window ((device qt-device) window)
  (let* ((hunk (window-hunk window))
         (stream (qt-hunk-widget hunk))
         (item (qt-hunk-item hunk)))
    (if item
        (#_hide stream)
        #+nil (#_removeItem (#_scene item) item)
        (#_close stream))
    (setf (slot-value device 'windows)
          (remove window (slot-value device 'windows)))
    (let ((buffer (window-buffer window)))
      (setf (buffer-windows buffer) (delete window (buffer-windows buffer))))))

(defmethod device-make-window ((device qt-device)
                               start modelinep window font-family
                               ask-user x y width height proportion)
  (declare (ignore window font-family ask-user x y width height))
  (let* ((old-window (current-window))
         (victim (window-hunk old-window)))
    (let* ((old-widget (qt-hunk-widget (window-hunk *current-window*)))
           (scene (#_scene old-widget))
           (new-lines 10)
           (old-lines 15)
           (pos (device-hunk-position victim))
           (new-height (if modelinep (1+ new-lines) new-lines))
           (new-text-pos (if modelinep (1- pos) pos))
           (new-widget (make-instance 'hunk-widget))
           (new-hunk (make-instance 'qt-hunk
                                    :position pos
                                    :height new-height
                                    ;; :text-position new-text-pos
                                    ;; :text-height new-lines
                                    :device device
                                    :widget new-widget))
           (new-window (internal-make-window :hunk new-hunk))
           (metrics (#_new QFontMetrics *font*)))
      (setf (widget-modeline new-widget)
            (widget-modeline old-widget))
      (setf (slot-value new-hunk 'cw) (+ 0 (#_width metrics "m"))
            (slot-value new-hunk 'ch) (+ 2 (#_height metrics)))
      (setf (device-hunk-window new-hunk) new-window)
      (let* ((old-text-pos-diff (- pos (device-hunk-position victim)))
             (old-win-new-pos (- pos new-height)))
        (setf (device-hunk-height victim)
              (- (device-hunk-height victim) new-height))
        (setf (device-hunk-position victim) old-win-new-pos))
      (hi::setup-window-image start new-window new-lines
                              (window-width old-window))
      (prepare-window-for-redisplay new-window)
      (when modelinep
        (setup-modeline-image (line-buffer (mark-line start)) new-window))
      ;; (change-window-image-height old-window old-lines)
      (shiftf (device-hunk-previous new-hunk)
              (device-hunk-previous (device-hunk-next victim))
              new-hunk)
      (shiftf (device-hunk-next new-hunk) (device-hunk-next victim) new-hunk)
      ;; (setf *currently-selected-hunk* nil)
      (setf hi::*screen-image-trashed* t)
      (setf (qt-hunk-item new-hunk) (#_addWidget scene new-widget))
      (let ((p (position *current-window* (slot-value device 'windows))))
        (setf (slot-value device 'windows)
              (append (subseq (slot-value device 'windows) 0 p)
                      (list new-window)
                      (subseq (slot-value device 'windows) p))))
      new-window)))

#+nil
(defmethod w
           ((device qt-device) staprt modelinep window font-family
            ask-user x y width-arg height-arg proportion)
  (let* ((hunk (window-hunk *current-window*))
         (old-hunk-widget (qt-hunk-widget hunk))
         (new (make-instance 'hunk-widget))
         (window (hi::internal-make-window))
         (new-hunk (make-instance 'qt-hunk
                                  :position 10
                                  :widget new)))
    (setf (widget-modeline new) (widget-modeline old-hunk-widget))
    (redraw-widget device window new-hunk *current-buffer* t)
    (let ((p (position *current-window* (slot-value device 'windows))))
      (setf (slot-value device 'windows)
            (append (subseq (slot-value device 'windows) 0 p)
                    (list window)
                    (subseq (slot-value device 'windows) p))))
    (#_addWidget (#_scene old-hunk-widget) new)
    (setf *currently-selected-hunk* nil)
    (setf *screen-image-trashed* t)
    window))

(defmethod resize-event ((instance hunk-widget) resize-event)
  (call-next-qmethod)
  #+nil (#_setMaximumWidth *tabs* (#_width wrapper))
  (note-sheet-region-changed instance))

(defvar *standard-column-width* 80)

(defun standard-width-in-pixels ()
  (+ (* *standard-column-width* (#_width (#_new QFontMetrics *font*) "m"))
     ;; leave space for the gutter on the left side
     ;; (but not the right side, so that the column width is cut off cleanly)
     *gutter*))

(defun offset-on-each-side (widget)
  (if (centerize-widget-p widget)
      (let ((white-width (standard-width-in-pixels))
            (full-width (#_width widget)))
        (max 0.0d0 (/ (- full-width white-width) 2.0d0)))
      0.0d0))

(defmethod device-beep ((device qt-device) stream)
  )

(defclass qt-editor-input (editor-input)
  ())

(defvar *alt-is-meta* t)

(defvar *qt-initialized-p* nil)

(defmethod key-press-event ((instance hunk-widget) event)
  ;; (call-next-qmethod)
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

(defmethod get-key-event
    ((stream qt-editor-input) &optional ignore-abort-attempts-p)
  (hi::%editor-input-method stream ignore-abort-attempts-p))

(defun in-main-qthread-p ()
  (and hi::*in-the-editor*
       (typep (current-device) 'qt-device)))

(defmethod hi::dispatch-events-with-backend ((backend (eql :qt)))
  ;; The whole *INTERESTING-EVENT-RECEIVED* business is here to prevent
  ;; calling into INTERNAL-REDISPLAY too often (which is particularly
  ;; bad because even no-op redisplay is currently expensive enough to
  ;; be noticable, but would seem suboptimal in any case).
  ;;
  ;; #_processEvents as called here is already a blocking call, but
  ;; apparently Qt has a timeout event somewhere that is set to two
  ;; seconds.  As a result, the redisplay loop would call us to block,
  ;; only to enter INTERNAL-REDISPLAY again after 2s, even though no
  ;; events have been received that are relevant to redisplay.
  ;;
  ;; The workaround is to simply go back into Qt until a signal or
  ;; method has indicated that we did something which might have
  ;; affected Hemlock state.
  ;;
  ;; On my machine, this makes the difference between hemlock.qt always
  ;; showing up with 1% CPU usage in top, and not showing up.
  ;;
  ;; Note that processEvents has flags to inhibit processing of certain
  ;; events, which end up matching our relevancy test (user input and
  ;; socket stuff), but they are only useful in the opposite situation
  ;; of not wanting to block, and briefly wanting to ignore those kinds
  ;; of events.
  (setf *interesting-event-received* nil)
  (qt::run-pending)
  (iter (until *interesting-event-received*)
        (#_processEvents (#_QAbstractEventDispatcher::instance)
                         (#_QEventLoop::WaitForMoreEvents))))

(defmethod hi::dispatch-events-no-hang-with-backend ((backend (eql :qt)))
  (#_processEvents (#_QAbstractEventDispatcher::instance)
                   (#_QEventLoop::AllEvents)))

(defmethod unget-key-event (key-event (stream qt-editor-input))
  (hi::un-event key-event stream))

(defmethod clear-editor-input ((stream qt-editor-input))
  ;; hmm?
  )

(defmethod listen-editor-input ((stream qt-editor-input))
  (hi::input-event-next (hi::editor-input-head stream)))

(defun note-sheet-region-changed (hunk-pane)
  (when (slot-boundp hunk-pane 'hunk)
    (qt-window-changed (slot-value hunk-pane 'hunk))
    (hi::internal-redisplay)))

(defvar *font*)
(defvar *tabs*)
(defvar *buffers-to-tabs*)
(defvar *main-stack*)
(defvar *main-hunk-widget*)
(defvar *echo-hunk-widget*)
(defvar *executor*)
(defvar *notifier*)

(defun make-hemlock-widget ()
  (let* ((wrapper (#_new QSplitter))
         (vbox (#_new QVBoxLayout))
         (tabs (#_new QTabBar))
         (main (make-instance 'hunk-widget
                              :centerize t
                              :paint-margin t))
         (echo (make-instance 'hunk-widget))
         (font
          (let ((font (#_new QFont)))
            (#_fromString font *font-family*)
            (#_setPointSize font *font-size*)
            font))
         (*font* font)
         (metrics (#_new QFontMetrics font)))
    (#_addWidget vbox tabs)
    (let ((main-stack (#_new QStackedWidget)))
      (setf *main-stack* main-stack)
      (setf *main-hunk-widget* main)
      (#_addWidget vbox main-stack)
      (#_addWidget main-stack main))
    (let ((x (#_new QWidget)))
      (#_setLayout x vbox)
      (#_addWidget wrapper x))
    (#_addWidget wrapper echo)
    (setf *echo-hunk-widget* echo)
    (#_setOrientation wrapper (#_Qt::Vertical))
    (#_setFocusPolicy tabs (#_Qt::NoFocus))
    (#_setSpacing vbox 0)
    (#_setMargin vbox 0)
    ;; fixme: should be a default, not a strict minimum:
    (#_setMinimumSize wrapper
                      (standard-width-in-pixels)
                      (* 25 (#_height metrics)))
    (#_setMaximumHeight echo 100)
    (values main echo font wrapper tabs)))

(defun add-buffer-tab-hook (buffer)
  (when (in-main-qthread-p)
    (#_addTab *tabs* (buffer-name buffer))))

(defun buffer-tab-index (buffer)
  (dotimes (i (#_count *tabs*) (error "buffer tab missing"))
    (when (equal (#_tabText *tabs* i) (buffer-name buffer))
      (return i))))

(defun delete-buffer-tab-hook (buffer)
  (when (in-main-qthread-p)
    (#_removeTab *tabs* (buffer-tab-index buffer))))

(defun update-buffer-tab-hook (buffer new-name)
  (when (in-main-qthread-p)
    (#_setTabText *tabs*
                (buffer-tab-index buffer)
                new-name)))

(defun set-buffer-tab-hook (buffer)
  (when (in-main-qthread-p)
    (#_setCurrentIndex *tabs* (buffer-tab-index buffer))))

(defun set-stack-widget-hook (buffer)
  (when (in-main-qthread-p)
    (#_setCurrentWidget *main-stack*
                        (or (hi::buffer-widget buffer)
                            *main-hunk-widget*))))

(add-hook hemlock::make-buffer-hook 'add-buffer-tab-hook)
(add-hook hemlock::delete-buffer-hook 'delete-buffer-tab-hook)
(add-hook hemlock::buffer-name-hook 'update-buffer-tab-hook)
(add-hook hemlock::set-buffer-hook 'set-buffer-tab-hook)
(add-hook hemlock::set-buffer-hook 'set-stack-widget-hook)

(defun signal-receiver (function)
  (make-instance 'signal-receiver
                 :function (lambda (&rest args)
                             (setf *interesting-event-received* t)
                             (apply function args))))

(defun connect (source signal cont)
  (let ((receiver (signal-receiver cont)))
    (push receiver *do-not-gc-list*)
    (#_QObject::connect source signal receiver (QSLOT "invoke()"))))

(defun connect/int (source signal cont)
  (let ((receiver (signal-receiver cont)))
    (push receiver *do-not-gc-list*)
    (#_QObject::connect source signal receiver (QSLOT "invoke(int)"))))

(defun connect/string (source signal cont)
  (let ((receiver (signal-receiver cont)))
    (push receiver *do-not-gc-list*)
    (#_QObject::connect source
                        signal
                        receiver
                        (QSLOT "invoke(const QString&)"))))

(defun connect/boolean (source signal cont)
  (let ((receiver (signal-receiver cont)))
    (push receiver *do-not-gc-list*)
    (#_QObject::connect source
                        signal
                        receiver
                        (QSLOT "invoke(bool)"))))

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
                                  args)))
          ("invoke(const QString&)" (lambda (this &rest args)
                                      (apply (signal-receiver-function this)
                                             args)))
          ("invoke(bool)" (lambda (this &rest args)
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

#+(or)
(defun control-g-handler (&rest *)
  (let ((widget *echo-hunk-widget*))
    (cond
      ((#_hasFocus widget)
       (hi::q-event *editor-input* #k"control-g"))
      (t
       (setf *steal-focus-out* t)
       (#_setFocus *echo-hunk-widget*)
       (clear-echo-area)
       (message "Focus restored.  Welcome back to Hemlock.")))))

(defun control-g-handler (&rest *)
  (setf *steal-focus-out* t)
  (#_setFocus *echo-hunk-widget*)
  (clear-echo-area)
  (hi::q-event *editor-input* #k"control-g"))

(defvar *invoke-later-thunks*)
(defvar *invoke-later-timer*)

(defmethod hi::invoke-later ((backend (eql :qt)) fun)
  (push fun *invoke-later-thunks*)
  (#_setSingleShot *invoke-later-timer* t)
  (#_start *invoke-later-timer*))

(defun process-invoke-later-thunks ()
  (iter (while *invoke-later-thunks*)
        (funcall (pop *invoke-later-thunks*))))

(defmethod hi::backend-init-raw-io ((backend (eql :qt)) display)
  (declare (ignore display))
  (setf hi::*editor-input* (make-instance 'qt-editor-input)))

(defmethod hi::%init-screen-manager ((backend-type (eql :qt)) (display t))
  (declare (ignore display))
  (let (main echo widget)
    (setf (values main echo *font* widget *tabs*)
          (make-hemlock-widget))
    (let* ((device (make-instance 'qt-device))
           (window (#_new QMainWindow)))
      (setf (device-name device) "Qt"
            (device-bottom-window-base device) nil)
      ;; keep the QMainWindow from being GCed:
      (setf (device-main-window device) window)
      (#_setWindowTitle window "Hemlock")
      (#_setCentralWidget window widget)
      (let ((menu (#_addMenu (#_menuBar window) "File")))
        (add-command-action menu "Find File")
        (add-command-action menu "Save File")
        (#_addSeparator menu)
        (add-command-action menu "Write File")
        (#_addSeparator menu)
        (add-command-action menu "Save All Files and Exit"))
      (let ((menu (#_addMenu (#_menuBar window) "Lisp")))
        (add-command-action menu "Start Slave Thread")
        (add-command-action menu "Start Slave Process")
        (add-command-action menu "Select Slave")
        (add-command-action menu "Select Eval Buffer"))
      (let ((menu (#_addMenu (#_menuBar window) "Buffer")))
        (add-command-action menu "Bufed")
        (add-command-action menu "Select Buffer"))
      (let ((menu (#_addMenu (#_menuBar window) "Browser")))
        (add-command-action menu "Browse")
        (add-command-action menu "Browse Qt Class")
        (add-command-action menu "CLHS")
        (add-command-action menu "Google")
        (#_addSeparator menu)
        (add-command-action menu "Enter Foreign Widget")
        (add-command-action menu "Leave Foreign Widget"))
      (let ((menu (#_addMenu (#_menuBar window) "Preferences")))
        (add-command-action menu "Select Font")
        (#_addSeparator menu)
        (add-command-action menu "Save Window Geometry")
        (add-command-action menu "Restore Window Geometry"))
      (setf hi::*real-editor-input* *editor-input*)
      (set-up-qt-hunks device main echo nil)
      (setf (widget-modeline main) (#_statusBar window))
      (dolist (buffer hi::*buffer-list*)
        (unless (eq buffer *echo-area-buffer*)
          (add-buffer-tab-hook buffer)))
      (connect/int *tabs*
                   (qsignal "currentChanged(int)")
                   (lambda (index)
                     (change-to-buffer (hemlock-ext::find-buffer (#_tabText *tabs* index)))))
      (connect (#_new QShortcut
                      (#_new QKeySequence "Ctrl+G")
                      (#_window *main-hunk-widget*))
               (QSIGNAL "activated()")
               'control-g-handler)
      (restore-window-geometry window)
      (#_show window)
      ;; undo the minimum set before, so that it's only a default
      ;; (fixme: it still overrides a saved geometry):
      (#_setMinimumSize widget 0 0)
      (setf *notifier* (make-instance 'qt-repl::repl-notifier))
      (setf *executor* (make-instance 'qt-repl::repl-executer
                                      :notifier *notifier*)))))

(defmethod hi::invoke-with-event-loop ((backend (eql :qt)) fun &aux keep)
  (unless *qt-initialized-p*
    ;; HACK!  Disable SBCL's SIGCHLD handler.  I don't know what exactly
    ;; it is doing wrong, but if SBCL sets a signal handler for this, it
    ;; leads to segfaults whenever a process started by Qt exits.
    ;;
    ;; It doesn't matter what the handler would do; a no-op lambda
    ;; already has this effect.
    ;;
    ;; [Perhaps it's due to the way Qt tries to chain call our handler,
    ;; or perhaps we are interrupting FFI code, or is it an altstack thing?
    ;; I have no idea.]
    #+sbcl (sb-kernel::default-interrupt sb-unix:sigchld)

    (format t "Loading libraries [qt")
    (force-output)
    (ensure-smoke :qt)

    (format t ", qtwebkit")
    (force-output)
    (ensure-smoke :qtwebkit)

    (format t "].~%Connecting to window system...")
    (force-output)
    (push (make-qapplication) keep)

    (format t "done.~%")
    (force-output)
    (setf *qt-initialized-p* t))
  ;; When in a slave, we need to create a QEventLoop here, otherwise
  ;; we will segfault later.  Let's just do it unconditionally:
  (push (#_new QEventLoop) keep)
  (let* ((*do-not-gc-list* '())
         (*invoke-later-thunks* '())
         (*invoke-later-timer* (#_new QTimer))
         (*interesting-event-received* nil))
    (connect *invoke-later-timer*
             (QSIGNAL "timeout()")
             #'process-invoke-later-thunks)
    #-sbcl (funcall fun)
    #+sbcl (sb-int:with-float-traps-masked
               (:overflow :invalid :divide-by-zero)
             (funcall fun))))


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

(defun probe-namestring (x)
  (when (and x (probe-file x))
    (etypecase x
      (string x)
      (pathname (namestring x)))))

(defun find-background-svg ()
  (etypecase hemlock:*background-image*
    ((or string pathname)
     (or (probe-namestring hemlock:*background-image*)
         (progn
           (format t "Specified background image not found: ~A~%"
                   hemlock:*background-image*)
           nil)))
    ((eql :auto)
     (or (probe-namestring (merge-pathnames ".hemlock/background.svg"
                                            (user-homedir-pathname)))
         (probe-namestring (merge-pathnames "background.svg"
                                            (hi::installation-directory)))))
    (null)))

(defun qt-window-changed (hunk)
  (let ((widget (qt-hunk-widget hunk)))
    (when (qt-hunk-want-background-p hunk)
      (with-slots (background-pixmap background-pixmap-item)
          widget
        (setf background-pixmap
              (let ((file (find-background-svg)))
                (if file
                    (let* ((renderer (#_new QSvgRenderer file))
                           (w widget)
                           (pixmap (#_new QPixmap (#_width w) (#_height w))))
                      (let ((painter (#_new QPainter pixmap)))
                        (#_render renderer painter)
                        (#_end painter))
                      pixmap)
                    nil)))
        (when background-pixmap-item
          (#_removeItem (#_scene background-pixmap-item) background-pixmap-item)
          (setf background-pixmap-item nil))
        (when background-pixmap
          (setf background-pixmap-item
                (#_addPixmap (#_scene widget)
                             background-pixmap))
          (#_setZValue background-pixmap-item -2)
          #+nil (#_setBackgroundBrush
                 (#_scene widget)
                 (#_new QBrush background-pixmap)))))
    (with-slots (white-item-1 white-item-2)
        widget
      (when white-item-1
        (#_removeItem (#_scene white-item-1) white-item-1)
        (setf white-item-1 nil))
      (when white-item-2
        (#_removeItem (#_scene white-item-2) white-item-2)
        (setf white-item-2 nil))
      (let ((offset (truncate (offset-on-each-side widget))))
        (setf white-item-1
              (#_addRect (#_scene widget)
                         (#_new QRectF
                                offset
                                0
                                (- (#_width widget) (* 2 offset))
                                (#_height widget))
                         (#_new QPen (#_Qt::NoPen))
                         (#_new QBrush
                                (#_new QBrush (#_new QColor 255 255 255 210)))))
        (setf white-item-2
              (#_addRect (#_scene widget)
                         (#_new QRectF
                                (- (#_width widget) offset)
                                0
                                offset
                                (#_height widget))
                         (#_new QPen (#_Qt::NoPen))
                         (#_new QBrush
                                (#_new QBrush (#_new QColor 255 255 255 180))))))
      (#_setZValue white-item-1 -1)
      (#_setZValue white-item-2 -1)))
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

(defun set-up-qt-hunks (device main-widget echo-widget another-widget)
  (let* ((buffer *current-buffer*)
         (start (buffer-start-mark buffer))
         (first (cons dummy-line the-sentinel)))
    (declare (ignorable start first))
    (setf (buffer-windows buffer) nil
          (buffer-windows *echo-area-buffer*) nil)
    (let* ((window (hi::internal-make-window))
           (hunk (make-instance 'qt-hunk
                                :want-background-p t
                                :widget main-widget)))
      (redraw-widget device window hunk buffer t)
      (setf *current-window* window)
      (push window (slot-value device 'windows))
      (setf (device-hunk-previous hunk) hunk)
      (setf (device-hunk-next hunk) hunk)
      (setf (device-hunks device) (list hunk)))
    (when another-widget
      (let* ((window (hi::internal-make-window))
             (hunk (make-instance 'qt-hunk
                                  :want-background-p nil
                                  :widget another-widget)))
        (redraw-widget device window hunk buffer t)
        (push window (slot-value device 'windows))
        (push hunk (device-hunks device))))
    ;;
    (when echo-widget                   ;hmm
      (let ((echo-window (hi::internal-make-window))
            (echo-hunk (make-instance 'qt-hunk
                                      :want-background-p nil
                                      :widget echo-widget)))
        (redraw-widget device echo-window echo-hunk *echo-area-buffer* nil)
        (setf *echo-area-window* echo-window)
        ;; why isn't this on the list of hunks?
        ;; List of hunks isn't used at all.
        ))))

(defvar *font-family*
  "Fixed [Misc]"
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

;;; (defun dis-line-rect (hunk dl)
;;;   (let* ((h (slot-value hunk 'ch))
;;;          (w (slot-value hunk 'cw))
;;;          (xo *gutter*)
;;;          (yo *gutter*)
;;;      (chrs (dis-line-chars dl))
;;;      (start 0)                      ;...
;;;      (end (dis-line-length dl))     ;...
;;;      (x1 (+ xo (* w start)))
;;;      (y1 (+ 1 yo (* (dis-line-position dl) h)))
;;;      (m (#_new QFontMetrics *font*))
;;;      (ww (#_width m (subseq chrs start end)))
;;;      (hh (#_ascent m)))
;;;     (#_new QRect x1 y1 ww (* 2 hh))))

(defun dis-line-rect (hunk dl)
  (nth-line-rect hunk (dis-line-position dl)))

(defun nth-line-rect (hunk i)
  (let* ((x *gutter*)
         (y (+ *gutter* (* i (slot-value hunk 'ch))))
         (w (- (#_width (qt-hunk-widget hunk))
               (ceiling (offset-on-each-side (qt-hunk-widget hunk)))))
         (h (slot-value hunk 'ch)))
    (#_new QRect x y w h)))

(defun cursor-rect (hunk x y)
  (with-slots (cw ch) hunk
    (when (and x y cw ch)
      (#_new QRect
             (+ *gutter* (* x cw))
             (+ *gutter* (* y ch))
             (1+ cw) (1+ ch)))))

(defun clear-line-items (scene hunk position)
  (dolist (old-item (line-items hunk position))
    (#_removeItem scene old-item))
  (setf (line-items hunk position) nil))

(defun update-line-items (scene hunk dl &optional modelinep)
  (let* ((position (dis-line-position dl))
         (h (slot-value hunk 'ch))
         (w (slot-value hunk 'cw))
         (offset (truncate (offset-on-each-side (qt-hunk-widget hunk))))
         (xo (+ offset *gutter*))
         (yo *gutter*)
         (chrs (dis-line-chars dl))
         (y (+ yo (* position h))))
    (unless (zerop (dis-line-flags dl))
      (setf (hi::dis-line-tick dl) (incf *tick*)))
    (clear-line-items scene hunk position)
    (when modelinep
      (setf y (- (#_height (qt-hunk-widget hunk)) h 2)))
    ;; font changes
    (let ((font 0)                      ;###
          (start 0)
          (end (dis-line-length dl))
          (changes (dis-line-font-changes dl)))
      (iter
       (cond ((null changes)
              (push (add-chunk-item scene hunk chrs
                                    (+ xo (* w start))
                                    (+ 1 y)
                                    start end font)
                    (line-items hunk position))
              (return))
             (t
              (push (add-chunk-item scene hunk chrs
                                    (+ xo (* w start))
                                    (+ 1 y)
                                    start (font-change-x changes) font)
                    (line-items hunk position))
              (setf font (font-change-font changes)
                    start (font-change-x changes)
                    changes (font-change-next changes)))))))
  (setf (dis-line-flags dl) unaltered-bits (dis-line-delta dl) 0))

(defun clear-all-line-items (scene hunk)
  (with-slots (itab) hunk
    (iter
     (for i from 0)
     (for items in-vector itab)
     (dolist (old-item items)
       (#_removeItem scene old-item))
     (setf (elt itab i) nil))))

;; Smart isn't very smart, but still much better for "a single line changed"
;; kind of situations.
(defun dumb-or-smart-redisplay (device window dumb)
  (let* ((widget (qt-hunk-widget (window-hunk window)))
         (hunk (window-hunk window))
         (first (window-first-line window))
         (offset (truncate (offset-on-each-side widget)))
         (scene (#_scene widget)))

    (when dumb
      (clear-all-line-items scene hunk))

    ;; "empty" lines
    (let ((pos (dis-line-position (car (window-last-line window))))
          (old (window-old-lines window)))
      (when (and pos old)
        (iter:iter (iter:for i from (1+ pos) to old)
                   (clear-line-items scene hunk i)))
      (setf (window-old-lines window) pos))

    ;; render "changed" lines
    (do ((i 0 (1+ i))
         (dl (cdr first) (cdr dl)))
        ((eq dl the-sentinel)
         (setf (window-old-lines window) (1- i)))
      (when (or dumb (plusp (dis-line-flags (car dl))))
        (update-line-items scene hunk (car dl))))

    ;; modeline
    (when (window-modeline-buffer window)
      (update-modeline-fields (window-buffer window) window)
      (#_showMessage (widget-modeline widget)
                     (subseq (window-modeline-buffer window)
                             0
                             (window-modeline-buffer-len window)))
      (setf (dis-line-flags (window-modeline-dis-line window))
            unaltered-bits)))

  ;; tell the redisplay algorithm that we did our job, otherwise it
  ;; retries forever:
  (let* ((first (window-first-line window))
         (hunk (window-hunk window))
         (device (device-hunk-device hunk)))
    (setf (window-first-changed window) the-sentinel
          (window-last-changed window) first)))

(defun add-chunk-item (scene hunk string x y start end font)
  (declare (ignore font))
;;;        (#_setPen painter (#_black "Qt"))
;;;        (#_setFont painter *font*)
  #+nil (incf y (#_ascent (#_new QFontMetrics *font*)))
  (let ((item
         (#_addSimpleText scene (subseq string start end) *font*)))
    #+nil (#_setPen item (#_new QPen (#_new QColor (random 255) (random 255) (random 255))))
    (#_setPos item x y)
    #+nil (#_setZValue item 1)
    item))

(defun make-virtual-buffer (name widget &rest args)
  (let ((buffer (apply #'make-buffer name args)))
    (when buffer
      (setf (buffer-writable buffer) nil)
      (setf (hi::buffer-widget buffer) widget)
      (#_addWidget *main-stack* widget)
      buffer)))

;;

(defcommand "Enter Foreign Widget" (p)
  "" ""
  (declare (ignore p))
  (let ((widget (hi::buffer-widget (current-buffer))))
    (unless widget
      (editor-error "Not a foreign widget."))
    (setf *steal-focus-out* nil)
    (#_setFocus widget)
    (clear-echo-area)
    (message "Focus set to foreign widget. Type C-g to go back.")))

(defcommand "Leave Foreign Widget" (p)
  "Like control-g-handler, except for the C-g behaviour." ""
  (declare (ignore p))
  (let ((echo *echo-hunk-widget*))
    (unless (#_hasFocus echo)
      (setf *steal-focus-out* t)
      (#_setFocus echo)
      (clear-echo-area)
      (message "Focus restored.  Welcome back to Hemlock."))))

(defcommand "Disable Steal Focus" (p)
  "" ""
  (declare (ignore p))
  (setf *steal-focus-out* nil)
  (message "Focus stealing disabled"))

(defcommand "Enable Steal Focus" (p)
  "" ""
  (declare (ignore p))
  (setf *steal-focus-out* t)
  (#_setFocus *echo-hunk-widget*))

(defcommand "Abc" (p)
  "" ""
  (let ((w (qt-hunk-item (window-hunk (current-window)))))
    (#_setZValue w 500)
    (#_setTransform (let* ((old (qt::qobject-class w))
                           (old-ptr (qt::qobject-pointer w))
                           (new (qt::find-qclass "QGraphicsItem"))
                           (new-ptr (qt::%cast old-ptr
                                               (qt::unbash old)
                                               (qt::unbash new))))
                      (make-instance 'qt::qobject
                                     :class new
                                     :pointer new-ptr))
                    (#_translate (if p
                                     (#_rotate (#_scale (#_new QTransform) 0.75 0.75) 45)
                                     (#_new QTransform)) 200 0)
                    nil)))

(defcommand "Def" (p)
  "" ""
  (let* ((widget #+nil (#_new QWebView) (#_new QPushButton "test"))
         (w (#_addWidget
             (#_scene (qt-hunk-widget (window-hunk (current-window))))
             widget)))
    (push widget *do-not-gc-list*)
    (push w *do-not-gc-list*)
    #+nil (#_setUrl widget (#_new QUrl "file:///etc"))
    (#_setZValue w 500)
    #+nil
    (#_setTransform (let* ((old (qt::qobject-class w))
                           (old-ptr (qt::qobject-pointer w))
                           (new (qt::find-qclass "QGraphicsItem"))
                           (new-ptr (qt::%cast old-ptr
                                               (qt::unbash old)
                                               (qt::unbash new))))
                      (make-instance 'qt::qobject
                                     :class new
                                     :pointer new-ptr))
                    (#_translate (if p
                                     (#_rotate (#_scale (#_new QTransform) 0.75 0.75) 45)
                                     (#_new QTransform)) 200 0)
                    nil)))
