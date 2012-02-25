;;;; -*- Mode: Lisp; indent-with-tabs: nil -*-

(in-package :hemlock.qt)

(pushnew :qt hi::*available-backends*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :hemlock.qt)
    (named-readtables:defreadtable :hemlock.qt
      (:merge :qt)
      (:dispatch-macro-char #\# #\k 'hemlock-ext::parse-key-fun))))

(named-readtables:in-readtable :hemlock.qt)

(defun enable-syntax ()
  (named-readtables:in-readtable :hemlock.qt)
  nil)

(defvar *settings-organization* "Hemlock")
(defvar *settings-application* "Hemlock")

(defvar *font*)
(defvar *modeline-font*)
(defvar *tabs*)
(defvar *buffers-to-tabs*)
(defvar *main-stack*)
(defvar *main-hunk-widget*)
(defvar *executor*)
(defvar *notifier*)

(defun qsettings ()
  (#_new QSettings
         *settings-organization*
         *settings-application*))

(defun save-window-geometry (window)
  (with-object (sx (qsettings))
    (#_setValue sx
                "geometry"
                (#_new QVariant (#_saveGeometry window)))))

(defcommand "Save Window Geometry" (p)
  "Save current window's geometry in Qt settings." ""
  (declare (ignore p))
  (save-window-geometry
   (#_window (qt-hunk-widget (window-hunk (current-window))))))

(defun restore-window-geometry (window)
  (with-object (sx (qsettings))
    (#_restoreGeometry window
                       (#_toByteArray (#_value sx "geometry")))))

(defcommand "Restore Window Geometry" (p)
  "Restore the current window's geometry from Qt settings." ""
  (declare (ignore p))
  (restore-window-geometry
   (#_window (qt-hunk-widget (window-hunk (current-window))))))

(defun save-font ()
  (with-object (sx (qsettings))
    (format t "setting font ~A~%" (#_toString *font*))
    (#_setValue sx "main font" (#_toString *font*))))

(defun qvariant-string (x)
  ;; fixme: CommonQt now unmarshals string QVariants automatically, so
  ;; (#_toString ...) fails on those pre-unmarshalled strings.  But
  ;; sometimes we get a default QVariant, which CommonQt doesn't unpack,
  ;; and we need to call (#_toString) after all.  This doesn't seem
  ;; ideal...
  (if (stringp x)
      x
      (#_toString x)))

(defun restore-font ()
  (with-object (sx (qsettings))
    (let ((str (qvariant-string (#_value sx "main font"))))
      (when (plusp (length str))
        (setf *font*
              (let ((font (#_new QFont)))
                (#_fromString font str)
                font))))))

(defcommand "Select Font" (p)
  "Open a font dialog and change the current display font." ""
  (declare (ignore p))
  (let (font)
    (unless (qt::with-&bool (arg nil)
              (setf font (#_QFontDialog::getFont arg *font*)))
      (editor-error "Font dialog cancelled"))
    (setf *font* font)
    (save-font)))

(defparameter *gutter* 10
  "The gutter to place between between the matter in a hemlock pane and its
   margin to improve legibility (sp?, damn i miss ispell).")

(defclass qt-device (device)
  ((cursor-hunk :initform nil
                :documentation "The hunk that has the cursor."
                :accessor device-cursor-hunk)
   (cursor-item :initform nil
                :accessor device-cursor-item)
   #+nil (windows :initform nil)
   (main-window :initform nil
                :accessor device-main-window)))

(defun current-device ()
  (device-hunk-device (window-hunk (current-window))))

(defclass qt-hunk (device-hunk)
  ((widget :initarg :widget
           :reader qt-hunk-widget)
   (native-widget-item :initform nil
                       :accessor hunk-native-widget-item)
   (item :initarg :item
         :accessor qt-hunk-item)
   (item-group :initform nil
               :accessor hunk-item-group)
   (background-items :initform nil
                    :accessor hunk-background-items)
   (want-background-p :initarg :want-background-p
                      :initform nil
                      :accessor qt-hunk-want-background-p)
   (itab :initform (make-array 0 :adjustable t :initial-element nil))
   (text-position :initarg :text-position
                  :accessor device-hunk-text-position)
   (text-height :initarg :text-height
                :accessor device-hunk-text-height)
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
                   :accessor hunk-widget-rect-pixmap-item)
     (height :initform 0
             :accessor hunk-widget-height))
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsView")
  (:override ("resizeEvent" resize-event)
             ("keyPressEvent" key-press-event)
             ("focusOutEvent" focus-out-event)
             ("event" intercept-event)
             ("contextMenuEvent" context-menu-event)
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
  (#_setScene instance (#_new QGraphicsScene instance))
  (#_setHorizontalScrollBarPolicy instance (#_Qt::ScrollBarAlwaysOff))
  (#_setVerticalScrollBarPolicy instance (#_Qt::ScrollBarAlwaysOff)))

(defmethod device-init ((device qt-device))
  ;; (redisplay-all)
  )

(defmethod device-exit ((device qt-device)))

(defmacro with-timer (&body body)
  `(call-with-timer (lambda () ,@body)))

(defun call-with-timer (fun)
  (let ((a (get-internal-real-time)))
    (multiple-value-prog1
        (funcall fun)
      (let ((b (get-internal-real-time)))
        (format *trace-output*
                " ~D"
                (round(* (- b a) (/ 1000 internal-time-units-per-second))))
        (force-output *trace-output*)))))

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

(defvar *processing-events-p* nil)

(defun exhaustively-dispatch-events-no-hang ()
  ;; Must dispatch all remaining events here (but not actually block).
  ;;
  ;; Redisplay can lead to events being posted, and Hemlock's event loop
  ;; is built to alternate between INTERNAL-REDISPLAY and
  ;; DISPATCH-EVENTS, with the expectation that only meaningful events
  ;; like keyboard or socket interaction will make event dispatching
  ;; return.  So if redisplay exited with a pending event,
  ;; editor-input-method would degenerate into a busy loop.
  (assert (not *processing-events-p*))
  (let ((ev (#_QAbstractEventDispatcher::instance))
        (*processing-events-p* t))
    (iter (while (#_processEvents ev (#_QEventLoop::AllEvents))))))

(defmethod device-force-output ((device qt-device))
  (unless *processing-events-p*
    (exhaustively-dispatch-events-no-hang)))

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
              (with-objects ((path (#_new QPainterPath))
                             (pen (#_new QPen (#_Qt::NoPen)))
                             (color (#_new QColor 0 180 180 64))
                             (brush (#_new QBrush color)))
                (#_addRect path 0 0 cw ch)
                (let* ((scene (#_scene (qt-hunk-widget hunk)))
                       (group (ensure-hunk-item-group scene hunk))
                       (item (#_new QGraphicsPathItem path group)))
                  (qt::cancel-finalization item)
                  (#_setPen item pen)
                  (#_setBrush item brush)
                  item))))
      (#_setPos cursor-item
                (* x cw)
                (* y ch)))
    (#_setZValue cursor-item 3)))

(defmethod device-show-mark ((device qt-device) window x y time)
  )

;;;; Windows

(defmethod device-next-window ((device qt-device) window)
  (device-hunk-window (device-hunk-next (window-hunk window))))

(defmethod device-previous-window ((device qt-device) window)
  (device-hunk-window (device-hunk-previous (window-hunk window))))

(defvar *currently-selected-hunk* nil)

(defmethod device-delete-window ((device qt-device) window)
  (let* ((hunk (window-hunk window))
         (prev (device-hunk-previous hunk))
         (next (device-hunk-next hunk))
         (device (device-hunk-device hunk))
         (group (hunk-item-group hunk)))
    (when group
      (when (eq hunk (device-cursor-hunk device))
        (setf (device-cursor-item device) nil))
      (setf (hunk-native-widget-item hunk) nil)
      (with-slots (itab) hunk
        (fill itab nil))
      (#_delete group))
    (setf (device-hunk-next prev) next)
    (setf (device-hunk-previous next) prev)
    (let ((buffer (window-buffer window)))
      (setf (buffer-windows buffer) (delete window (buffer-windows buffer))))
    (let ((new-lines (device-hunk-height hunk)))
      (declare (fixnum new-lines))
      (cond ((eq hunk (device-hunks (device-hunk-device next)))
             (incf (device-hunk-height next) new-lines)
             (incf (device-hunk-text-height next) new-lines)
             (let ((w (device-hunk-window next)))
               (hi::change-window-image-height w (+ new-lines (window-height w)))))
            (t
             (incf (device-hunk-height prev) new-lines)
             (incf (device-hunk-position prev) new-lines)
             (incf (device-hunk-text-height prev) new-lines)
             (incf (device-hunk-text-position prev) new-lines)
             (let ((w (device-hunk-window prev)))
               (hi::change-window-image-height w (+ new-lines (window-height w)))))))
    (when (eq hunk (device-hunks device))
      (setf (device-hunks device) next)))
  (setf *currently-selected-hunk* nil)
  (setf hi::*screen-image-trashed* t))

(defmethod device-make-window ((device qt-device)
                               start modelinep window font-family
                               ask-user x y width height proportion)
  (declare (ignore window font-family ask-user x y width height))
  (let* ((old-window (current-window))
         (victim (window-hunk old-window))
         (text-height (device-hunk-text-height victim))
         (availability (if modelinep (1- text-height) text-height)))
    (when (> availability 1)
      (let* ((new-lines (truncate (* availability proportion)))
             (old-lines (- availability new-lines))
             (pos (device-hunk-position victim))
             (new-height (if modelinep (1+ new-lines) new-lines))
             (new-text-pos (if modelinep (1- pos) pos))
             (widget (qt-hunk-widget (window-hunk *current-window*)))
             (new-hunk (make-instance 'qt-hunk
                                      :position pos
                                      :height new-height
                                      :text-position new-text-pos
                                      :text-height new-lines
                                      :device device
                                      :widget widget))
             (new-window (internal-make-window :hunk new-hunk)))
        (with-object (metrics (#_new QFontMetrics *font*))
          (setf (slot-value new-hunk 'cw) (+ 0 (#_width metrics "m"))
                (slot-value new-hunk 'ch) (+ 2 (#_height metrics))))
        (setf (device-hunk-window new-hunk) new-window)
        (let* ((old-text-pos-diff (- pos (device-hunk-text-position victim)))
               (old-win-new-pos (- pos new-height)))
          (declare (fixnum old-text-pos-diff old-win-new-pos))
          (setf (device-hunk-height victim)
                (- (device-hunk-height victim) new-height))
          (setf (device-hunk-text-height victim) old-lines)
          (setf (device-hunk-position victim) old-win-new-pos)
          (setf (device-hunk-text-position victim)
                (- old-win-new-pos old-text-pos-diff)))
        (hi::setup-window-image start new-window new-lines
                                (window-width old-window))
        (prepare-window-for-redisplay new-window)
        (when modelinep
          (setup-modeline-image (line-buffer (mark-line start)) new-window))
        (hi::change-window-image-height old-window old-lines)
        (shiftf (device-hunk-previous new-hunk)
                (device-hunk-previous (device-hunk-next victim))
                new-hunk)
        (shiftf (device-hunk-next new-hunk) (device-hunk-next victim) new-hunk)
        (setf *currently-selected-hunk* nil)
        (setf hi::*screen-image-trashed* t)
        new-window))))

(defmethod resize-event ((widget hunk-widget) resize-event)
  (call-next-qmethod)
  #+nil (#_setMaximumWidth *tabs* (#_width wrapper))
  (update-full-screen-items widget)
  (hi::enlarge-device (current-device)
                      (recompute-hunk-widget-height widget))
  (hi::internal-redisplay))

(defvar *standard-column-width* 80)

(defun standard-width-in-pixels ()
  (with-object (metrics (#_new QFontMetrics *font*))
    (+ (* *standard-column-width* (#_width metrics "m"))
       ;; leave space for the gutter on the left side
       ;; (but not the right side, so that the column width is cut off cleanly)
       *gutter*)))

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

(defmethod context-menu-event ((instance hunk-widget) event)
  ;; (call-next-qmethod)
  (let ((menu (#_new QMenu)))
    (add-menus menu)
    (#_exec menu (#_globalPos event))))

(defmethod key-press-event ((instance hunk-widget) event)
  ;; (call-next-qmethod)
  (hi::q-event *editor-input* (qevent-to-key-event event)))

(defun parse-modifiers (event)
  (let ((mods (#_modifiers event)))
    (logior (if (logtest mods
                         (qt::primitive-value (#_Qt::ControlModifier)))
                (hemlock-ext:key-event-bits #k"control-a")
                0)
            (if (or (logtest mods
                             (qt::primitive-value (#_Qt::MetaModifier)))
                    (and *alt-is-meta*
                         (logtest mods
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
  (assert (not *processing-events-p*))
  (setf *interesting-event-received* nil)
  (let ((*processing-events-p* t))
    (iter (until *interesting-event-received*)
          (#_processEvents (#_QAbstractEventDispatcher::instance)
                           (#_QEventLoop::WaitForMoreEvents)))))

(defmethod hi::dispatch-events-no-hang-with-backend ((backend (eql :qt)))
  (assert (not *processing-events-p*))
  (let ((*processing-events-p* t))
    (#_processEvents (#_QAbstractEventDispatcher::instance)
                     (#_QEventLoop::AllEvents))))

(defmethod unget-key-event (key-event (stream qt-editor-input))
  (hi::un-event key-event stream))

(defmethod clear-editor-input ((stream qt-editor-input))
  ;; hmm?
  )

(defmethod listen-editor-input ((stream qt-editor-input))
  (hi::input-event-next (hi::editor-input-head stream)))

(defun make-hemlock-widget ()
  (let* ((vbox (#_new QVBoxLayout))
         (tabs (#_new QTabBar))
         (main (make-instance 'hunk-widget
                              :centerize t
                              :paint-margin t))
         (font
          (let ((font (#_new QFont)))
            (#_fromString font *font-family*)
            (#_setPointSize font *font-size*)
            font))
         (*font* font)
         (font (progn (restore-font) *font*))
         (*modeline-font*
          (let ((font (#_new QFont)))
            (#_fromString font *modeline-font-family*)
            (#_setPointSize font (#_pointSize *font*))
            font)))
    (#_addWidget vbox tabs)
    (#_hide tabs)
    (let ((main-stack (#_new QStackedWidget)))
      (setf *main-stack* main-stack)
      (setf *main-hunk-widget* main)
      (#_addWidget vbox main-stack)
      (#_addWidget main-stack main))
    (#_setFocusPolicy tabs (#_Qt::NoFocus))
    (#_setSpacing vbox 0)
    (#_setMargin vbox 0)
    (let ((central-widget (#_new QWidget)))
      (#_setLayout central-widget vbox)
      (values main font *modeline-font* central-widget tabs))))

(defun add-buffer-tab-hook (buffer)
  (when (in-main-qthread-p)
    (#_addTab *tabs* (buffer-name buffer))))

(defun buffer-tab-index (buffer)
  (dotimes (i (#_count *tabs*))
    (when (equal (#_tabText *tabs* i) (buffer-name buffer))
      (return i))))

(defun delete-buffer-tab-hook (buffer)
  (when (in-main-qthread-p)
    (let ((idx (buffer-tab-index buffer)))
      (if idx
          (#_removeTab *tabs* idx)
          (warn "buffer tab missing")))))

(defun update-buffer-tab-hook (buffer new-name)
  (when (in-main-qthread-p)
    (let ((idx (buffer-tab-index buffer)))
      (if idx
          (#_setTabText *tabs* idx new-name)
          (warn "buffer tab missing")))))

(defun set-buffer-tab-hook (buffer)
  (when (in-main-qthread-p)
    (let ((idx (buffer-tab-index buffer)))
      (if idx
          (#_setCurrentIndex *tabs* idx)
          (warn "buffer tab missing")))))

(defun set-stack-widget-hook (buffer)
  (when (in-main-qthread-p)
    (#_setCurrentWidget *main-stack*
                        *main-hunk-widget*)))

(add-hook hemlock::make-buffer-hook 'add-buffer-tab-hook)
(add-hook hemlock::delete-buffer-hook 'delete-buffer-tab-hook)
(add-hook hemlock::buffer-name-hook 'update-buffer-tab-hook)
(add-hook hemlock::set-buffer-hook 'set-buffer-tab-hook)
(add-hook hemlock::set-buffer-hook 'set-stack-widget-hook)

(defun splitter-sizes (splitter)
  (qt::qlist-to-list (#_sizes splitter)))

(defun (setf splitter-sizes) (newval splitter)
  (#_setSizes splitter (qt::qlist-append (qt::make-qlist<int>) newval))
  newval)

(defun resize-echo-area (nlines backgroundp)
  (setf (device-hunk-height (window-hunk *echo-area-window*))
        nlines)
  (setf (device-hunk-text-height (window-hunk *echo-area-window*))
        nlines)
  (hi::change-window-image-height *echo-area-window* nlines)
  (setf (qt-hunk-want-background-p (window-hunk *echo-area-window*))
        backgroundp)
  (redisplay-all)
  #+nil
  (let* ((widget (or widget
                     (qt-hunk-widget (window-hunk *echo-area-window*))))
         (splitter (#_centralWidget (#_window widget)))
         (new-height (with-object (metrics (#_new QFontMetrics *font*))
                       (+ (* 2 *gutter*)
                          (* nlines (#_height metrics))))))
    (#_setMinimumHeight widget 1)
    (destructuring-bind (top bottom)
        (splitter-sizes splitter)
      (let ((diff (- new-height bottom)))
        (setf (splitter-sizes splitter)
              (list (- top diff) new-height))))))

(defun minimize-echo-area ()
  (resize-echo-area 1 nil))

(defun enlarge-echo-area ()
  (resize-echo-area 5 t))

(defun set-window-hook (new-window)
  (when (in-main-qthread-p)
    (cond
      ((eq new-window *echo-area-window*)
       (enlarge-echo-area))
      ((eq *current-window* *echo-area-window*)
       (minimize-echo-area)))))

(add-hook hemlock::set-window-hook 'set-window-hook)

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
  (#_setFocus *main-hunk-widget*)
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

(defun add-menus (parent)
  (let ((menu (#_addMenu parent "File")))
    (add-command-action menu "Find File")
    (add-command-action menu "Save File")
    (#_addSeparator menu)
    (add-command-action menu "Write File")
    (#_addSeparator menu)
    (add-command-action menu "Save All Files and Exit"))
  (let ((menu (#_addMenu parent "View")))
    (add-command-action menu "Toggle Menu Bar")
    (add-command-action menu "Toggle Tab Bar")
    (add-command-action menu "Toggle Full Screen"))
  (let ((menu (#_addMenu parent "Lisp")))
    (add-command-action menu "Start Slave Thread")
    (add-command-action menu "Start Slave Process")
    (add-command-action menu "List Slaves"))
  (let ((menu (#_addMenu parent "Buffer")))
    (add-command-action menu "Bufed"))
  (let ((menu (#_addMenu parent "Browser")))
    (add-command-action menu "Browse")
    (add-command-action menu "Browse Qt Class")
    (add-command-action menu "CLHS")
    (add-command-action menu "Google")
    (#_addSeparator menu)
    (add-command-action menu "Enter Foreign Widget")
    (add-command-action menu "Leave Foreign Widget"))
  (let ((menu (#_addMenu parent "Preferences")))
    (add-command-action menu "Select Font")
    (#_addSeparator menu)
    (add-command-action menu "Save Window Geometry")
    (add-command-action menu "Restore Window Geometry")))

(defmethod hi::%init-screen-manager ((backend-type (eql :qt)) (display t))
  (declare (ignore display))
  (let (main central-widget)
    (setf (values main *font* *modeline-font* central-widget *tabs*)
          (make-hemlock-widget))
    (let* ((device (make-instance 'qt-device))
           (window (#_new QMainWindow)))
      (setf (device-name device) "Qt"
            (device-bottom-window-base device) nil)
      ;; keep the QMainWindow from being GCed:
      (setf (device-main-window device) window)
      (#_setWindowTitle window "Hemlock")
      (#_setCentralWidget window central-widget)
      (add-menus (#_menuBar window))
      (#_hide (#_menuBar window))
      (setf hi::*real-editor-input* *editor-input*)
      (set-up-qt-hunks device main)
      (dolist (buffer hi::*buffer-list*)
        (unless (eq buffer *echo-area-buffer*)
          (add-buffer-tab-hook buffer)))
      (connect/int *tabs*
                   (qsignal "currentChanged(int)")
                   (lambda (index)
                     (change-to-buffer (hemlock-ext::find-buffer (#_tabText *tabs* index)))))
      (with-object (key (#_new QKeySequence "Ctrl+G"))
        (connect (#_new QShortcut key (#_window *main-hunk-widget*))
                 (QSIGNAL "activated()")
                 'control-g-handler))
      #+nil
      (#_setMinimumHeight echo
                          (with-object (metrics (#_new QFontMetrics *font*))
                            (+ (* 2 *gutter*) (#_height metrics))))
      (restore-window-geometry window)
      (#_show window)
      ;; (minimize-echo-area echo)
      ;; undo the minimum set before, so that it's only a default
      ;; (fixme: it still overrides a saved geometry):
      #+nil (#_setMinimumSize widget 0 0)
      (setf *notifier* (make-instance 'qt-repl::repl-notifier))
      (setf *executor* (make-instance 'qt-repl::repl-executer
                                      :notifier *notifier*)))))

(defmethod hi::make-event-loop ((backend (eql :qt)))
  'qt-event-loop)

(defmethod hi::invoke-with-existing-event-loop ((backend (eql :qt)) loop fun)
  (assert (eq loop 'qt-event-loop))
  (hi::invoke-with-new-event-loop backend fun))

(defmethod hi::invoke-with-new-event-loop ((backend (eql :qt)) fun &aux keep)
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

    (format t "Loading shared libraries [")
    (let ((first t))
      (dolist (module '(:qtcore :qtgui :qtnetwork :qtsvg :qtwebkit))
        (if first
            (setf first nil)
            (write-string ", "))
        (format t "~A" (string-downcase module))
        (force-output)
        (ensure-smoke module)))

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

(defun update-full-screen-items (widget)
  (when t ;;(qt-hunk-want-background-p hunk)
    (with-slots (background-pixmap background-pixmap-item)
        widget
      (setf background-pixmap
            (let ((file (find-background-svg)))
              (if file
                  (let ((pixmap (#_new QPixmap
                                       (#_width widget)
                                       (#_height widget))))
                    (with-objects
                        ((renderer (#_new QSvgRenderer file))
                         (painter (#_new QPainter pixmap)))
                      (#_render renderer painter)
                      (#_end painter)
                      pixmap))
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
      (with-object (~)
        (setf white-item-1
              (#_addRect (#_scene widget)
                         (#_new QRectF
                                offset
                                0
                                (- (#_width widget) (* 2 offset))
                                (#_height widget))
                         (~ (#_new QPen (#_Qt::NoPen)))
                         (~ (#_new QBrush
                                   (~ (#_new QBrush (~ (#_new QColor 255 255 255 210)))))))))
      (with-object (~)
        (setf white-item-2
              (#_addRect (#_scene widget)
                         (~ (#_new QRectF
                                   (- (#_width widget) offset)
                                   0
                                   offset
                                   (#_height widget)))
                         (~ (#_new QPen (#_Qt::NoPen)))
                         (~ (#_new QBrush
                                   (~ (#_new QBrush (~ (#_new QColor 255 255 255 180))))))))))
    (#_setZValue white-item-1 -1)
    (#_setZValue white-item-2 -1)))

(defun old-resize-junk ()
  #+nil
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

(defmethod hi::device-enlarge-window ((device qt-device) window offset)
  (let* ((hunk (window-hunk window))
         (victim
          (cond
            ((eq hunk (device-hunks (device-hunk-device hunk)))
             ;; we're the first hunk
             (let ((victim (device-hunk-next hunk)))
               (when (eq hunk victim)
                 ;; ... the first and only hunk
                 (editor-error "Cannot enlarge only window"))
               ;; move the victim down
               (incf (device-hunk-position hunk) offset)
               (incf (device-hunk-text-position hunk) offset)
               victim))
            (t
             ;; we're not first hunk, so there is a victim in front of us
             ;; move us up
             (let ((victim (device-hunk-previous hunk)))
               (decf (device-hunk-position victim) offset)
               (decf (device-hunk-text-position victim) offset)
               victim)))))
    ;; bump up our height
    (incf (device-hunk-height hunk) offset)
    (incf (device-hunk-text-height hunk) offset)
    ;; make the victim smaller
    (decf (device-hunk-height victim) offset)
    (decf (device-hunk-text-height victim) offset)
    ;; housekeeping
    (let ((w (device-hunk-window victim)))
      (hi::change-window-image-height w (- offset (window-height w))))
    (let ((w (device-hunk-window hunk)))
      (hi::change-window-image-height w (+ offset (window-height w))))
    (setf hi::*screen-image-trashed* t)))

(defmethod hi::enlarge-device
    ((device qt-device) offset)
  #+nil (hi::set-up-screen-image device)
  (let ((first (device-hunks device)))
    (incf (device-hunk-position first) offset)
    (incf (device-hunk-text-position first) offset)
    (incf (device-hunk-height first) offset)
    (incf (device-hunk-text-height first) offset)
    (let ((w (device-hunk-window first)))
      (hi::change-window-image-height w (+ offset (window-height w))))
    (do ((hunk (device-hunk-next first) (device-hunk-next hunk)))
        ((eq hunk first))
      (incf (device-hunk-position hunk) offset)
      (incf (device-hunk-text-position hunk) offset))
    (let ((hunk (window-hunk *echo-area-window*)))
      (incf (device-hunk-position hunk) offset)
      (incf (device-hunk-text-position hunk) offset))
    (setf hi::*screen-image-trashed* t)))

(defun recompute-hunk-widget-height (widget)
  (let ((new (with-object (metrics (#_new QFontMetrics *font*))
               (max 2 (floor (- (#_height widget)
                                (* 2 *gutter*))
                             (+ 2 (#_height metrics))))))
        (old (hunk-widget-height widget)))
    (setf (hunk-widget-height widget) new)
    (- new old)))

(defun set-up-qt-hunks (device main-widget)
  (progn ;;with-object (metrics (#_new QFontMetrics *font*))
    (let* ((buffer *current-buffer*)
           (start (buffer-start-mark buffer))
           (first (cons dummy-line the-sentinel))
           #+nil
           (width (max 5 (floor (- (#_width (qt-hunk-widget hunk))
                                   (* 2 *gutter*))
                                (+ 0 (#_width metrics "m")))))
           (height (recompute-hunk-widget-height main-widget))
           (echo-height #+nil (value hemlock::echo-area-height)
                        1)
           (main-lines (- height echo-height 1)) ;-1 for echo modeline.
           (main-text-lines (1- main-lines))     ;also main-modeline-pos
           )
      (declare (ignorable start first))
      (setf (buffer-windows buffer) nil
            (buffer-windows *echo-area-buffer*) nil)
      (let* ((window (hi::internal-make-window))
             (last-text-line (1- main-text-lines))
             (hunk (make-instance 'qt-hunk
                                  :position main-lines ;main-text-lines
                                  :height main-lines
                                  :text-position last-text-line
                                  :text-height main-text-lines
                                  :widget main-widget)))
        (redraw-widget device window hunk buffer t)
        (setf *current-window* window)
        #+nil (push window (slot-value device 'windows))
        (setf (device-hunk-previous hunk) hunk)
        (setf (device-hunk-next hunk) hunk)
        (setf (device-hunks device) hunk))
      (let ((echo-window (hi::internal-make-window))
            (echo-hunk (make-instance 'qt-hunk
                                      :position (1- height)
                                      :height echo-height
                                      :text-position (- height 2)
                                      :text-height echo-height
                                      :widget main-widget)))
        (redraw-widget device echo-window echo-hunk *echo-area-buffer* nil)
        (setf *echo-area-window* echo-window)))))

(defvar *font-family*
  "Fixed [Misc]"
  #+nil "Courier New")

(defvar *modeline-font-family*
  "Sans")

(defvar *font-size*
  10)

(defun redraw-widget (device window hunk buffer modelinep)
  (setf (slot-value (qt-hunk-widget hunk) 'hunk)
        hunk)
  (let* ((start (buffer-start-mark buffer))
         (first (cons dummy-line the-sentinel))
         (font *font*)
         width height)
    (with-object (metrics (#_new QFontMetrics font))
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
       ;; (device-hunk-position hunk) 0
       ;; (device-hunk-height hunk) height
       (device-hunk-next hunk) nil
       (device-hunk-previous hunk) nil
       (device-hunk-device hunk) device

       (window-tick window) -1  ; The last time this window was updated.
       (window-%buffer window) buffer ; buffer displayed in this window.
       (window-height window) (device-hunk-height hunk)  ; Height of window in lines.
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
      (hi::update-window-image window))))

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

#+(or)
(defun dis-line-rect (hunk dl)
  (nth-line-rect hunk (dis-line-position dl)))

#+(or)
(defun nth-line-rect (hunk i)
  (let* ((x *gutter*)
         (y (+ *gutter* (* i (slot-value hunk 'ch))))
         (w (- (#_width (qt-hunk-widget hunk))
               (ceiling (offset-on-each-side (qt-hunk-widget hunk)))))
         (h (slot-value hunk 'ch)))
    (#_new QRect x y w h)))

#+(or)
(defun cursor-rect (hunk x y)
  (with-slots (cw ch) hunk
    (when (and x y cw ch)
      (#_new QRect
             (+ *gutter* (* x cw))
             (+ *gutter* (* y ch))
             (1+ cw) (1+ ch)))))

(defun clear-line-items (scene hunk position)
  (declare (ignore scene))
  (dolist (old-item (line-items hunk position))
    (#_delete old-item))
  (setf (line-items hunk position) nil))

(defun update-modeline-items (scene hunk dl)
  (let* ((position (+ (dis-line-position dl)
                      ;; fixme?
                      (device-hunk-text-height hunk)))
         (h (slot-value hunk 'ch))
         #+nil (w (slot-value hunk 'cw))
         (widget (qt-hunk-widget hunk))
         (offset (truncate (offset-on-each-side widget)))
         (chrs (dis-line-chars dl))
         (y (* position h)))
    (unless (zerop (dis-line-flags dl))
      (setf (hi::dis-line-tick dl) (incf *tick*)))
    (clear-line-items scene hunk position)
    (with-objects ((pen (#_new QPen (#_Qt::NoPen)))
                   (color (#_new QColor 255 255 255 210))
                   (oops (#_new QBrush color))
                   (brush (#_new QBrush oops))
                   (rect (#_new QRectF
                                (- (+ offset *gutter*))
                                y
                                (#_width widget)
                                h)))
      (let ((item
             (#_new QGraphicsRectItem
                    rect
                    (ensure-hunk-item-group scene hunk))))
        (qt::cancel-finalization item)
        (#_setPen item pen)
        (#_setBrush item brush)
        (#_setZValue item 0)
        (push item (line-items hunk position))))
    (let ((len (dis-line-length dl)))
      (push (add-chunk-item scene
                            hunk
                            chrs
                            0
                            (+ 1 y)
                            0
                            len
                            0
                            *modeline-font*)
            (line-items hunk position))))
  (setf (dis-line-flags dl) unaltered-bits (dis-line-delta dl) 0))

(defun update-line-items (scene hunk dl)
  (let* ((position (dis-line-position dl))
         (h (slot-value hunk 'ch))
         (w (slot-value hunk 'cw))
         ;; (widget (qt-hunk-widget hunk))
         ;; (offset (truncate (offset-on-each-side widget)))
         (chrs (dis-line-chars dl))
         (y (* position h)))
    (unless (zerop (dis-line-flags dl))
      (setf (hi::dis-line-tick dl) (incf *tick*)))
    (clear-line-items scene hunk position)
    ;; 
    (handler-case
        (let* ((no (hi::tag-line-number (hi::dis-line-tag dl)))
               (str (princ-to-string no)))
          (push (add-chunk-item scene hunk str
                                (- (+ (* w (length str)) (* 2 *gutter*)))
                                (+ 1 y)
                                0
                                (length str)
                                (if (zerop (mod no 5))
                                    16
                                    15))
                (line-items hunk position)))
      (error (c) (warn "~A" c)))
    ;; font changes
    (let ((start 0)
          (font 0)
          (end (dis-line-length dl))
          (changes (dis-line-font-changes dl)))
      (iter
       (cond ((null changes)
              (push (add-chunk-item scene hunk chrs
                                    (* w start)
                                    (+ 1 y)
                                    start
                                    end
                                    font)
                    (line-items hunk position))
              (return))
             (t
              (push (add-chunk-item scene hunk chrs
                                    (* w start)
                                    (+ 1 y)
                                    start
                                    (font-change-x changes)
                                    font)
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

(defun reset-hunk-background (window hunk)
  (declare (ignore window))
  (let* ((widget (qt-hunk-widget hunk))
         (scene (#_scene widget)))
    (dolist (item (hunk-background-items hunk))
      (#_delete item))
    (setf (hunk-background-items hunk)
          (when (qt-hunk-want-background-p hunk)
            (let ((offset (offset-on-each-side widget)))
              (with-objects
                  ((pen1 (#_new QPen (#_Qt::SolidLine)))
                   (pen2 (#_new QPen (#_Qt::NoPen)))
                   (color1 (#_new QColor 255 255 255 210))
                   (color2 (#_new QColor 255 255 255 128))
                   (oops1 (#_new QBrush color1))
                   (oops2 (#_new QBrush color2))
                   (brush1 (#_new QBrush oops1))
                   (brush2 (#_new QBrush oops2))
                   (rect1 (#_new QRectF
                                 (- (+ (* 2 *gutter*) (truncate offset 2)))
                                 (- *gutter*)
                                 (+ (- (#_width widget) offset)
                                    (* 2 *gutter*))
                                 (+ (* (slot-value hunk 'ch)
                                       (device-hunk-height hunk))
                                    (* 2 *gutter*))))
                   (rect2 (#_new QRectF rect1)))
                (#_setColor pen1 (#_new QColor (#_Qt::black)))
                (#_adjust rect2 -5 -5 5 5)
                (let* ((group (ensure-hunk-item-group scene hunk))
                       (item1 (#_new QGraphicsRectItem rect1 group))
                       (item2 (#_new QGraphicsRectItem rect2 group)))
                  (qt::cancel-finalization item1)
                  (qt::cancel-finalization item2)
                  (#_setPen item1 pen1)
                  (#_setPen item2 pen2)
                  (#_setBrush item1 brush1)
                  (#_setBrush item2 brush2)
                  (#_setZValue item1 1)
                  (#_setZValue item2 1)
                  (#_setZValue group 3)
                  (list item1 item2))))))))

;; Smart isn't very smart, but still much better for "a single line changed"
;; kind of situations.
(defun dumb-or-smart-redisplay (device window dumb)
  (declare (ignore device))
  (let* ((widget (qt-hunk-widget (window-hunk window)))
         (hunk (window-hunk window))
         (first (window-first-line window))
         (offset (truncate (offset-on-each-side widget)))
         (scene (#_scene widget)))

    (when dumb
      (reset-hunk-background window hunk)
      (clear-all-line-items scene hunk))

    (let* ((native-widget (hi::buffer-widget (window-buffer window)))
           (current-item (hunk-native-widget-item hunk))
           (current-widget (and current-item
                                (#_widget current-item))))
      (unless (eq native-widget current-widget)
        (let ((group (ensure-hunk-item-group scene hunk)))
          (when current-item
            (setf (hunk-native-widget-item hunk) nil)
            (#_setWidget current-item (qt::null-qobject "QWidget"))
            (#_delete current-item))
          (when native-widget
            (let ((item (#_new QGraphicsProxyWidget)))
              (#_setParent native-widget (qt::null-qobject "QWidget"))
              (#_setWidget item native-widget)
              (#_setParentItem item group)
              (#_setPos item (- offset) 0)
              (#_setZValue item 4)
;;;               (#_setAcceptHoverEvents item t)
;;;               (#_setEnabled native-widget t)
              (#_setFocusPolicy native-widget (#_Qt::StrongFocus))
;;;               (#_setFocusProxy widget native-widget)
              (#_setGeometry native-widget
                             (- (+ offset))
                             0
                             (- (#_width widget)
                                (* 2 *gutter*))
                             (* (slot-value hunk 'ch)
                                (device-hunk-text-height hunk)))
              #+nil (#_setTransform item
                                    (#_scale (#_rotate (#_new QTransform) 10)
                                             0.75 0.75)
                                    nil)
              (setf (hunk-native-widget-item hunk) item))))))

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
      (let ((dis-line (car dl)))
        ;; fixme.  See comment in COMPUTE-LINE-IMAGE:
        (hi::sync-dis-line-tag (hi::dis-line-line dis-line) dis-line)

        (when (or dumb (plusp (dis-line-flags dis-line)))
          (update-line-items scene hunk dis-line))))

    ;; modeline
    (when (window-modeline-buffer window)
      (update-modeline-fields (window-buffer window) window)
      (let ((dl (window-modeline-dis-line window)))
        (update-modeline-items scene hunk dl)
        (setf (dis-line-flags dl) unaltered-bits)) 
      (setf (dis-line-flags (window-modeline-dis-line window))
            unaltered-bits)))

  ;; tell the redisplay algorithm that we did our job, otherwise it
  ;; retries forever:
  (let* ((first (window-first-line window))
         ;; (hunk (window-hunk window))
         #+nil (device (device-hunk-device hunk)))
    (setf (window-first-changed window) the-sentinel
          (window-last-changed window) first)))

(defun ensure-hunk-item-group (scene hunk)
  (or (hunk-item-group hunk)
      (setf (hunk-item-group hunk)
            (let ((g (#_new QGraphicsItemGroup)))
              (#_addItem scene g)
              (qt::cancel-finalization g)
              g))))

(defun add-chunk-item
    (scene hunk string x y start end font-color &optional (font *font*))
  (let* ((item
          (#_new QGraphicsSimpleTextItem (ensure-hunk-item-group scene hunk)))
         (widget (qt-hunk-widget hunk))
         (offset (truncate (offset-on-each-side widget))))
    (qt::cancel-finalization item)
    ;; (#_setPos (hunk-item-group hunk) 50 50)
    (with-objects
        ((t2 (#_translate (#_new QTransform)
                          (+ offset *gutter*)
                          (+ *gutter*
                             (progn ;; with-object (metrics (#_new QFontMetrics *font*))
                               (* (slot-value hunk 'ch)
                                  (- (device-hunk-position hunk)
                                     (device-hunk-height hunk))))))))
      (#_setTransform (hunk-item-group hunk) t2 nil))
    (#_setText item (subseq string start end))
    (#_setFont item font)
    (let ((color
           (elt #(                      ;fg       bg
                  #x000000 #xffffff
                  #x999999 #xffffff     ;1 = comments
                  #xffd700 #xffffff     ;2 = backquote
                  #x000000 #xffffff     ;3 = unquote

                  #x008b8b #xffffff     ;4 = strings
                  #x0000ff #xffffff     ;5 = quote
                  #x00aa00 #xffffff     ;6 = #+
                  #xff0000 #xffffff     ;7 = #-

                  #x000000 #xbebebe
                  #xff0000 #xbebebe
                  #x00aa00 #xbebebe
                  #xffff00 #xbebebe

                  #x0000ff #xbebebe
                  #xff00ff #xbebebe
                  #x00ffff #xbebebe
                  #xbebebe #x000000
                  #xffffff #x000000)
                (* 2 (mod font-color 17)))))
      (with-objects ((color (#_new QColor
                                   (ldb (byte 8 16) color)
                                   (ldb (byte 8 8) color)
                                   (ldb (byte 8 0) color)))
                     (brush (#_new QBrush color)))
        (#_setBrush item brush)))
    (#_setPos item x y)
    (#_setZValue item 2)
    item))

(defun make-virtual-buffer (name widget &rest args)
  (let ((buffer (apply #'make-buffer name args)))
    (when buffer
      (setf (buffer-writable buffer) nil)
      (setf (hi::buffer-widget buffer) widget)
      ;; (#_addWidget *main-stack* widget)
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
  (let ((main *main-hunk-widget*))
    (unless (#_hasFocus main)
      (setf *steal-focus-out* t)
      (#_setFocus main)
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
  (#_setFocus *main-hunk-widget*))

#+nil
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

(defcommand "Toggle Tab Bar" (p)
    "" ""
  (#_setVisible *tabs* (not (#_isVisible *tabs*))))

(defun main-window ()
  (#_window (qt-hunk-widget (window-hunk (current-window)))))

(defcommand "Toggle Menu Bar" (p)
    "" ""
  (let ((menubar (#_menuBar (main-window))))
    (#_setVisible menubar (not (#_isVisible menubar)))))

(defcommand "Toggle Full Screen" (p)
  "" ""
  (let ((win (main-window)))
    (if (logtest (#_windowState win)
                 (primitive-value (#_Qt::WindowFullScreen)))
        (#_showNormal win)
        (#_showFullScreen win))))

#+nil
(defcommand "Def" (p)
  "" ""
  (let* ((widget (#_new QWebView))
         (w (#_addWidget
             (#_scene (qt-hunk-widget (window-hunk (current-window))))
             widget)))
    (push widget *do-not-gc-list*)
    (push w *do-not-gc-list*)
    (#_setUrl widget (#_new QUrl "http://www.google.com"))
    #+nil (#_setZValue w 500)
    #+nil (let* ((new-class (qt::find-qclass "QGraphicsItem"))
                 (new-ptr (qt::%cast w new-class)))
            (make-instance 'qt::qobject
                           :class new-class
                           :pointer new-ptr))
    (#_setTransform w
                    #+nil (#_translate (#_new QTransform)  1 2)
                    (#_translate (if p
                                     (#_rotate (#_scale (#_new QTransform) 0.75 0.75) 45)
                                     (#_new QTransform))
                                 400 0)
                    nil)))
