;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar group-interesting-xevents
    '(:structure-notify)))

(defvar group-interesting-xevents-mask
  (apply #'xlib:make-event-mask group-interesting-xevents))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar child-interesting-xevents
    '(:key-press :button-press :button-release :structure-notify :exposure
                 :enter-window :leave-window)))

(defvar child-interesting-xevents-mask
  (apply #'xlib:make-event-mask child-interesting-xevents))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar random-typeout-xevents
    '(:key-press :button-press :button-release :enter-window :leave-window
                 :exposure)))

(defvar random-typeout-xevents-mask
  (apply #'xlib:make-event-mask random-typeout-xevents))

(declaim (special hemlock::*open-paren-highlight-font*
                  hemlock::*active-region-highlight-font*))

(defparameter lisp-fonts-pathnames '("fonts/"))

;;; SETUP-FONT-FAMILY sets *default-font-family*, opening the three font names
;;; passed in.  The font family structure is filled in from the first argument.
;;; Actually, this ignores default-highlight-font and default-open-paren-font
;;; in lieu of "Active Region Highlighting Font" and "Open Paren Highlighting
;;; Font" when these are defined.
;;;
(defun setup-font-family (display)
  (let* ((font-family (make-font-family :map (make-array font-map-size
                                                         :initial-element 0)
                                        :cursor-x-offset 0
                                        :cursor-y-offset 0))
         (font-family-map (font-family-map font-family)))
    (declare (simple-vector font-family-map))
    (setf *default-font-family* font-family)
    (let ((font (xlib:open-font display (variable-value 'hemlock::default-font))))
      (unless font
        (error "Cannot open font -- ~S" (variable-value 'hemlock::default-font)))
      (fill font-family-map font)
      (let ((width (xlib:max-char-width font)))
        (setf (font-family-width font-family) width)
        (setf (font-family-cursor-width font-family) width))
      (let* ((baseline (xlib:font-ascent font))
             (height (+ baseline (xlib:font-descent font))))
        (setf (font-family-height font-family) height)
        (setf (font-family-cursor-height font-family) height)
        (setf (font-family-baseline font-family) baseline)))
    (setup-one-font display
                    (variable-value 'hemlock::open-paren-highlighting-font)
                    font-family-map
                    hemlock::*open-paren-highlight-font*)
    (setup-one-font display
                    (variable-value 'hemlock::active-region-highlighting-font)
                    font-family-map
                    hemlock::*active-region-highlight-font*)
    ;; GB
    (setup-one-font display
                    "-*-lucidatypewriter-medium-r-*-*-*-120-*-*-*-*-iso8859-1"
                    font-family-map
                    7)))

;;; SETUP-ONE-FONT tries to open font-name for display, storing the result in
;;; font-family-map at index.  XLIB:OPEN-FONT will return font stuff regardless
;;; if the request is valid or not, so we finish the output to get synch'ed
;;; with the server which will cause any errors to get signaled.  At this
;;; level, we want to deal with this error here returning nil if the font
;;; couldn't be opened.
;;;
(defun setup-one-font (display font-name font-family-map index)
  (handler-case (let ((font (xlib:open-font display (namestring font-name))))
                  (xlib:display-finish-output display)
                  (setf (svref font-family-map index) font))
    (xlib:name-error ()
     (warn "Cannot open font -- ~S" font-name)
     nil)))

;;; INIT-RAW-IO  --  Internal
;;;
;;;    This function should be called whenever the editor is entered in a new
;;; lisp.  It sets up process specific data structures.
;;;
#+nilamb-duplicate(defun init-raw-io (display)
  #-clx (declare (ignore display))
  (setf *editor-windowed-input* nil)
  (cond #+clx
        (display
         (setf *editor-windowed-input*
               #+(or CMU scl) (ext:open-clx-display display)
               #+(or sbcl openmcl)  (xlib::open-default-display #+nil display)
               #-(or sbcl CMU scl openmcl) (xlib:open-display "localhost"))
         (setf *editor-input* (make-windowed-editor-input))
         (setup-font-family *editor-windowed-input*))
        #+nilamb
        (t ;; The editor's file descriptor is Unix standard input (0).
           ;; We don't need to affect system:*file-input-handlers* here
           ;; because the init and exit methods for tty redisplay devices
           ;; take care of this.
           ;;
         (setf *editor-file-descriptor* 0)
         (setf *editor-input* (make-tty-editor-input 0))))
  (setf *real-editor-input* *editor-input*)
  *editor-windowed-input*)


(defhvar "Raise Echo Area When Modified"
  "When set, Hemlock raises the echo area window when output appears there."
  :value nil)

;;; RAISE-ECHO-AREA-WHEN-MODIFIED -- Internal.
;;;
;;; INIT-BITMAP-SCREEN-MANAGER in bit-screen.lisp adds this hook when
;;; initializing the bitmap screen manager.
;;;
(defun raise-echo-area-when-modified (buffer modified)
  (when (and (value hemlock::raise-echo-area-when-modified)
             (eq buffer *echo-area-buffer*)
             modified)
    (let* ((hunk (window-hunk *echo-area-window*))
           (win (window-group-xparent (bitmap-hunk-window-group hunk))))
      (xlib:map-window win)
      (setf (xlib:window-priority win) :above)
      (xlib:display-force-output
       (bitmap-device-display (device-hunk-device hunk))))))
