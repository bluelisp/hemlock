;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock.x11)

(defclass bitmap-device (device)
  ((display :initarg :display
            :initform nil
            :accessor bitmap-device-display
            :documentation "CLX display object.")))

(defun make-bitmap-device (&rest initargs)
  (apply #'make-instance 'bitmap-device initargs))

;;; Bitmap hunks.
;;;
;;; The lock field is no longer used.  If events could be handled while we
;;; were in the middle of something with the hunk, then this could be set
;;; for exclusion purposes.
;;;
(defclass bitmap-hunk (device-hunk)
  ((width
    :initarg :width
    :initform nil
    :accessor bitmap-hunk-width
    :documentation "Pixel width.")
   (char-height
    :initarg :char-height
    :initform nil
    :accessor bitmap-hunk-char-height
    :documentation "Height of text body in characters.")
   (char-width
    :initarg :char-width
    :initform nil
    :accessor bitmap-hunk-char-width
    :documentation "Width in characters.")
   (start
    :initarg :start
    :initform nil
    :accessor bitmap-hunk-start
    :documentation "Head of dis-line list (no dummy).")
   (end
    :initarg :end
    :initform nil
    :accessor bitmap-hunk-end
    :documentation "Exclusive end, i.e. nil if nil-terminated.")
   (modeline-dis-line
    :initarg :modeline-dis-line
    :initform nil
    :accessor bitmap-hunk-modeline-dis-line
    :documentation "Dis-line for modeline, or NIL if none.")
   (modeline-pos
    :initarg :modeline-pos
    :initform nil
    :accessor bitmap-hunk-modeline-pos
    :documentation "Position of modeline in pixels.")
   (lock
    :initarg :lock
    :initform t
    :accessor bitmap-hunk-lock
    :documentation "Something going on, set trashed if we're changed.")
   (trashed
    :initarg :trashed
    :initform nil
    :accessor bitmap-hunk-trashed
    :documentation "Something bad happened, recompute image.")
   (font-family
    :initarg :font-family
    :initform nil
    :accessor bitmap-hunk-font-family
    :documentation "Font-family used in this window.")
   (input-handler
    :initarg :input-handler
    :initform nil
    :accessor bitmap-hunk-input-handler
    :documentation "Gets hunk, char, x, y when char read.")
   (changed-handler
    :initarg :changed-handler
    :initform nil
    :accessor bitmap-hunk-changed-handler
    :documentation "Gets hunk when size changed.")
   (thumb-bar-p
    :initarg :thumb-bar-p
    :initform nil
    :accessor bitmap-hunk-thumb-bar-p
    :documentation "True if we draw a thumb bar in the top border.")
   (window-group
    :initarg :window-group
    :initform nil
    :accessor bitmap-hunk-window-group
    :documentation "The window-group to which this hunk belongs.")))

(defclass x11-hunk (bitmap-hunk)
  ((xwindow
    :initarg :xwindow
    :initform nil
    :accessor bitmap-hunk-xwindow
    :documentation "X window for this hunk.")
   (gcontext
    :initarg :gcontext
    :initform nil
    :accessor bitmap-hunk-gcontext
    :documentation "X gcontext for xwindow.")))

(defun bitmap-hunk-height (hunk)
  (hi::device-hunk-height hunk))

(defun (setf bitmap-hunk-height) (value hunk)
  (setf (hi::device-hunk-height hunk) value))

(defun bitmap-hunk-window (hunk)
  (hi::device-hunk-window hunk))

(defun (setf bitmap-hunk-window) (value hunk)
  (setf (hi::device-hunk-window hunk) value))

(defun bitmap-hunk-previous (hunk)
  (hi::device-hunk-previous hunk))

(defun (setf bitmap-hunk-previous) (value hunk)
  (setf (hi::device-hunk-previous hunk) value))

(defun bitmap-hunk-next (hunk)
  (hi::device-hunk-next hunk))

(defun (setf bitmap-hunk-next) (value hunk)
  (setf (hi::device-hunk-next hunk) value))

(defun bitmap-hunk-device (hunk)
  (hi::device-hunk-device hunk))

(defun (setf bitmap-hunk-device) (value hunk)
  (setf (hi::device-hunk-device hunk) value))

(defun bitmap-hunk-position (hunk)
  (hi::device-hunk-position hunk))

(defun (setf bitmap-hunk-position) (value hunk)
  (setf (hi::device-hunk-position hunk) value))

(defun make-bitmap-hunk (&rest initargs)
  (apply #'make-instance 'x11-hunk initargs))

#||
;;;; What we want now is:

(defclass bitmap-hunk (device-hunk)
  ;; a hunk for a generic bitmap device
  )

(defclass x11-hunk (bitmap-hunk)
  ;; XLIB stufff
  )

(defclass clim-hunk (bitmap-hunk)
  ;; A CLIM hunk
  )

;; and having hunk-replace-line etc. (essentially hunk-draw.lisp)
;; being methods on these.

||#

(defgeneric hunk-put-string* (hunk x y font-family font string start end))
(defgeneric old-hunk-replace-line (hunk dl &optional position))
(defgeneric hunk-clear-lines (hunk start count))
(defgeneric hunk-copy-lines (hunk src dst count))
(defgeneric hunk-replace-modeline (hunk))
