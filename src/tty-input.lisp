;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock-internals)

;;;; Editor input from a tty.

(defclass tty-editor-input (editor-input)
  ((fd :initarg :fd
       :accessor tty-editor-input-fd)))

(defun make-tty-editor-input (&rest args)
  (apply #'make-instance 'tty-editor-input args))

(defmethod get-key-event
    ((stream tty-editor-input) &optional ignore-abort-attempts-p)
  (%editor-input-method stream ignore-abort-attempts-p))

(defmethod unget-key-event (key-event (stream tty-editor-input))
  (un-event key-event stream))

(defmethod clear-editor-input ((stream tty-editor-input))
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
          (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
               *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

;;; Note that we never return NIL as long as there are events to be served with
;;; SERVE-EVENT.  Thus non-keyboard input (i.e. process output)
;;; effectively causes LISTEN to block until either all the non-keyboard input
;;; has happened, or there is some real keyboard input.
;;;
(defmethod listen-editor-input ((stream tty-editor-input))
  (process-editor-tty-input)
  nil)

(defun register-tty-translations ()
  (assert hemlock.terminfo:*terminfo*)
  (register-tty-translation hemlock.terminfo:key-left #k"Leftarrow" :kludge t)
  (register-tty-translation hemlock.terminfo:key-up #k"Uparrow" :kludge t)
  (register-tty-translation hemlock.terminfo:key-down #k"Downarrow" :kludge t)
  (register-tty-translation hemlock.terminfo:key-right #k"Rightarrow" :kludge t)
  (register-tty-translation hemlock.terminfo:key-npage #k"control-v")
  (register-tty-translation hemlock.terminfo:key-ppage #k"meta-v")
  (register-tty-translation #\newline #k"Return")
  (register-tty-translation #\tab #k"Tab")
  (register-tty-translation #\backspace #k"Backspace")
  (register-tty-translation #\rubout #k"Delete")
  (register-tty-translation #\escape #k"Escape"))
