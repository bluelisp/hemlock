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

(defvar *tty-translations* (make-hash-table :test #'equal))

(defun register-tty-translations ()
  (assert hemlock.terminfo:*terminfo*)
  (flet ((reg (string keysym &key kludge)
           (when kludge
             ;; FIXME: This is pretty terrible, but for some reason my *terminfo* has
             ;; Esc,O,<foo> for arraw keys, whereas terminal actually sends Esc,[,<foo>
             ;; -- either I don't understand how terminfo stuff is supposed to work,
             ;; Apple ships with a broken terminfo db, or if something is wrong with
             ;; the terminfo code. I'm inclined to blame me...
             (assert (eq #\O (char string 1)))
             (setf string (format nil "~A[~A" (char string 0) (subseq string 2))))
           (setf (gethash (string string) *tty-translations*) keysym)))
    (reg hemlock.terminfo:key-left #k"Leftarrow" :kludge t)
    (reg hemlock.terminfo:key-up #k"Uparrow" :kludge t)
    (reg hemlock.terminfo:key-down #k"Downarrow" :kludge t)
    (reg hemlock.terminfo:key-right #k"Rightarrow" :kludge t)
    (reg hemlock.terminfo:key-ppage #k"Pageup")
    (reg hemlock.terminfo:key-npage #k"Pagedown")
    (reg #\newline #k"Return")
    (reg #\tab #k"Tab")
    (reg #\backspace #k"Backspace")
    (reg #\rubout #k"Delete")
    (reg #\escape #k"Escape")))

(defun translate-tty-event (data)
  (let* ((string (coerce data 'string))
         (sym (gethash string *tty-translations*)))
    (if sym
        (hemlock-ext:make-key-event sym 0)
        (when (= 1 (length string))
          (hemlock-ext:char-key-event (char string 0))))))

(defun tty-key-event (data)
  (loop with start = 0
        with length = (length data)
        while (< start length)
        do (loop for end from length downto (1+ start)
                 do (let ((event (translate-tty-event (subseq data start end))))
                      (when event
                        (q-event *real-editor-input* event)
                        (setf start end)
                        (return))))))

