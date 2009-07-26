;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Editor input from a tty.

(defclass tty-editor-input (editor-input)
  ((fd :initarg :fd
       :accessor tty-editor-input-fd)))

(defmethod get-key-event ((stream tty-editor-input) ignore-abort-attempts-p)
  (editor-input-method-macro))

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
  (loop
    ;; Don't service anymore events if we just got some input.
    (when (or (input-event-next (editor-input-head stream))
              (editor-tty-listen stream))
      (return t))
    ;; If nothing is pending, check the queued input.
    (unless (hemlock-ext:serve-event 0)
      (return (not (null (input-event-next (editor-input-head stream))))))))