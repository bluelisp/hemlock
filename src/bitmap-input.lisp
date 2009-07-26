;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock-internals)

;;;; Editor input from windowing system.

(defclass windowed-editor-input (editor-input)
  ((hunks :initarg :hunks
          :accessor windowed-editor-input-hunks
          :documentation "List of bitmap-hunks which input to this stream."))
  (:documentation
   "Editor input from windowing system."))

(defun make-windowed-editor-input (&optional (head (make-input-event)) (tail head))
  (make-instance 'windowed-editor-input
                 :head head :tail tail))

#+clx
;;; There's actually no difference from the TTY case...
;;; ### Hmm. --GB
(defmethod get-key-event ((stream windowed-editor-input) &optional ignore-abort-attempts-p)
  (editor-input-method-macro))

#+clx
(defmethod unget-key-event (key-event (stream windowed-editor-input))
  (un-event key-event stream))

#+clx
(defmethod clear-editor-input ((stream windowed-editor-input))
  (loop (unless (hemlock-ext:serve-event 0) (return)))
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
          (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
               *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

#+clx
(defmethod listen-editor-input ((stream windowed-editor-input))
  (loop
    ;; Don't service anymore events if we just got some input.
    (when (input-event-next (editor-input-head stream))
      (return t))
    ;;
    ;; If nothing is pending, check the queued input.
    (unless (hemlock-ext:serve-event 0)
      (return (not (null (input-event-next (editor-input-head stream))))))))

