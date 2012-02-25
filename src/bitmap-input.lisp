;;;; -*- Mode: Lisp; indent-with-tabs: nil -*-

(in-package :hemlock-internals)

(pushnew :clx *available-backends*)

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

;;; There's actually no difference from the TTY case...
;;; ### Hmm. --GB
(defmethod get-key-event ((stream windowed-editor-input) &optional ignore-abort-attempts-p)
  (%editor-input-method stream ignore-abort-attempts-p))

(defmethod unget-key-event (key-event (stream windowed-editor-input))
  (un-event key-event stream))

(defmethod clear-editor-input ((stream windowed-editor-input))
  (dispatch-events-no-hang)
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
          (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
               *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

(defmethod listen-editor-input ((stream windowed-editor-input))
;;;   (loop
;;;     ;; Don't service anymore events if we just got some input.
;;;     (when (input-event-next (editor-input-head stream))
;;;       (return t))
;;;     ;;
;;;     ;; If nothing is pending, check the queued input.
;;;     (unless (hemlock-ext:serve-event 0)
;;;       (return (not (null (input-event-next (editor-input-head stream)))))))
  (dispatch-events-no-hang)
  nil)

(defun cleanup-for-wm-closed-display (closed-display)
  ;; Remove fd-handlers
  (hemlock-ext:disable-clx-event-handling closed-display)
  ;; Close file descriptor and note DEAD.
  (xlib:close-display closed-display)
  ;;
  ;; At this point there is not much sense to returning to Lisp
  ;; as the editor cannot be re-entered (there are lots of pointers
  ;; to the dead display around that will cause subsequent failures).
  ;; Maybe could switch to tty mode then (save-all-files-and-exit)?
  ;; For now, just assume user wanted an easy way to kill the session.
  (hemlock-ext:quit))

(defmethod %editor-input-method
    :around
    ((editor-input windowed-editor-input) abortp)
  (declare (ignore abortp))
  (handler-bind
      ((error
        (lambda (condition)
          (when (typep condition 'stream-error)
            (let* ((stream (stream-error-stream condition))
                   (display *editor-windowed-input*)
                   (display-stream
                    (and display (xlib::display-input-stream display))))
              (when (eq stream display-stream)
                (format *error-output* "~%Hemlock: Display died!~%~%")
                (cleanup-for-wm-closed-display display)
                (exit-hemlock nil))
              (let ((device
                     (device-hunk-device (window-hunk (current-window)))))
                (device-exit device))
              (invoke-debugger condition)))))
       (xlib:closed-display
        (lambda(condition)
          (let ((display (xlib::closed-display-display condition)))
            (format *error-output*
                    "Closed display on stream ~a~%"
                    (xlib::display-input-stream display)))
          (exit-hemlock nil))))
     (call-next-method)))

(defmethod backend-init-raw-io ((backend (eql :clx)) display)
  (setf *editor-windowed-input*
        #+(or CMU scl) (ext:open-clx-display display)
        #+(or sbcl openmcl)  (xlib::open-default-display display)
        #-(or sbcl CMU scl openmcl) (xlib:open-display "localhost"))
  (setf *editor-input* (make-windowed-editor-input))
  (setf *real-editor-input* *editor-input*)
  (setup-font-family *editor-windowed-input*)
  *editor-windowed-input*)

(defmethod %init-screen-manager ((backend-type (eql :clx)) (display t))
  (init-bitmap-screen-manager display))
