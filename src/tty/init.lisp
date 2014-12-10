(in-package :hi)


;;;; Terminal init and exit methods.

(defmethod device-init ((device tty-device))
  (setup-input)
  (tty-write-cmd (tty-device-init-string device))
  (redisplay-all))

(defmethod device-exit ((device tty-device))
  (cursor-motion device 0 (1- (tty-device-lines device)))
  ;; Can't call the clear-to-eol method since we don't have a hunk to
  ;; call it on, and you can't count on the bottom hunk being the echo area.
  ;;
  (if (tty-device-clear-to-eol-string device)
      (tty-write-cmd (tty-device-clear-to-eol-string device))
      (dotimes (i (tty-device-columns device)
                  (cursor-motion device 0 (1- (tty-device-lines device))))
        (tty-write-char #\space)))
  (tty-write-cmd (tty-device-cm-end-string device))
  (device-force-output device)
  (reset-input))


(defmethod backend-init-raw-io ((backend (eql :tty)) display)
  (declare (ignore display))
  ;; The editor's file descriptor is Unix standard input (0).
  ;; We don't need to affect system:*file-input-handlers* here
  ;; because the init and exit methods for tty redisplay devices
  ;; take care of this.
  ;;
  (setf *editor-file-descriptor* 0)
  (setf *editor-input* (make-tty-editor-input :fd 0))
  (setf *real-editor-input* *editor-input*))


;;; WITHOUT-HEMLOCK -- Public.
;;;
;;; Code:lispinit.lisp uses this for a couple interrupt handlers, and
;;; eval-server.lisp.
;;;
#+CMU
(defmacro without-hemlock (&body body)
  "When in the editor and not in the debugger, call the exit method of Hemlock's
   device, so we can type.  Do the same thing on exit but call the init method."
  `(progn
     (when (and *in-the-editor* (not debug::*in-the-debugger*))
       (let ((device (device-hunk-device (window-hunk (current-window)))))
         (device-exit device)))
     ,@body
     (when (and *in-the-editor* (not debug::*in-the-debugger*))
       (let ((device (device-hunk-device (window-hunk (current-window)))))
         (device-init device)))))

#-CMU
(defmacro without-hemlock (&body body)
  "When in the editor and not in the debugger, call the exit method of Hemlock's
   device, so we can type.  Do the same thing on exit but call the init method."
  `(progn
    (when (and *in-the-editor* )
      (let ((device (device-hunk-device (window-hunk (current-window)))))
        (device-exit device)))
    ,@body
    (when (and *in-the-editor* )
      (let ((device (device-hunk-device (window-hunk (current-window)))))
        (device-init device)))))

#+(or)
(defun pause-hemlock ()
  "Pause hemlock and pop out to the Unix Shell."
  (without-hemlock
   (unix:unix-kill (unix:unix-getpid) :sigstop))
  t)
