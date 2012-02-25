;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contatins the stream operations for pop-up-displays.
;;;
;;; Written by Blaine Burks.
;;;

(in-package :hemlock.x11)



;;;; Line-buffered Stream Methods.

;; ###GB we want a more optimized interface

(defmethod stream-write-char ((stream random-typeout-stream) char)
  (with-slots (line-buffered-p) stream
    (cond (line-buffered-p
           (insert-character (random-typeout-stream-mark stream) char)
           (when (and (char= char #\newline)
                      (not (random-typeout-stream-no-prompt stream)))
             (device-random-typeout-line-more
              (device-hunk-device
               (window-hunk (random-typeout-stream-window stream)))
              stream 1)))
          (t
           (insert-character (random-typeout-stream-mark stream) char)))))

(defmethod stream-write-string ((stream random-typeout-stream) string &optional start end)
  (setf start (or start 0))
  (setf end (or end (length string)))
  (with-slots (line-buffered-p) stream
    (cond (line-buffered-p
           (insert-string (random-typeout-stream-mark stream) string start end)
           (unless (random-typeout-stream-no-prompt stream)
             (let ((count (count #\newline string)))
               (when count
                 (device-random-typeout-line-more
                  (device-hunk-device
                   (window-hunk (random-typeout-stream-window stream)))
                  stream count)))))
          (t
           (insert-string (random-typeout-stream-mark stream) string start end)))))

(defmethod stream-finish-output ((stream random-typeout-stream))
  (with-slots (line-buffered-p) stream
    (cond (line-buffered-p
           (random-typeout-redisplay (random-typeout-stream-window stream)))
          (t
           nil))))

(defmethod stream-force-output ((stream random-typeout-stream))
  (stream-finish-output stream))

(defmethod stream-line-column ((stream random-typeout-stream))
  (mark-charpos (random-typeout-stream-mark stream)))

;;; Tty line-buffered support.

;;; UPDATE-TTY-LINE-BUFFERED-STREAM is called when anything is written to
;;; a line-buffered-random-typeout-stream on the tty.  It just makes sure
;;; hemlock doesn't choke on extra-long strings.
;;;
(defun update-tty-line-buffered-stream (stream newline-count)
  (let ((window (random-typeout-stream-window stream)))
    (when (plusp newline-count) (random-typeout-redisplay window))
    (loop
      (when (no-text-past-bottom-p window) (return))
      (display-more-prompt stream)
      (scroll-window window (window-height window))
      (random-typeout-redisplay window))))


;;; DO-BITMAP-FULL-MORE and DO-TTY-FULL-MORE scroll through the fresh text in
;;; random typeout buffer.
;;;

;;; Tty full-buffered support.

(defun do-tty-full-more (stream)
  (let* ((window (random-typeout-stream-window stream))
         (buffer (window-buffer window)))
    (with-mark ((end-check (buffer-end-mark buffer)))
      (when (and (mark/= (buffer-start-mark buffer) end-check)
                 (empty-line-p end-check))
        (line-end (line-offset end-check -1)))
      (loop
        (when (displayed-p end-check window)
          (return))
        (display-more-prompt stream)
        (scroll-window window (window-height window))))))


;;; Proclaim this special so the compiler doesn't warn me.  I hate that.
;;;
(declaim (special *more-prompt-action*))

(defun display-more-prompt (stream)
  (unless (random-typeout-stream-no-prompt stream)
    (let ((window (random-typeout-stream-window stream))
          (*more-prompt-action* :more))
      (update-modeline-field (window-buffer window) window :more-prompt)
      (random-typeout-redisplay window)
      (wait-for-more stream)
      (let ((*more-prompt-action* :empty))
        (update-modeline-field (window-buffer window) window :more-prompt)))))
