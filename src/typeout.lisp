;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock-internals)

(defun make-unique-buffer (base-name &rest keys)
  (loop
     for i from 0
     for name = base-name then (format nil "~A<~D>" base-name i)
     for buf = (apply #'make-buffer name keys)
     when buf do (return buf)))

(defmode "Typeout" :major-p t
  :documentation
  "Typeout is a read-only buffer for pop up displays.")

(defcommand "Dismiss Typeout" (p)
  "Close the Typeout Buffer" ""
  (declare (ignore p))
  (hlet ((hemlock::ask-for-new-buffer nil))
    (hemlock::kill-buffer-command nil (buffer-name *current-buffer*))))

(bind-key "Dismiss Typeout" #k"q" :mode "Typeout")

(defun invoke-with-pop-up-display (cont buffer-name height)
  (declare (ignore height))
  (let ((buf (make-unique-buffer buffer-name :modes '("Typeout"))))
    (change-to-buffer buf)
    (with-output-to-mark (stream (buffer-point buf))
      (funcall cont stream))
    (setf (buffer-modified buf) nil)))

(declaim (special *random-typeout-ml-fields* *buffer-names*))

(defvar *random-typeout-buffers* () "A list of random-typeout buffers.")

(defun get-random-typeout-info (buffer-name line-buffered-p)
  (let* ((buffer (getstring buffer-name *buffer-names*))
         (stream
          (cond
           ((not buffer)
            (let* ((buf (make-buffer
                         buffer-name
                         :modes '("Fundamental")
                         :modeline-fields *random-typeout-ml-fields*
                         :delete-hook
                         (list #'(lambda (buffer)
                                   (setq *random-typeout-buffers*
                                         (delete buffer *random-typeout-buffers*
                                                 :key #'car))))))
                   (point (buffer-point buf))
                   (stream (make-random-typeout-stream
                            (copy-mark point :left-inserting))))
              (setf (random-typeout-stream-more-mark stream)
                    (copy-mark point :right-inserting))
              (push (cons buf stream) *random-typeout-buffers*)
              stream))
           ((member buffer *random-typeout-buffers* :key #'car)
            (delete-region (buffer-region buffer))
            (let* ((pair (assoc buffer *random-typeout-buffers*))
                   (stream (cdr pair)))
              (setf *random-typeout-buffers*
                    (cons pair (delete pair *random-typeout-buffers*)))
              (setf (random-typeout-stream-first-more-p stream) t)
              (setf (random-typeout-stream-no-prompt stream) nil)
              stream))
           (t
            (error "~A is not a random typeout buffer."
                   (buffer-name buffer))))))
    (setf (slot-value stream 'line-buffered-p)
          line-buffered-p)
    stream))
