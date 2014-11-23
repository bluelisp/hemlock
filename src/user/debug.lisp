;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Slave debugging

(in-package :hemlock)


(defvar *slave-stack-frames* nil)
(defvar *slave-stack-frames-end* nil)
;;;

(defstruct (slave-stack-frame
             (:constructor make-slave-stack-frame (label remote-frame)))
  label
  remote-frame)


;;; This is the debug buffer if it exists.
;;;
(defvar *debug-buffer* nil)

;;; This is the cleanup method for deleting *debug-buffer*.
;;;
(defun delete-debug-buffers (buffer)
  (when (eq buffer *debug-buffer*)
    (setf *debug-buffer* nil)
    (setf *slave-stack-frames* nil)))


;;;; Commands.

(defmode "Debug" :major-p t
  :documentation "Debug mode presents a list of slave symbols.")

(defcommand "Debug Quit" (p)
  "Kill the debug buffer."
  ""
  (declare (ignore p))
  (when *debug-buffer* (delete-buffer-if-possible *debug-buffer*)))

(defun slave-stack-frame-from-mark (mark)
  )

(defun refresh-debug (buf entries)
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (setf *slave-stack-frames-end* (length entries))
    (setf *slave-stack-frames* (coerce entries 'vector))
    (with-output-to-mark (s (buffer-point buf))
      (iter:iter (iter:for entry in  entries)
                 (iter:for i from 0)
                 (debug-write-line i entry s)))))

(defvar *debug-context* nil)

(defun make-debug-buffer (context entries impl thread)
  (let ((buf (or *debug-buffer*
                 (make-buffer (format nil "Slave Debugger ~A" impl thread)
                              :modes '("Debug")))))
    (setf *debug-buffer* buf)
    (setf *debug-context* context)
    (refresh-debug buf
                   (mapcar (lambda (entry)
                             (make-slave-stack-frame (car entry)
                                                     (cdr entry)))
                           entries))
    (let ((fields (buffer-modeline-fields *debug-buffer*)))
      (setf (cdr (last fields))
            (list (or (modeline-field :debug-cmds)
                      (make-modeline-field
                       :name :debug-cmds :width 18
                       :function
                       #'(lambda (buffer window)
                           (declare (ignore buffer window))
                           "  Type ? for help.")))))
      (setf (buffer-modeline-fields *debug-buffer*) fields))
    (buffer-start (buffer-point buf))
    (change-to-buffer buf)))

(defun debug-write-line (i entry s)
  (format s "~D: ~A~%" i (slave-stack-frame-label entry)))

(defun debug-using-master (&optional (start 0) (end 10))
  (if prepl:*debugging-context*
      (let ((frames
             (mapcar (lambda (frame)
                       (cons (with-output-to-string (s)
                               (conium:print-frame frame s))
                             (hemlock.wire:make-remote-object frame)))
                     (conium:compute-backtrace start end)))
            (context nil
                     #+nil (hemlock.wire:make-remote-object
                            prepl:*debugging-context*))
            ;; fixme: show the slave name rather than just the impl type
            (impl (lisp-implementation-type))
            (thread (bordeaux-threads:thread-name
                     (bordeaux-threads:current-thread))))
        (hemlock::eval-in-master
         `(make-debug-buffer ',context ',frames ',impl ',thread)))
      (prepl:debugger nil nil (lambda () (debug-using-master start end)))))
