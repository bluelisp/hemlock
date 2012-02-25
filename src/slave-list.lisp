;;;; -*- Mode: Lisp; indent-with-tabs: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains slave-list-related code.
;;;

(in-package :hemlock)



;;;; Representation of existing slaves.

(defvar *slave-list-items* nil)
(defvar *slave-list-items-end* nil)
;;;

(defstruct slave-list-item
  (marked nil)
  name
  info)


;;; This is the slave-list buffer if it exists.
;;;
(defvar *slave-list-buffer* nil)

;;; This is the cleanup method for deleting *slave-list-buffer*.
;;;
(defun delete-slave-list-buffers (buffer)
  (when (eq buffer *slave-list-buffer*)
    (setf *slave-list-buffer* nil)
    (setf *slave-list-items* nil)))


;;;; Commands.

(defmode "Slave-List" :major-p t
  :documentation
  "List of slaves")

(defcommand "Mark Slave" (p)
  "" ""
  (declare (ignore p))
  (let* ((point (current-point))
         (item-at-point (array-element-from-mark point *slave-list-items*)))
    (with-writable-buffer (*slave-list-buffer*)
      (setf (slave-list-item-marked item-at-point) t)
      (with-mark ((point point))
        (setf (next-character (line-start point)) #\*))
      (line-offset point 1))))

(defcommand "Unmark Slave" (p)
  "" ""
  (declare (ignore p))
  (with-writable-buffer (*slave-list-buffer*)
    (setf (slave-list-item-marked
           (array-element-from-mark (current-point) *slave-list-items*))
          nil)
    (with-mark ((point (current-point)))
      (setf (next-character (line-start point)) #\space))
    (line-offset (current-point) 1)))

(defcommand "Quit Slave List" (p)
  "" ""
  (declare (ignore p))
  (when *slave-list-buffer* (delete-buffer-if-possible *slave-list-buffer*)))

(defcommand "Goto Slave" (p)
  "" ""
  (let ((info (slave-list-item-info
               (array-element-from-mark (current-point) *slave-list-items*))))
    (change-to-buffer
     (or (server-info-slave-buffer info)
         (editor-error "Slave has no buffer")))
    (unless (or p (not (prompt-for-y-or-n :prompt "Set as current slave? "
                                          :default t
                                          :must-exist t
                                          :default-string "Y")))
      (setf (variable-value 'current-eval-server :global) info))))

(defcommand "Activate Slave" (p)
  "" ""
  (declare (ignore p))
  (setf (variable-value 'current-eval-server :global)
        (slave-list-item-info
         (array-element-from-mark (current-point) *slave-list-items*)))
  (refresh-slave-list *slave-list-buffer*))

(defun list-slave-items ()
  (hi::map-string-table 'list
                        (lambda (info)
                          (make-slave-list-item
                           :name (server-info-name info)
                           :marked nil
                           :info info))
                        *server-names*))

(defun refresh-slave-list (buf)
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (let ((items (coerce (list-slave-items) 'vector)))
      (setf *slave-list-items-end* (length items))
      (setf *slave-list-items* items)
      (with-output-to-mark (s (buffer-point buf))
        (iter:iter (iter:for c in-vector items)
                   (slave-list-write-line c s)))
      (buffer-start (current-point)))))

(defcommand "List Slaves" (p)
  "" ""
  (declare (ignore p))
  (let ((buf (or *slave-list-buffer*
                 (make-buffer "Slave-List" :modes '("Slave-List")
                              :delete-hook (list #'delete-slave-list-buffers)))))
    (unless *slave-list-buffer*
      (setf *slave-list-buffer* buf)
      (refresh-slave-list buf)
      (let ((fields (buffer-modeline-fields *slave-list-buffer*)))
        (setf (cdr (last fields))
              (list (or (modeline-field :slave-list-cmds)
                        (make-modeline-field
                         :name :slave-list-cmds :width 18
                         :function
                         #'(lambda (buffer window)
                             (declare (ignore buffer window))
                             "  Type ? for help.")))))
        (setf (buffer-modeline-fields *slave-list-buffer*) fields))
      (buffer-start (buffer-point buf)))
    (change-to-buffer buf)))

(defcommand "Refresh Slave List" (p)
  "" ""
  (declare (ignore p))
  (when *slave-list-buffer*
    (refresh-slave-list *slave-list-buffer*)))

(defun slave-list-write-line (item s)
  (let ((info (slave-list-item-info item)))
    (format s " ~:[         ~;(current)~] ~A~30T~A ~A~%"
            (eq info (value current-eval-server))
            (slave-list-item-name item)
            (server-info-implementation-type info)
            (server-info-implementation-version info))))

(defcommand "Slave-List Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Slave-List"))

(bind-key "Mark Slave" #k"m" :mode "Slave-List")
(bind-key "Unmark Slave" #k"u" :mode "Slave-List")
(bind-key "Quit Slave List" #k"q" :mode "Slave-List")
(bind-key "Goto Slave" #k"space" :mode "Slave-List")
(bind-key "Activate Slave" #k"return" :mode "Slave-List")
(bind-key "Refresh Slave List" #k"g" :mode "Slave-List")
(bind-key "Next Line" #k"n" :mode "Slave-List")
(bind-key "Previous Line" #k"p" :mode "Slave-List")
(bind-key "Slave-List Help" #k"?" :mode "Slave-List")

