;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains Coned (Connection Editing) code.
;;;

(in-package :hemlock)



;;;; Representation of existing connections.

;;; This is the array of connections in the coned connection.  Each element is a cons,
;;; where the CAR is the connection, and the CDR indicates whether the connection
;;; should be deleted (t deleted, nil don't).
;;;
(defvar *coned-connections* nil)
(defvar *coned-connections-end* nil)
;;;

(defstruct (coned-connection
             (:constructor make-coned-connection (connection)))
  connection
  (deleted nil))

(defun coned-connection (x)
  (coned-connection-connection x))


;;; This is the coned buffer if it exists.
;;;
(defvar *coned-buffer* nil)

;;; This is the cleanup method for deleting *coned-buffer*.
;;;
(defun delete-coned-buffers (buffer)
  (when (eq buffer *coned-buffer*)
    (setf *coned-buffer* nil)
    (setf *coned-connections* nil)))


;;;; Commands.

(defmode "Coned" :major-p t
  :documentation
  "Coned allows the user view and delete connections.")

(defhvar "Virtual Connection Deletion"
  "When set, \"Coned Delete\" marks a buffer for deletion instead of immediately
   deleting it."
  :value t)

(defhvar "Coned Delete Confirm"
  "When set, \"Coned\" commands that actually delete connections ask for
   confirmation before taking action."
  :value t)

(defcommand "Coned Delete" (p)
  "Delete the connection."
  "Delete the connection."
  (declare (ignore p))
  (let* ((point (current-point))
         (buf-info (array-element-from-mark point *coned-connections*)))
    (if (and (not (value virtual-coned-deletion))
             (or (not (value coned-delete-confirm))
                 (prompt-for-y-or-n :prompt "Delete connection? " :default t
                                    :must-exist t :default-string "Y")))
        (delete-coned-connection (coned-connection buf-info))
        (with-writable-buffer (*coned-buffer*)
          (setf (coned-connection-deleted buf-info) t)
          (with-mark ((point point))
            (setf (next-character (line-start point)) #\D))))))

(defcommand "Coned Undelete" (p)
  "Undelete the connection."
  "Undelete the connection."
  (declare (ignore p))
  (with-writable-buffer (*coned-buffer*)
    (setf (coned-connection-deleted
           (array-element-from-mark (current-point) *coned-connections*))
          nil)
    (with-mark ((point (current-point)))
      (setf (next-character (line-start point)) #\space))))

(defcommand "Coned Expunge" (p)
  "Expunge connections marked for deletion."
  "Expunge connections marked for deletion."
  (declare (ignore p))
  (expunge-coned-connections))

(defcommand "Coned Quit" (p)
  "Kill the coned buffer, expunging any buffer marked for deletion."
  "Kill the coned buffer, expunging any buffer marked for deletion."
  (declare (ignore p))
  (expunge-coned-connections)
  (when *coned-buffer* (delete-buffer-if-possible *coned-buffer*)))

;;; EXPUNGE-CONED-CONNECTIONS deletes the marked connections in the coned buffer,
;;; signalling an error if the current buffer is not the coned buffer.  This
;;; returns t if it deletes some buffer, otherwise nil.  We build a list of
;;; connections before deleting any because the CONED-DELETE-HOOK moves elements
;;; around in *coned-connections*.
;;;
(defun expunge-coned-connections ()
  (unless (eq *coned-buffer* (current-buffer))
    (editor-error "Not in the Coned buffer."))
  (let (connections)
    (dotimes (i *coned-connections-end*)
      (let ((buf-info (svref *coned-connections* i)))
        (when (coned-connection-deleted buf-info)
          (push (coned-connection buf-info) connections))))
    (if (and connections
             (or (not (value coned-delete-confirm))
                 (prompt-for-y-or-n :prompt "Delete connections? " :default t
                                    :must-exist t :default-string "Y")))
        (dolist (b connections t) (delete-coned-connection b))))
  (refresh-coned *coned-buffer*))

(defun delete-coned-connection (conn)
  (delete-connection conn))


(defcommand "Coned Goto" (p)
  "Change to the connection's buffer."
  "Change to the connection's buffer."
  (declare (ignore p))
  (change-to-buffer
   (or (connection-buffer
        (coned-connection
         (array-element-from-mark (current-point) *coned-connections*)))
       (editor-error "connection has no buffer"))))

(defun refresh-coned (buf)
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (let ((connections (list-all-connections)))
      (setf *coned-connections-end* (length connections))
      (setf *coned-connections*
            (map 'vector #'make-coned-connection connections))
      (with-output-to-mark (s (buffer-point buf))
        (dolist (c connections)
          (coned-write-line c s))))))

(defcommand "Coned" (p)
  "Creates a list of connections in a buffer supporting operations such as deletion
   and selection.  If there already is a coned buffer, just go to it."
  "Creates a list of connections in a buffer supporting operations such as deletion
   and selection.  If there already is a coned buffer, just go to it."
  (declare (ignore p))
  (let ((buf (or *coned-buffer*
                 (make-buffer "Coned" :modes '("Coned")
                              :delete-hook (list #'delete-coned-buffers)))))
    (unless *coned-buffer*
      (setf *coned-buffer* buf)
      (refresh-coned buf)
      (let ((fields (buffer-modeline-fields *coned-buffer*)))
        (setf (cdr (last fields))
              (list (or (modeline-field :coned-cmds)
                        (make-modeline-field
                         :name :coned-cmds :width 18
                         :function
                         #'(lambda (buffer window)
                             (declare (ignore buffer window))
                             "  Type ? for help.")))))
        (setf (buffer-modeline-fields *coned-buffer*) fields))
      (buffer-start (buffer-point buf)))
    (change-to-buffer buf)))

(defcommand "Coned Refresh" (p)
  "" ""
  (declare (ignore p))
  (when *coned-buffer*
    (refresh-coned *coned-buffer*)))

(defun coned-write-line (connection s)
  (format s "  ~A ~20T~A ~20T~A~%"
          (connection-name connection)
          (type-of connection)
          (let ((buf (connection-buffer connection)))
            (when buf
              (buffer-name buf)))))


(defcommand "Coned Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Coned"))



;;;; Maintenance hooks.

(eval-when (:compile-toplevel :execute)
(defmacro with-coned-point ((point buffer &optional pos) &rest body)
  (let ((pos (or pos (gensym))))
    `(when (and *coned-connections*
                (not (eq *coned-buffer* ,buffer))
                (not (eq *echo-area-buffer* ,buffer)))
       (let ((,pos (position ,buffer *coned-connections* :key #'car
                             :test #'eq :end *coned-connections-end*)))
         (unless ,pos (error "Unknown Coned buffer."))
         (let ((,point (buffer-point *coned-buffer*)))
           (unless (line-offset (buffer-start ,point) ,pos 0)
             (error "Coned buffer not displayed?"))
           (with-writable-buffer (*coned-buffer*) ,@body))))))
) ;eval-when
