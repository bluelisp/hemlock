;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains clbuild-related code.
;;;

(in-package :hemlock)



;;;; Representation of existing connections.

(defvar *clbuild-info* nil)
(defvar *clbuild-info-end* nil)
;;;

(defstruct clbuild-info
  (marked nil)
  name
  installed
  description
  dependencies)


;;; This is the clbuild buffer if it exists.
;;;
(defvar *clbuild-buffer* nil)

;;; This is the cleanup method for deleting *clbuild-buffer*.
;;;
(defun delete-clbuild-buffers (buffer)
  (when (eq buffer *clbuild-buffer*)
    (setf *clbuild-buffer* nil)
    (setf *clbuild-info* nil)))


;;;; Commands.

(defmode "Clbuild" :major-p t
  :documentation
  "clbuild projects")

(defcommand "Mark Clbuild Project" (p)
  "" ""
  (declare (ignore p))
  (let* ((point (current-point))
         (buf-info (array-element-from-mark point *clbuild-info*)))
    (with-writable-buffer (*clbuild-buffer*)
      (setf (clbuild-info-marked buf-info) t)
      (with-mark ((point point))
        (setf (next-character (line-start point)) #\*))
      (line-offset point 1))))

(defcommand "Unmark Clbuild Project" (p)
  "" ""
  (declare (ignore p))
  (with-writable-buffer (*clbuild-buffer*)
    (setf (clbuild-info-marked
           (array-element-from-mark (current-point) *clbuild-info*))
          nil)
    (with-mark ((point (current-point)))
      (setf (next-character (line-start point)) #\space))
    (line-offset (current-point) 1)))

(defcommand "Clbuild Quit" (p)
  "" ""
  (declare (ignore p))
  (when *clbuild-buffer* (delete-buffer-if-possible *clbuild-buffer*)))

#+(or)                                  ;use dired?
(defcommand "Clbuild Goto" (p)
  "" ""
  (declare (ignore p))
  (change-to-buffer
   (or (connection-buffer
        (clbuild-info
         (array-element-from-mark (current-point) *clbuild-info*)))
       (editor-error "connection has no buffer"))))

#+(or)
(defcommand "Clbuild Dependency Graph" (p)
  "" ""
  (declare (ignore p))
  (let ((name (mapcar #'clbuild-info-name (list-marked-clbuild-projects))))
    (if *dependency-graph-buffer*
        (qt-hemlock::add-project-to-graph nil name)
        (qt-hemlock::show-project-graph-command nil names))))

(defcommand "Clbuild Dependency Graph" (p)
  "" ""
  (declare (ignore p))
  (let ((name (clbuild-info-name
               (array-element-from-mark (current-point) *clbuild-info*))))
    (if qt-hemlock::*dependency-graph-buffer*
        (progn
          (change-to-buffer qt-hemlock::*dependency-graph-buffer*)
          (qt-hemlock::add-project-to-graph-command nil name))
        (qt-hemlock::show-project-graph-command nil (list name)))))

(defvar *clbuild-directory* "/home/david/clbuild/")

(defun list-clbuild-info ()
  (let ((source (merge-pathnames "source/" *clbuild-directory*)))
    (with-open-file (s (merge-pathnames "projects" *clbuild-directory*))
      (sort (iter:iter (let ((line (read-line s nil)))
                         (iter:while line)
                         (cl-ppcre:register-groups-bind
                          (name comment)
                          ("^(^[a-zA-Z0-9_-]*)[^#]*(?:#(.*))?$" line)
                          (when (plusp (length name))
                            (iter:collect (make-clbuild-info
                                           :name name
                                           :installed (probe-file
                                                       (merge-pathnames
                                                        name
                                                        source))
                                           :description (or comment "")
                                           :dependencies nil))))))
            #'string-lessp
            :key #'clbuild-info-name))))

(defun list-marked-clbuild-projects ()
  (unless (eq *clbuild-buffer* (current-buffer))
    (editor-error "Not in the clbuild buffer."))
  (coerce (remove-if-not #'clbuild-info-marked
                         (subseq *clbuild-info* 0 *clbuild-info-end*))
          'list))

(defun refresh-clbuild (buf)
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (let ((projects (coerce (list-clbuild-info) 'vector)))
      (setf *clbuild-info-end* (length projects))
      (setf *clbuild-info* projects)
      (with-output-to-mark (s (buffer-point buf))
        (iter:iter (iter:for c in-vector projects)
                   (clbuild-write-line c s)))
      (buffer-start (current-point)))))

(defcommand "Clbuild" (p)
  "" ""
  (declare (ignore p))
  (let ((buf (or *clbuild-buffer*
                 (make-buffer "Clbuild" :modes '("Clbuild")
                              :delete-hook (list #'delete-clbuild-buffers)))))
    (unless *clbuild-buffer*
      (setf *clbuild-buffer* buf)
      (refresh-clbuild buf)
      (let ((fields (buffer-modeline-fields *clbuild-buffer*)))
        (setf (cdr (last fields))
              (list (or (modeline-field :clbuild-cmds)
                        (make-modeline-field
                         :name :clbuild-cmds :width 18
                         :function
                         #'(lambda (buffer window)
                             (declare (ignore buffer window))
                             "  Type ? for help.")))))
        (setf (buffer-modeline-fields *clbuild-buffer*) fields))
      (buffer-start (buffer-point buf)))
    (change-to-buffer buf)))

(defcommand "Clbuild Refresh" (p)
  "" ""
  (declare (ignore p))
  (when *clbuild-buffer*
    (refresh-clbuild *clbuild-buffer*)))

(defun clbuild-write-line (info s)
  (format s "  ~C ~A~30T~A~%"
          (if (clbuild-info-installed info) #\i #\u)
          (clbuild-info-name info)
          (clbuild-info-description info)))

(defcommand "Clbuild Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Clbuild"))

(bind-key "Mark Clbuild Project" #k"m" :mode "Clbuild")
(bind-key "Unmark Clbuild Project" #k"u" :mode "Clbuild")
;;; (bind-key "Clbuild Expunge" #k"!" :mode "Clbuild")
;;; (bind-key "Clbuild Expunge" #k"x" :mode "Clbuild")
(bind-key "Clbuild Quit" #k"q" :mode "Clbuild")
;; (bind-key "Clbuild Goto" #k"space" :mode "Clbuild")
(bind-key "Clbuild Refresh" #k"g" :mode "Clbuild")
(bind-key "Next Line" #k"n" :mode "Clbuild")
(bind-key "Previous Line" #k"p" :mode "Clbuild")
(bind-key "Clbuild Help" #k"?" :mode "Clbuild")
(bind-key "Clbuild Dependency Graph" #k"$" :mode "Clbuild")
