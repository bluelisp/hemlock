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

(defcommand "Install Clbuild Project" (p)
  "" ""
  (let ((projects (list-marked-clbuild-projects)))
    (dolist (project projects)
      (install-clbuild-project project (not p)))))

(defun install-clbuild-project (project dependenciesp)
  (make-new-shell nil
                  nil
                  `("clbuild"
                    "install"
                    ,@ (ecase dependenciesp
                         ((t) (list "--dependencies"))
                         ((nil) (list "--no-dependencies"))
                         (:ask nil))
                    ,(clbuild-info-name project))
                  t))

(defun process-output-to-string (cmd)
  (message "Running ~A..." cmd)
  (with-output-to-string (s)
    (let ((proc
           (make-process-connection
            cmd
            :filter (lambda (connection bytes)
                      (write-string (hi::default-filter connection bytes)
                                    s)
                      nil))))
      (iter:iter (iter:until (connection-exit-code proc))
                 (dispatch-events)))
    (message "Done" )))

(defvar *clbuild-directory* nil)

(defun clbuild-directory ()
  (or *clbuild-directory*
      (setf *clbuild-directory*
            (concatenate 'string
                         (string-right-trim
                          #.(format nil "/~%")
                          (process-output-to-string "clbuild pwd"))
                         "/"))))

(defun list-clbuild-info ()
  (let ((source (merge-pathnames "source/" (clbuild-directory))))
    (flet ((process-file (name)
             (with-open-file (s (merge-pathnames name (clbuild-directory)))
               (iter:iter (let ((line (read-line s nil)))
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
                                              :dependencies nil)))))))))
      (sort (append (process-file "projects")
                    (process-file "wnpp-projects")
                    (process-file "my-projects"))
            #'string-lessp
            :key #'clbuild-info-name))))

(defun list-marked-clbuild-projects ()
  "All marked projects or the current one, as a list."
  (unless (eq *clbuild-buffer* (current-buffer))
    (editor-error "Not in the clbuild buffer."))
  (or (coerce (remove-if-not #'clbuild-info-marked
                             (subseq *clbuild-info* 0 *clbuild-info-end*))
              'list)
      (list (array-element-from-mark (current-point) *clbuild-info*))))

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
(bind-key "Install Clbuild Project" #k"i" :mode "Clbuild")
