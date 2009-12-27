;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Slave Apropos (as opposed to "hemlock com>mand name apropos" aka Apropos)

(in-package :hemlock)



(defvar *apropos-entries* nil)
(defvar *apropos-entries-end* nil)
;;;

(defstruct (apropos-entry
             (:constructor internal-make-apropos-entry
                           (slavesym kind docstring)))
  slavesym
  kind
  docstring)

(defun parse-apropos-entry (slot-list)
  (destructuring-bind (slavesym &optional (kind :unbound) docstring)
                      slot-list
    (internal-make-apropos-entry
     slavesym
     kind
     (if (eq docstring :not-documented) nil docstring))))

;;; This is the apropos buffer if it exists.
;;;
(defvar *apropos-buffer* nil)

;;; This is the cleanup method for deleting *apropos-buffer*.
;;;
(defun delete-apropos-buffers (buffer)
  (when (eq buffer *apropos-buffer*)
    (setf *apropos-buffer* nil)
    (setf *apropos-entries* nil)))


;;;; Commands.

(defmode "Apropos" :major-p t
  :documentation "Apropos mode presents a list of slave symbols.")

(defcommand "Apropos Quit" (p)
  "Kill the apropos buffer."
  ""
  (declare (ignore p))
  (when *apropos-buffer* (delete-buffer-if-possible *apropos-buffer*)))

(defun apropos-entry-from-mark (mark)
  )

(defcommand "Apropos Find Definition" (p)
  "" ""
  (declare (ignore p))
  (let ((entry (apropos-entry-from-mark (current-point))))
    (when entry
      (change-to-definition entry))))

(defcommand "Apropos Describe" (p)
  "" ""
  (declare (ignore p))
  (let ((entry (apropos-entry-from-mark (current-point))))
    (when entry
      (change-to-definition entry))))

(defun refresh-apropos (buf entries)
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (setf *apropos-entries-end* (length entries))
    (setf *apropos-entries* (coerce entries 'vector))
    (with-output-to-mark (s (buffer-point buf))
      (dolist (entry entries)
        (apropos-write-line entry s)))))

(defun make-apropos-buffer (entries)
  (let ((buf (or *apropos-buffer*
                 (make-buffer "*Slave Apropos*" :modes '("Apropos")))))
    (setf *apropos-buffer* buf)
    (refresh-apropos buf entries)
    (let ((fields (buffer-modeline-fields *apropos-buffer*)))
      (setf (cdr (last fields))
            (list (or (modeline-field :apropos-cmds)
                      (make-modeline-field
                       :name :apropos-cmds :width 18
                       :function
                       #'(lambda (buffer window)
                           (declare (ignore buffer window))
                           "  Type ? for help.")))))
      (setf (buffer-modeline-fields *apropos-buffer*) fields))
    (buffer-start (buffer-point buf))
    (change-to-buffer buf)))

(defun apropos-write-line (entry s)
  (format s "~A~%  ~:(~A~)~:[~;: ~:*~A~]~%~%"
          (apropos-entry-slavesym entry)
          (apropos-entry-kind entry)
          (let ((docstring (apropos-entry-docstring entry)))
            (when docstring
              (first-line-of-string docstring)))))

(defun first-line-of-string (str)
  (with-input-from-string (s str) (read-line s nil "")))

(defcommand "Apropos Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Apropos"))

(defcommand "Slave Apropos" (p)
  "" ""
  (declare (ignore p))
  (let ((default (hemlock::symbol-string-at-point)))
    ;; Fixme: MARK-SYMBOL isn't very good, meaning that often we
    ;; will get random forms rather than a symbol.  Let's at least
    ;; catch the case where the result is more than a line long,
    ;; and give up.
    (when (find #\newline default)
      (setf default nil))
    (slave-apropos
     (hemlock-interface::prompt-for-string
      :prompt "Apropos string: "
      :default default))))

(defun slave-apropos (str)
  (hemlock::eval-in-slave `(%apropos ',str)))

(defun %apropos (str)
  (let ((data
         (mapcar (lambda (sym)
                   (cons (make-slave-symbol sym)
                         (conium:describe-symbol-for-emacs sym)))
                 (apropos-list str))))
    (hemlock::eval-in-master `(%apropos-results ',data ',str))))

(defun %apropos-results (data str)
  (let ((entries (mapcar #'parse-apropos-entry data)))
    (cond
     ((null data)
      (message "No apropos results for: ~A" str))
     (t
      (make-apropos-buffer entries)))))
