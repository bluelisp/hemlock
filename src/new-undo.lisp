;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock-internals)

(defvar *log* nil)

;;; Ouch! this all isn't _that_ easy.

(defmacro add-logging (attr)
  `(defmethod (setf ,attr) :around (new-value line)
    (let ((old (,attr line)))
      (push `(,',attr ,line ,old ,new-value) *log*))
    (call-next-method)))

(add-logging line-previous)
(add-logging line-next)
(add-logging mark-line)

(defun dada ()
  (let ((log *log*)
        (*log* nil))
    (dolist (k log)
      (destructuring-bind (slot object old new) k
        (funcall (fdefinition `(setf ,slot)) old object)))))
;;;;

(defun mark-position (mark)
  (let ((line-no 0)
        (line (mark-line mark)))
    (do ()
        ((null (line-previous line)))
      (incf line-no)
      (setf line (line-previous line)))
    (list (line-buffer (mark-line mark))
          line-no (mark-charpos mark))))

(defmethod insert-character :around (mark character)
  (push `(insert-character ,(mark-position mark) ,character)
        *log*)
  (call-next-method))

(defmethod insert-string :around (mark string &optional (start 0) (end (length string)))
  (push `(insert-string ,(mark-position mark) ,(subseq string start end))
        *log*)
  (call-next-method))

(defmethod delete-characters :around (mark &optional (n 1))
  (push `(delete-characters ,(mark-position mark) ,n)
        *log*)
  (call-next-method))

(defun dada (q)
  (dolist (k q)
    (ecase (car k)
      (insert-character
       (destructuring-bind ((buffer line-no char-pos) char) (cdr k)
         (delete-characters (position-mark buffer line-no char-pos)))))))

(defun position-mark (buffer line-no char-pos)
  (let ((line (mark-line (buffer-start-mark buffer))))
    (dotimes (i line-no)
      (setf line (line-next line)))
    (mark line char-pos)))