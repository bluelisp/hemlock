;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock-internals)

(defvar *log* nil)

;;; Ouch! this all isn't _that_ easy.

;; (defmacro add-logging (attr)
;;   `(defmethod (setf ,attr) :around (new-value line)
;;     (let ((old (,attr line)))
;;       (push `(,',attr ,line ,old ,new-value) *log*))
;;     (call-next-method)))

;; (add-logging line-previous)
;; (add-logging line-next)
;; (add-logging mark-line)

;; (defun dada ()
;;   (let ((log *log*)
;;         (*log* nil))
;;     (dolist (k log)
;;       (destructuring-bind (slot object old new) k
;;         (funcall (fdefinition `(setf ,slot)) old object)))))
;;;;

(defvar *performing-undo* nil)

(defun mark-position (mark)
  (let ((line-no 0)
        (line (mark-line mark)))
    (do ()
        ((null (line-previous line)))
      (incf line-no)
      (setf line (line-previous line)))
    (list (line-buffer (mark-line mark))
          line-no (mark-charpos mark))))

;;; below, I am not quite sure about left vs. right inserting --amb

(defmethod insert-character :around (mark character)
  (with-mark ((start mark :right-inserting))
    (prog1
        (call-next-method)
      (unless (or *performing-undo*
                  (eq (line-buffer (mark-line mark)) *echo-area-buffer*))
        (push `(delete-characters ,(mark-position start)) *log*)))))

(defmethod insert-string :around (mark string &optional (start 0) (end (length string)))
  (if (car (mark-position mark)) ; used with kill-ring?
      (progn
        (with-mark ((start mark :right-inserting)
                    (end mark :left-inserting))
          (prog1
              (call-next-method)
            (unless (or *performing-undo*
                        (eq (line-buffer (mark-line mark)) *echo-area-buffer*))
              (push
               `(delete-region ,(mark-position start) ,(mark-position end))
               *log*)))))
      (call-next-method)))

(defmethod insert-region :around (mark region)
  (with-mark ((start mark :right-inserting)
              (end mark :left-inserting))
    (prog1
        (call-next-method)
      (unless (or *performing-undo*
                  (eq (line-buffer (mark-line mark)) *echo-area-buffer*))
        (push
         `(delete-region ,(mark-position start) ,(mark-position end))
         *log*)))))

(defmethod delete-characters :around (mark &optional (n 1))
  (with-mark ((start mark :right-inserting)
              (end mark :left-inserting))
    (character-offset end n)
    (let ((string (region-to-string (region start end))))
      (prog1
          (call-next-method)
        (unless (or *performing-undo*
                    (eq (line-buffer (mark-line mark)) *echo-area-buffer*))
          (push
           `(insert-string ,(mark-position start) ,string)
           *log*))))))

(defmethod delete-region :around (region)
  (with-mark ((start (region-start region) :right-inserting)
              (end (region-end region) :left-inserting))
    (let ((string (region-to-string region)))
      (prog1
          (call-next-method)
        (unless (or *performing-undo*
                    (eq (line-buffer (mark-line (region-start region)))
                        *echo-area-buffer*))
          (push
           `(insert-string ,(mark-position start) ,string)
           *log*))))))

(defmethod delete-and-save-region :around (region)
  (with-mark ((start (region-start region) :right-inserting)
              (end (region-end region) :left-inserting))
    (let ((string (region-to-string region)))
      (prog1
          (call-next-method)
        (unless (or *performing-undo*
                    (eq (line-buffer (mark-line (region-start region)))
                        *echo-area-buffer*))
          (push
           `(insert-string ,(mark-position start) ,string)
           *log*))))))

(defun dada ()
  (let ((*performing-undo* t))
    (do ((k (pop *log*) (pop *log*)))
        ((null k))
      (undo k))))

(defun undo (k)
  (ecase (car k)
    (delete-characters
     (destructuring-bind ((buffer line-no char-pos)) (cdr k)
       (delete-characters (position-mark buffer line-no char-pos))))
    (delete-region
     (destructuring-bind ((buffer1 line-no1 char-pos1)
                          (buffer2 line-no2 char-pos2)) (cdr k)
       (delete-region
        (region (position-mark buffer1 line-no1 char-pos1)
                (position-mark buffer2 line-no2 char-pos2)))))
    (insert-string
     (destructuring-bind ((buffer line-no char-pos) string) (cdr k)
       (insert-string (position-mark buffer line-no char-pos) string)))))

(defun position-mark (buffer line-no char-pos)
  (let ((line (mark-line (buffer-start-mark buffer))))
    (dotimes (i line-no)
      (if line
        (setf line (line-next line))
        (error "Line is NIL")))
    (mark line char-pos)))