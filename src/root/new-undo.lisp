;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Prelimiary Undo
;;;   Created: 2004-12-26
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2004 by Gilbert Baumann

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;; TODO

;; - Some form of consolidation
;; - Redo
;; - Ensure that what we indentified as a protocol of functions
;;   modifying a buffer is not violated. Both by runtime and compile
;;   time measures.
;; - Look and Feel:
;;   Find out what different variants of undo are implemented both
;;   with gnu emacs and xemacs, try to simulate those.
;; - Q: Are there any commands that modify more than one buffer?
;;   (Besides the kill ring and stuff)
;; - FILTER-REGION (function region) is missing
;; - (SETF NEXT-CHARACTER) (character mark) is missing
;; - Don't record undo information of functions that don't modify the
;;   buffer at all.

;; There is this MODIFYING-BUFFER which should enable access to
;; LINE-NEXT and LINE-PREVIOUS.

;; Instead of putting :command entires onto an undo list, do
;; undo-chunks directly.

;; Q: Do we need a separate Redo command or should we go the route
;;    that XEmacs follows and make any other command than undo change
;;    the current undo sequence and let subsequent undos really redo
;;    stuff? This is useful but awkward.

;; Also: I really want the self-insert-commands be grouped by words
;; and not just by 20 characters as the [documented] behaviour of
;; XEmacs. This also is the observed behavior.

(in-package :hemlock-internals)

;; Unfortunately we need numeric buffer positions. (Hmm, maybe after
;; all RMS has a point?) Anyhow to graft this onto hemlock we define
;; two functions MARK-POSITION and POSTION-MARK to convert and back
;; and fro. Further these new kind of buffer positions are passed
;; around as (buffer line-number character-position) triples.

(defun mark-position (mark)
  (let ((line-no 0)
        (line (mark-line mark)))
    (do ()
        ((null (line-previous line)))
      (incf line-no)
      (setf line (line-previous line)))
    (list (line-buffer (mark-line mark))
          line-no (mark-charpos mark))))

(defun position-mark (buffer line-no char-pos)
  (let ((line (mark-line (buffer-start-mark buffer))))
    (assert line)
    (dotimes (i line-no)
      (let ((next (line-next line)))
        (unless next
          (error "Corrupted undo position, line ~D column ~D"
                 line-no char-pos))
        (setf line next)))
    (mark line char-pos)))

;;;; Insertion

(defun update-tag-line-number (mark)
  (let* ((line (mark-line mark))
         (buffer (line-buffer line)))
    (assert buffer)
    (setf (buffer-tag-line-number buffer)
          (min (buffer-tag-line-number buffer)
               (line-number line)))))

;;; Insert can call itself and other functions.  We only want to recorder
;;; the outermost call.
(defvar *insert-noted-p* nil)

(defmethod insert-character :around (mark character)
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-string ,(mark-position mark)
                                   ,(string character))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod insert-string :around (mark string &optional (start 0) (end (length string)))
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-string ,(mark-position mark)
                                   ,(subseq string start end))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod insert-region :around (mark region)
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-string ,(mark-position mark)
                                   ,(region-to-string region))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod ninsert-region :around (mark region)
  ;; the "n" refers to the region argument.
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-region ,(mark-position mark)
                                   ,(region-to-string region))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

;;;; Deletion

;; We make delete-characters and delete-region both call off to
;; delete-and-save-region which is the most general method and has the
;; benefit to return the deleted stuff.

(defmethod delete-characters :around (mark &optional (n 1))
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           ;; For now delete-characters just calls delete-region in
           ;; any case.  code borrowed from htext4.lisp
           (let* ((line (mark-line mark))
                  (charpos (mark-charpos mark))
                  (length (line-length* line)))
             (setf (mark-line *internal-temp-mark*) line
                   (mark-charpos *internal-temp-mark*) charpos)
             (let ((other-mark (character-offset *internal-temp-mark* n)))
               (cond
                 (other-mark
                  (if (< n 0)
                      (setf (region-start *internal-temp-region*) other-mark
                            (region-end *internal-temp-region*) mark)
                      (setf (region-start *internal-temp-region*) mark
                            (region-end *internal-temp-region*) other-mark))
                  (delete-and-save-region *internal-temp-region*)
                  t)
                 (t nil)))))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))
           

(defmethod delete-region :around (region)
  (let* ((mark (region-start region))
         (buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (delete-and-save-region region))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod delete-and-save-region :around (region)
  (let* ((mark (region-start region))
         (buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (let ((pos (mark-position mark))
                 (matter (call-next-method)))
             (push `(delete-region ,pos ,(region-to-string matter))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark)
             matter))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

;;;;

(defvar last-was-undo-p nil)
(defvar this-is-undo-p nil)
(defvar undoing-undo-list nil)

(defcommand "New Undo" (p)
  ""
  ""
  (setf this-is-undo-p t)
  ;; ### pop the "New Undo" log entry
  (let* ((buffer (current-buffer))
         (undo-list (if last-was-undo-p
                        undoing-undo-list
                        (cddr (buffer-undo-list buffer))))
         (modifiedp nil))
    (block baz
      (loop
          (let ((chunk (pop undo-list)))
            (when (or (null chunk)
                      (and (eq (car chunk) :command) modifiedp))
              (let ((name (cadr chunk)))
                (if name
                    (message "~S" name)
                    (message "No further undo information")))
              (return-from baz nil))
            (when (and (not (eq (car chunk) :command))
                       (consp (cadr chunk))
                       (eq (car (cadr chunk)) buffer))
              (case (car chunk)
                (insert-string
                 (let* ((p (cadr chunk))
                        (matter (caddr chunk))
                        (n (length matter)))
                   ;; Firstly double check that the characters being
                   ;; deleted are as expected.
                   (let ((mark (apply #'position-mark p)))
                     (dotimes (i n)
                       (let ((c (next-character mark)))
                         (unless (eql c (schar matter i))
                           (error "Undo lost sync at line ~D colum ~D"
                                  (mark-line mark) (mark-charpos mark))))
                       (mark-after mark)))
                   (let ((mark (apply #'position-mark p)))
                     (delete-characters mark n)
                     (setf modifiedp t))))
                (delete-region
                 (let ((p (cadr chunk))
                       (matter (caddr chunk)))
                   (let ((mark (apply #'position-mark p)))
                     (insert-string mark matter)
                     (setf modifiedp t))))
                (point-position
                 (move-mark (current-point) (apply #'position-mark (cadr chunk)))) )))))

    (setf undoing-undo-list undo-list) ))

(defun new-undo-invoke-hook (command p)
  (declare (ignore p))
  (setf this-is-undo-p nil)
  (let ((buffer (current-buffer)))
    (when (and buffer (buffer-undo-p buffer))
      (push (list :command command) (buffer-undo-list buffer))
      (push (list 'point-position (mark-position (current-point)))
            (buffer-undo-list buffer))))
  nil)

(defparameter *invoke-hook* #'(lambda (command p)
                                (new-undo-invoke-hook command p)
                                (funcall (command-function command) p)
                                (setf last-was-undo-p this-is-undo-p))
  "This function is called by the command interpreter when it wants to invoke a
  command.  The arguments are the command to invoke and the prefix argument.
  The default value just calls the Command-Function with the prefix argument.")


(defcommand "Undo Mode" (p)
  "Enable the recording of Undo information in the current buffer."
  "Enable the recording of Undo information in the current buffer."
  (let* ((buffer (current-buffer))
         (p (if p (plusp p) (not (buffer-undo-p buffer)))))
    (setf (buffer-undo-p buffer) p)
    (setf (buffer-undo-list buffer) nil)
    (if p
        (message "Undo enabled")
        (message "Undo disabled"))))

