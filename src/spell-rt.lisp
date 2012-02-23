;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles
;;;
;;; This file contains system dependent primitives for the spelling checking/
;;; correcting code in Spell-Correct.Lisp, Spell-Augment.Lisp, and
;;; Spell-Build.Lisp.

(defpackage :spell
  (:use :cl)
  (:export spell-try-word spell-root-word spell-collect-close-words
           maybe-read-spell-dictionary correct-spelling max-entry-length
           spell-read-dictionary spell-add-entry spell-root-flags
           spell-remove-entry))

(in-package :spell)


;;;; Spell structure referencing and setting

(eval-when (:compile-toplevel :execute)

(defmacro sapref (sap offset)
  `(let ((index (* ,offset 2)))
     (logior (aref ,sap index)
             (ash (aref ,sap (+ index 1)) 8))))

(defsetf sapref (sap offset) (value)
  `(let ((index (* ,offset 2)))
     (setf (aref ,sap index) (logand ,value #xff))
     (setf (aref ,sap (+ index 1)) (ash ,value -8))))



;;;; Primitive String Hashing

;;; STRING-HASH employs the instruction SXHASH-SIMPLE-SUBSTRING which takes
;;; an end argument, so we do not have to use SXHASH.  SXHASH would mean
;;; doing a SUBSEQ of entry.
;;;
(defmacro string-hash (string length)
  #+(or cmu scl)
  `(lisp::%sxhash-simple-substring ,string ,length)
  #-(or cmu scl)
  `(if (= length (length string))
       (sxhash string)
       (sxhash (subseq string 0 length))))

) ;eval-when

(defun sap-replace (dst-string src-string src-start dst-start dst-end)
  (do ((i src-start (1+ i))
       (j dst-start (1+ j)))
      ((>= j dst-end))
    (let ((code (if (stringp src-string)
                    (char-code (schar src-string i))
                    (aref src-string i))))
      (if (stringp dst-string)
          (setf (schar dst-string j) (code-char code))
          (setf (aref dst-string j) code)))))


;;;; Binary Dictionary File I/O

(defun open-dictionary (f)
  (open f :direction :input :element-type '(unsigned-byte 8)))

(defun close-dictionary (stream)
  (close stream))

(defun read-dictionary-structure (stream bytes)
  (let* ((structure (make-array bytes :element-type '(unsigned-byte 8)))
         (count (read-sequence structure stream)))
    (unless (= bytes count)
      (error "Reading dictionary structure failed."))
    structure))

(defun read-dictionary-structure-u32 (stream size)
  (let ((structure (make-array size :element-type '(unsigned-byte 32))))
    (dotimes (i size)
      (let ((value (logior (read-byte stream)
                           (ash (read-byte stream) 8)
                           (ash (read-byte stream) 16)
                           (ash (read-byte stream) 24))))
        (setf (aref structure i) value)))
    structure))
