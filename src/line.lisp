;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(in-package :hemlock-internals)

;;;
;;; **********************************************************************
;;;
;;; This file contains definitions for the Line structure, and some
;;; functions and macros to manipulate them.
;;;

(setf (documentation 'linep 'function)
  "Returns true if its argument is a Hemlock line object, Nil otherwise.")
(setf (documentation 'line-previous 'function)
  "Return the Hemlock line that precedes this one, or Nil if there is no
  previous line.")
(setf (documentation 'line-next 'function)
  "Return the Hemlock line that follows this one, or Nil if there is no
  next line.")
(setf (documentation 'line-plist 'function)
  "Return a line's property list.  This may be manipulated with Setf and Getf.")


;;;; The line object:

(defclass line ()
  ((chars
    :initform ""
    :initarg :chars                     ; Hide the fact that the slot isn't really called CHARS.
    :accessor line-chars
    :documentation "Something that represents the contents of the line.  This is
                    guaranteed to change (as compared by EQL) whenver the contents of the
                    line changes, but might at arbitarary other times.  There are
                    currently about two different cases:

                    Normal:
                       A simple string holding the contents of the line.

                    A cached line:
                       The line is eq to Open-Line, and the actual contents are in the
                       line cache.  The %Chars may be either the original contents or a
                       negative fixnum.")
   (previous
    :initform nil
    :initarg :previous
    :accessor line-previous
    :documentation "Pointer to the previous line in the doubly linked list of line instances.")
   (next
    :initform nil
    :initarg :next
    :accessor line-next
    :documentation "Pointer to the next line in the doubly linked list of line instances.")
   (marks
    :initform nil
    :initarg :marks
    :accessor line-marks
    :documentation "A list of all the permanent marks pointing into this line.")
   (%buffer
    :initform nil
    :initarg :%buffer
    :accessor line-%buffer
    :documentation "The buffer to which this line belongs, or a *disembodied-buffer-count* if the line is not in any buffer.")
   (number
    :initform 0
    :initarg :number
    :accessor line-number
    :documentation "A non-negative integer (fixnum) that represents the ordering of lines
                    within continguous range of lines (a buffer or disembuffered region).
                    The number of the Line-Next is guaranteed to be strictly greater than
                    our number, and the Line-Previous is guaranteed to be strictly less.")
   (plist
    :initform nil
    :initarg :plist
    :accessor line-plist
    :documentation "The line property list, used by user code to annotate the text.")
   (tag
    :accessor %line-tag
    :initform nil
    :documentation "Line tag, which records information available only if all
                    preceding lines have been analyzed yet."))
  (:documentation
   "A Hemlock line object.  See Hemlock design document for details."))

(defun make-line (&rest initargs)
  (apply #'make-instance 'line initargs))

(defmethod linep ((line line))
  t)

(defmethod linep ((object t))
  nil)

(defstruct (syntax-info
             (:conc-name sy-)
             (:constructor make-syntax-info
                           (signature from-state to-state font-marks)))
  (signature :bogus-signature)
  from-state
  to-state
  font-marks)

(defstruct tag
  (ticks -1)
  (line-number 1 :type (integer 1))
  (syntax-info nil :type (or null syntax-info))
  (package (symbol-name :cl-user) :type (or null string)))


;;; Line-Signature  --  Public
;;;
;;;    We can just return the Line-Chars.
;;;
(declaim (inline line-signature))
(defun line-signature (line)
  "This function returns an object which serves as a signature for a line's
  contents.  It is guaranteed that any modification of text on the line will
  result in the signature changing so that it is not EQL to any previous value.
  Note that the signature may change even when the text hasn't been modified, but
  this probably won't happen often."
  (line-chars line))


(defun %copy-line (line &key previous number %buffer)
  (make-line :chars (line-chars line)
             :previous previous
             :number number
             :%buffer %buffer))


(defmacro line-length* (line)
  "Returns the number of characters on the line, but it's a macro!"
  `(cond ((eq ,line open-line)
          (+ left-open-pos (- line-cache-length right-open-pos)))
         (t
          (length (the simple-string (line-chars ,line))))))

;; $Log: line.lisp,v $
;; Revision 1.2  2004-12-15 12:16:45  crhodes
;; Make clim-hemlock basically work on sbcl -- mostly build fixes from Hannu
;; Koivisto.
;;
;; * don't declaim or declare stuff in CL special;
;; * classes come before methods specializing on them;
;; * clim-sys: not mp:
;;
;; Revision 1.1  2004/07/09 15:00:36  gbaumann
;; Let us see if this works.
;;
