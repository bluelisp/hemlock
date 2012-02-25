;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+(or CMU scl)
(ext:file-comment
  "$Header: /home/david/phemlock/cvsroot/phemlock/spell-build.lisp,v 1.1 2004-07-09 13:39:14 gbaumann Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles
;;;    Designed by Bill Chiles and Rob Maclachlan
;;;

;;; This file contains code to build a new binary dictionary file from
;;; text in system space.  This code relies on implementation dependent
;;; code from spell-rt.lisp.  Also, it is expected that spell-corr.lisp
;;; and spell-aug.lisp have been loaded.  In order to compile this file,
;;; you must first compile spell-rt, spell-corr.lisp, and spell-aug.lisp.

;;; The text file must be in the following format:
;;;      entry1/flag1/flag2/flag3
;;;  OR  entry1/flag1flag2flag3
;;;      entry2
;;;      entry3/flag1/flag2/flag3/flag4/flag5.
;;; The flags are single letter indicators of legal suffixes for the entry;
;;; the available flags and their correct use may be found at the beginning
;;; of spell-corr.lisp in the Hemlock sources.  There must be exactly one
;;; entry per line, and each line must be flushleft.

;;; The dictionary is built in system space as three distinct
;;; blocks of memory: the dictionary which is a hash table whose elements
;;; are one machine word or of type '(unsigned-byte 16); a descriptors
;;; vector which is described below; and a string table.  After all the
;;; entries are read in from the text file, one large block of memory is
;;; validated, and the three structures are moved into it.  Then the file
;;; is written.  When the large block of memory is validated, enough
;;; memory is allocated to write the three vector such that they are page
;;; aligned.  This is important for the speed it allows in growing the
;;; "dictionary" when augmenting it from a user's text file (see
;;; spell-aug.lisp).


(in-package :spell)



;;;; Constants

(eval-when (compile load eval)

(defconstant max-entry-count-estimate 400000)

;;; This is a hash table and should be a prime number.
(defconstant new-dictionary-size 446549)

(defconstant new-descriptors-size (1+ (* 4 max-entry-count-estimate)))

(defconstant max-string-table-length (* 10 max-entry-count-estimate))

); eval-when


;;;; Hashing

;;; These hashing macros are different from the ones in Spell-Correct.Lisp
;;; simply because we are using separate space and global specials/constants.
;;; Of course, they should be identical, but it doesn't seem worth cluttering
;;; up Spell-Correct with macro generating macros for this file.

(eval-when (compile eval)

(defmacro new-hash2-increment (hash)
  `(- new-dictionary-size
      2
      (the fixnum (rem ,hash (- new-dictionary-size 2)))))

(defmacro new-hash2-loop (loc hash dictionary)
  (let ((incr (gensym))
        (loop-loc (gensym)))
    `(let* ((,incr (new-hash2-increment ,hash))
            (,loop-loc ,loc))
       (declare (fixnum ,incr ,loop-loc))
       (loop (setf ,loop-loc
                   (rem (+ ,loop-loc ,incr) new-dictionary-size))
             (when (zerop (the fixnum (aref ,dictionary ,loop-loc)))
               (return ,loop-loc))
             (when (= ,loop-loc ,loc) (return nil))))))

(defmacro new-hash-entry (entry entry-len dictionary)
  (let ((hash (gensym))
        (loc (gensym)))
    `(let* ((,hash (string-hash ,entry ,entry-len))
            (,loc (rem ,hash new-dictionary-size)))
       (declare (fixnum ,loc))
       (cond ((not (zerop (the fixnum (aref ,dictionary ,loc))))
              (incf *collision-count*)
              (new-hash2-loop ,loc ,hash ,dictionary))
             (t ,loc)))))

) ;eval-when



;;;; Build-Dictionary

;;; An interesting value when building an initial dictionary.
(defvar *collision-count* 0)

(defvar *new-dictionary*)
(defvar *new-descriptors*)
(defvar *new-string-table*)

(defun build-dictionary (input output &optional save-structures-p)
  (let ((dictionary (make-array new-dictionary-size
                                :element-type '(unsigned-byte 32)))
        (descriptors (make-array new-descriptors-size
                                :element-type '(unsigned-byte 16)))
        (string-table (make-string max-string-table-length)))
    (write-line "Reading dictionary ...")
    (force-output)
    (setf *collision-count* 0)
    (multiple-value-bind (entry-count string-table-length)
                         (read-initial-dictionary input dictionary
                                                  descriptors string-table)
      (write-line "Writing dictionary ...")
      (force-output)
      (write-dictionary output dictionary descriptors entry-count
                        string-table string-table-length)
      (when save-structures-p
        (setf *new-dictionary* dictionary)
        (setf *new-descriptors* descriptors)
        (setf *new-string-table* string-table))
      (format t "~D entries processed with ~D collisions."
              entry-count *collision-count*))))

(defun read-initial-dictionary (f dictionary descriptors string-table)
  (let* ((filename (pathname f))
         (s (open filename :direction :input :if-does-not-exist nil)))
    (unless s (error "File ~S does not exist." f))
    (multiple-value-prog1
     (let ((descriptor-ptr 1)
           (string-ptr 0)
           (entry-count 0))
       (declare (fixnum descriptor-ptr string-ptr entry-count))
       (loop (multiple-value-bind (line eofp) (read-line s nil nil)
               (declare (type (or null simple-string) line))
               (unless line (return (values entry-count string-ptr)))
               (incf entry-count)
               (when (> entry-count max-entry-count-estimate)
                 (error "There are too many entries in text file!~%~
                        Please change constants in spell-build.lisp, ~
                        recompile the file, and reload it.~%~
                        Be sure to understand the constraints of permissible ~
                        values."))
               (setf line (string-upcase line))
               (let ((flags (or (position #\/ line :test #'char=) (length line))))
                 (declare (fixnum flags))
                 (cond ((> flags max-entry-length)
                        (format t "Entry ~s too long." (subseq line 0 flags))
                        (force-output))
                       (t (let ((new-string-ptr (+ string-ptr flags)))
                            (declare (fixnum new-string-ptr))
                            (when (> new-string-ptr max-string-table-length)
                              (error "Spell string table overflow!~%~
                                     Please change constants in ~
                                     spell-build.lisp, recompile the file, ~
                                     and reload it.~%~
                                     Be sure to understand the constraints ~
                                     of permissible values."))
                            (spell-place-entry line flags
                                               dictionary descriptors string-table
                                               descriptor-ptr string-ptr)
                            (incf descriptor-ptr 4)
                            (setf string-ptr new-string-ptr)))))
               (when eofp (return (values entry-count string-ptr))))))
     (close s))))

(defun spell-place-entry (line word-end dictionary descriptors string-table
                               descriptor-ptr string-ptr)
  (declare (simple-string line string-table)
           (fixnum word-end descriptor-ptr string-ptr)
           (type (array (unsigned-byte 32) (*)) dictionary)
           (type (array (unsigned-byte 16) (*)) descriptors))
  (nstring-upcase line :end word-end)
  (let* ((hash-loc (new-hash-entry line word-end dictionary))
         (descriptor-ptr+1 (1+ descriptor-ptr))
         (descriptor-ptr+2 (1+ descriptor-ptr+1))
         (descriptor-ptr+3 (1+ descriptor-ptr+2))
         )
    (unless hash-loc (error "Dictionary Overflow!"))
    (assert (zerop (logand (1- descriptor-ptr) #b11)))
    (setf (aref dictionary hash-loc) descriptor-ptr)
    (setf (aref descriptors descriptor-ptr)
          (dpb (the fixnum
                    (ldb new-hash-byte (string-hash line word-end)))
               stored-hash-byte
               word-end))
    (setf (aref descriptors descriptor-ptr+1)
          (ldb whole-index-low-byte string-ptr))
    (setf (aref descriptors descriptor-ptr+2)
          (ldb whole-index-high-byte string-ptr))
    (setf (aref descriptors descriptor-ptr+3) 0)
    (new-add-flags descriptors descriptor-ptr+3 line word-end)
    (replace string-table line :start1 string-ptr :end2 word-end)))

(defun new-add-flags (descriptors loc line word-end)
  (declare (simple-string line)
           (fixnum word-end)
           (type (array (unsigned-byte 16) (*)) descriptors))
  (do ((flag (1+ word-end) (1+ flag))
       (line-end (length line)))
      ((>= flag line-end))
    (declare (fixnum flag line-end))
    (let ((flag-char (schar line flag)))
      (unless (char= flag-char #\/)
        (let ((flag-mask (flag-mask flag-char)))
          (declare (fixnum flag-mask))
          (if (zerop flag-mask)
              (format t "Illegal flag ~S on word ~S.~%"
                        flag-char (subseq line 0 word-end))
              (setf (aref descriptors loc)
                    (logior flag-mask (aref descriptors loc)))))))))

(defun write-dictionary (filename dictionary descriptors entry-count
                           string-table string-table-length)
  (declare (type (array (unsigned-byte 32) (*)) dictionary)
           (type (array (unsigned-byte 16) (*)) descriptors)
           (simple-string string-table)
           (fixnum string-table-length))
  (with-open-file (s filename :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (flet ((write-byte16 (byte stream)
             (declare (type (unsigned-byte 16) byte))
             (write-byte (ldb (byte 8 0) byte) stream)
             (write-byte (ldb (byte 8 8) byte) stream))
           (write-byte32 (byte stream)
             (declare (type (unsigned-byte 32) byte))
             (write-byte (ldb (byte 8 0) byte) stream)
             (write-byte (ldb (byte 8 8) byte) stream)
             (write-byte (ldb (byte 8 16) byte) stream)
             (write-byte (ldb (byte 8 24) byte) stream)))
      (let ((descriptors-size (1+ (* 4 entry-count))))
        (write-byte16 magic-file-id s)
        (format t "Dictionary size: ~D. Descriptors size: ~D. String table size: ~D.~%"
                new-dictionary-size descriptors-size string-table-length)
        (write-byte32 new-dictionary-size s)
        (write-byte32 descriptors-size s)
        (write-byte32 string-table-length s)
        (dotimes (i new-dictionary-size)
          (write-byte32 (aref dictionary i) s))
        (dotimes (i descriptors-size)
          (write-byte16 (aref descriptors i) s)))))
  (with-open-file (s filename :direction :output :element-type 'base-char
                     #+scl #+scl :external-format :iso-8859-1
                     :if-exists :append)
    (write-string string-table s :end string-table-length)))
