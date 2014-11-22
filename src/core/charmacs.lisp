;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Implementation specific character-hacking macros and constants.
;;;
(in-package :hemlock-internals)

;;; This file contains various constants and macros which are implementation or
;;; ASCII dependant.  It contains some versions of CHAR-CODE which do not check
;;; types and ignore the top bit so that various structures can be allocated
;;; 128 long instead of 256, and we don't get errors if a loser visits a binary
;;; file.
;;;
;;; There are so many different constants and macros implemented the same.
;;; This is to separate various mechanisms; for example, in principle the
;;; char-code-limit for the syntax functions is independant of that for the
;;; searching functions
;;;




;;; Unicode has a large range of characters.  The first 256 are stored in
;;; a vector, and the rest are stored in a hash table.  A default for the
;;; hash table can be supplied.
(defstruct character-set
  page0
  table
  default)

(defun char-set-ref (set code)
  (cond ((< code 256)
         (let ((page0 (character-set-page0 set)))
           (aref page0 code)))
        (t
         (let ((table (character-set-table set))
               (default (character-set-default set)))
           (gethash code table default)))))

(defun (setf char-set-ref) (value set code)
  (cond ((< code 256)
         (let ((page0 (character-set-page0 set)))
           (setf (aref page0 code) value)))
        (t
         (let ((table (character-set-table set)))
           (setf (gethash code table) value)))))

(defun hi::%sp-find-character-with-attribute (string start end table mask)
  (declare (simple-string string)
           (fixnum start end mask)
           (type character-set table))
  "%SP-Find-Character-With-Attribute  String, Start, End, Table, Mask
  The codes of the characters of String from Start to End are used as indices
  into the Table, which is a U-Vector of 8-bit bytes. When the number picked
  up from the table bitwise ANDed with Mask is non-zero, the current
  index into the String is returned. The corresponds to SCANC on the Vax."
  (do ((index start (1+ index)))
      ((= index end) nil)
    (declare (fixnum index))
    (if (/= (logand (char-set-ref table (char-code (elt string index))) mask) 0)
        (return index))))

(defun hi::%sp-reverse-find-character-with-attribute (string start end table
                                                      mask)
  (declare (simple-string string)
           (fixnum start end mask)
           (type character-set table))
  "Like %SP-Find-Character-With-Attribute, only sdrawkcaB."
  (do ((index (1- end) (1- index)))
      ((< index start) nil)
    (declare (fixnum index))
    (if (/= (logand (char-set-ref table (char-code (elt string index))) mask) 0)
        (return index))))

;;;
;;;    search-hash-code must be a function with the following
;;; properties: given any character it returns a number between 0 and
;;; 255, and the same hash code must be returned for the upper and
;;; lower case forms of each character.  In ASCII this is can be done
;;; by ANDing out the 5'th bit.
;;;
(defmacro search-hash-code (ch)
  `(logand (char-code ,ch) #x+DF))

;;; Doesn't do anything special, but it should fast and not waste any time
;;; checking type and whatnot.
(defmacro search-char-upcase (ch)
  `(char-upcase (the base-char ,ch)))



;;;; DO-ALPHA-CHARS.

;;; ALPHA-CHARS-LOOP loops from start-char through end-char binding var
;;; to the alphabetic characters and executing body.  Note that the manual
;;; guarantees lower and upper case char codes to be separately in order,
;;; but other characters may be interspersed within that ordering.
(defmacro alpha-chars-loop (var start-char end-char result body)
  (let ((n (gensym))
        (end-char-code (gensym)))
    `(do ((,n (char-code ,start-char) (1+ ,n))
          (,end-char-code (char-code ,end-char)))
         ((> ,n ,end-char-code) ,result)
       (let ((,var (code-char ,n)))
         (when (alpha-char-p ,var)
           ,@body)))))

(defmacro do-alpha-chars ((var kind &optional result) &rest forms)
  "(do-alpha-chars (var kind [result]) . body).  Kind is one of
   :lower, :upper, or :both, and var is bound to each character in
   order as specified under character relations in the manual.  When
   :both is specified, lowercase letters are processed first."
  ;; ### Hmm, I added iso-latin-1 characters here, but this gets eaten
  ;; by the ALPHA-CHAR-P in ALPHA-CHARS-LOOP. --GB 2004-11-20
  (case kind
    (:both
     `(progn
       (alpha-chars-loop ,var #\a #\z nil ,forms)
       (alpha-chars-loop ,var (code-char 223) (code-char 246) nil ,forms)
       (alpha-chars-loop ,var (code-char 248) (code-char 255) nil ,forms)
       (alpha-chars-loop ,var #\A #\Z nil ,forms)
       (alpha-chars-loop ,var (code-char 192) (code-char 214) nil ,forms)
       (alpha-chars-loop ,var (code-char 216) (code-char 222) ,result ,forms) ))
    (:lower
     `(progn
       (alpha-chars-loop ,var (code-char 223) (code-char 246) ,forms)
       (alpha-chars-loop ,var (code-char 248) nil ,forms)
       (alpha-chars-loop ,var #\a #\z ,result ,forms) ))
    (:upper
     `(progn
       (alpha-chars-loop ,var #\A #\Z nil ,forms)
       (alpha-chars-loop ,var (code-char 192) (code-char 214) nil ,forms)
       (alpha-chars-loop ,var (code-char 216) (code-char 222) ,result ,forms) ))
    (t (error "Kind argument not one of :lower, :upper, or :both -- ~S."
              kind))))
