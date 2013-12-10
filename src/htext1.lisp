;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Hemlock Text-Manipulation functions.
;;; Written by Skef Wholey.
;;;
;;; The code in this file implements the functions in the "Representation
;;; of Text," "Buffers," and "Predicates" chapters of the Hemlock design
;;; document.
;;;

(in-package :hemlock-internals)


;;;; Representation of Text:

;;; Line cache mechanism.
;;;
;;; The "open line" is used when inserting and deleting characters from a line.
;;; It acts as a cache that provides a more flexible (but more expensive)
;;; representation of the line for multiple insertions and deletions.  When a
;;; line is open, it is represented as a vector of characters and two indices:
;;;
;;; +-----------------------------------------------------------+
;;; | F | O | O |   | B | x | x | x | x | x | x | x | x | A | R |
;;; +-----------------------------------------------------------+
;;;                       ^                               ^
;;;                   Left Pointer                   Right Pointer
;;;
;;; The open line is represented by 4 special variables:
;;;     Open-Line: the line object that is opened
;;;     Open-Chars: the vector of cached characters
;;;     Left-Open-Pos: index of first free character in the gap
;;;     Right-Open-Pos: index of first used character after the gap
;;;
;;; Note:
;;;    Any modification of the line cache must be protected by
;;; Without-Interrupts.  This is done automatically by modifying-buffer; other
;;; users beware.

(defvar line-cache-length 200
  "Length of Open-Chars.")

(defvar open-line ()
  "Line open for hacking on.")

(defvar open-chars  (make-string line-cache-length)
  "Vector of characters for hacking on.")

(defvar left-open-pos 0
  "Index to first free character to left of mark in Open-Chars.")

(defvar right-open-pos 0
  "Index to first used character to right of mark in Open-Chars.")

(defun grow-open-chars (&optional (new-length (* line-cache-length 2)))
  "Grows Open-Chars to twice its current length, or the New-Length if
  specified."
  (let ((new-chars (make-string new-length))
        (new-right (- new-length (- line-cache-length right-open-pos))))
    (%sp-byte-blt open-chars 0 new-chars 0 left-open-pos)
    (%sp-byte-blt open-chars right-open-pos new-chars new-right new-length)
    (setq right-open-pos new-right)
    (setq open-chars new-chars)
    (setq line-cache-length new-length)))

(defun close-line ()
  "Stuffs the characters in the currently open line back into the line they
  came from, and sets open-line to Nil."
  (when open-line
    (hemlock-ext:without-interrupts
      (let* ((length (+ left-open-pos (- line-cache-length right-open-pos)))
             (string (make-string length)))
        (%sp-byte-blt open-chars 0 string 0 left-open-pos)
        (%sp-byte-blt open-chars right-open-pos string left-open-pos length)
        (setf (line-chars open-line) string)
        (setf open-line nil)))))

;;; We stick decrementing fixnums in the line-chars slot of the open line
;;; so that whenever the cache is changed the chars are no longer eq.
;;; They decrement so that they will be distinct from positive fixnums,
;;; which might mean something else.
;;;
(defvar *cache-modification-tick* -1
  "The counter for the fixnums we stick in the chars of the cached line.")

(defun open-line (line mark)
  "Closes the current Open-Line and opens the given Line at the Mark.
  Don't call this, use modifying-line instead."
  (cond ((eq line open-line)
         (let ((charpos (mark-charpos mark)))
           (cond ((< charpos left-open-pos)     ; BLT 'em right!
                  (let ((right-start (- right-open-pos
                                        (- left-open-pos charpos))))
                    (%sp-byte-blt open-chars charpos
                                  open-chars right-start
                                  right-open-pos)
                    (setq left-open-pos charpos)
                    (setq right-open-pos right-start)))
                 ((> charpos left-open-pos)     ; BLT 'em left!
                  (%sp-byte-blt open-chars right-open-pos
                                open-chars left-open-pos
                                charpos)
                  (setq right-open-pos
                        (+ right-open-pos (- charpos left-open-pos)))
                  (setq left-open-pos charpos)))))

        (t
         (close-line)
         (let* ((chars (line-chars line))
                (len (length chars)))
           (declare (simple-string chars))
           (when (> len line-cache-length)
             (setq line-cache-length (* len 2))
             (setq open-chars (make-string line-cache-length)))
           (setq open-line line)
           (setq left-open-pos (mark-charpos mark))
           (setq right-open-pos
                 (- line-cache-length (- (length chars) left-open-pos)))
           (%sp-byte-blt chars 0 open-chars 0 left-open-pos)
           (%sp-byte-blt chars left-open-pos open-chars right-open-pos
                         line-cache-length)))))

;;;; Some macros for Text hacking:


(defmacro modifying-line (line mark)
  "Checks to see if the Line is already opened at the Mark, and calls Open-Line
  if not.  Sticks a tick in the open-line's chars.  This must be called within
  the body of a Modifying-Buffer form."
  `(progn
    (unless (and (= (mark-charpos ,mark) left-open-pos) (eq ,line open-line))
      (open-line ,line ,mark))
    (setf (line-chars open-line) (decf *cache-modification-tick*))))

;;; Now-Tick tells us when now is and isn't.
;;;
(defvar now-tick 0 "Current tick.")

(defmacro tick ()
  "Increments the ``now'' tick."
  `(incf now-tick))


;;; Yeah, the following is kind of obscure, but at least it doesn't
;;; call Bufferp twice.  The without-interrupts is just to prevent
;;; people from being screwed by interrupting when the buffer structure
;;; is in an inconsistent state.
;;;
(defun invoke-modifying-buffer (fun buffer)
  "Does groovy stuff for modifying buffers."
  (progn
    (when (bufferp buffer)
      (unless (buffer-writable buffer)
        (editor-error "Buffer ~S is read only." (buffer-name buffer)))
      (when (< (buffer-modified-tick buffer)
               (buffer-unmodified-tick buffer))
        (invoke-hook hemlock::buffer-modified-hook buffer t))
      (setf (buffer-modified-tick buffer) (tick)))
    ;; FIXME: what is hemlock-ext:without-interrupts for?
    (hemlock-ext:without-interrupts (funcall fun))))

(defmacro modifying-buffer (buffer &body forms)
  "Does groovy stuff for modifying buffers."
  `(invoke-modifying-buffer (lambda () ,@forms) ,buffer))

(defmacro always-change-line (mark new-line)
  (let ((scan (gensym))
        (prev (gensym))
        (old-line (gensym)))
    `(let ((,old-line (mark-line ,mark)))
       (when (not (eq (mark-%kind ,mark) :temporary))
         (do ((,scan (line-marks ,old-line) (cdr ,scan))
              (,prev () ,scan))
             ((eq (car ,scan) ,mark)
              (if ,prev
                  (setf (cdr ,prev) (cdr ,scan))
                  (setf (line-marks ,old-line) (cdr ,scan)))
              (setf (cdr ,scan) (line-marks ,new-line)
                    (line-marks ,new-line) ,scan))))
       (setf (mark-line ,mark) ,new-line))))

(defmacro change-line (mark new-line)
  (let ((scan (gensym))
        (prev (gensym))
        (old-line (gensym)))
    `(let ((,old-line (mark-line ,mark)))
       (unless (or (eq (mark-%kind ,mark) :temporary)
                   (eq ,old-line ,new-line))
         (do ((,scan (line-marks ,old-line) (cdr ,scan))
              (,prev () ,scan))
             ((eq (car ,scan) ,mark)
              (if ,prev
                  (setf (cdr ,prev) (cdr ,scan))
                  (setf (line-marks ,old-line) (cdr ,scan)))
              (setf (cdr ,scan) (line-marks ,new-line)
                    (line-marks ,new-line) ,scan))))
       (setf (mark-line ,mark) ,new-line))))

;;; MOVE-SOME-MARKS  --  Internal
;;;
;;;    Move all the marks from the line Old to New, performing some
;;; function on their charpos'es.  Charpos is bound to the charpos of
;;; the mark, and the result of the evaluation of the last form in
;;; the body should be the new charpos for the mark.  If New is
;;; not supplied then the marks are left on the old line.
;;;
(defmacro move-some-marks ((charpos old &optional new) &body body)
  (let ((last (gensym)) (mark (gensym)) (marks (gensym)))
    (if new
        `(let ((,marks (line-marks ,old)))
           (do ((,mark ,marks (cdr ,mark))
                (,last nil ,mark))
               ((null ,mark)
                (when ,last
                  (shiftf (cdr ,last) (line-marks ,new) ,marks))
                (setf (line-marks ,old) nil))
             (setf (mark-line (car ,mark)) ,new)
             (setf (mark-charpos (car ,mark))
                   (let ((,charpos (mark-charpos (car ,mark))))
                     ,@body))))
        `(dolist (,mark (line-marks ,old))
           (setf (mark-charpos ,mark)
                 (let ((,charpos (mark-charpos ,mark)))
                   ,@body))))))

;;; Maybe-Move-Some-Marks  --  Internal
;;;
;;;    Like Move-Some-Marks, but only moves the mark if the
;;; charpos is greater than the bound, OR the charpos equals the bound
;;; and the marks %kind is :left-inserting.
;;;
(defmacro maybe-move-some-marks ((charpos old &optional new) bound &body body)
  (let ((mark (gensym)) (marks (gensym)) (prev (gensym)))
    (if new
        `(do ((,mark (line-marks ,old))
              (,marks (line-marks ,new))
              (,prev ()))
             ((null ,mark)
              (setf (line-marks ,new) ,marks))
           (let ((,charpos (mark-charpos (car ,mark))))
             (cond
               ((or (> ,charpos ,bound)
                    (and (= ,charpos ,bound)
                         (eq (mark-%kind (car ,mark)) :left-inserting)))
                (setf (mark-line (car ,mark)) ,new)
                (setf (mark-charpos (car ,mark)) (progn ,@body))
                (if ,prev
                    (setf (cdr ,prev) (cdr ,mark))
                    (setf (line-marks ,old) (cdr ,mark)))
                (rotatef (cdr ,mark) ,marks ,mark))
               (t
                (setq ,prev ,mark  ,mark (cdr ,mark))))))
        `(dolist (,mark (line-marks ,old))
           (let ((,charpos (mark-charpos ,mark)))
             (when (or (> ,charpos ,bound)
                       (and (= ,charpos ,bound)
                            (eq (mark-%kind ,mark) :left-inserting)))
               (setf (mark-charpos ,mark) (progn ,@body))))))))


;;; Maybe-Move-Some-Marks*  --  Internal
;;;
;;;    Like Maybe-Move-Some-Marks, but ignores the mark %kind.
;;;
(defmacro maybe-move-some-marks* ((charpos old &optional new) bound &body body)
  (let ((mark (gensym)) (marks (gensym)) (prev (gensym)))
    (if new
        `(do ((,mark (line-marks ,old))
              (,marks (line-marks ,new))
              (,prev ()))
             ((null ,mark)
              (setf (line-marks ,new) ,marks))
           (let ((,charpos (mark-charpos (car ,mark))))
             (cond
               ((> ,charpos ,bound)
                (setf (mark-line (car ,mark)) ,new)
                (setf (mark-charpos (car ,mark)) (progn ,@body))
                (if ,prev
                    (setf (cdr ,prev) (cdr ,mark))
                    (setf (line-marks ,old) (cdr ,mark)))
                (rotatef (cdr ,mark) ,marks ,mark))
               (t
                (setq ,prev ,mark  ,mark (cdr ,mark))))))
        `(dolist (,mark (line-marks ,old))
           (let ((,charpos (mark-charpos ,mark)))
             (when (> ,charpos ,bound)
               (setf (mark-charpos ,mark) (progn ,@body))))))))

;;;; Lines.

(defun line-length (line)
  "Returns the number of characters on the line."
  (if (linep line)
      (line-length* line)
      (error "~S is not a line!" line)))

(defun line-buffer (line)
  "Returns the buffer with which the Line is associated.  If the line is
  not in any buffer then Nil is returned."
  (let ((buffer (line-%buffer line)))
    (if (bufferp buffer) buffer)))

(defun line-string (line)
  "Returns the characters in the line as a string.  The resulting string
  must not be destructively modified.  This may be set with Setf."
  (if (eq line open-line)
      (close-line))
  (line-chars line))

(defun (setf line-string) (string line)
  "Replace the contents of a line."
  (let ((buffer (line-%buffer line)))
    (modifying-buffer buffer
      (unless (simple-string-p string)
        (setq string (coerce string 'simple-string)))
      (when (eq line open-line) (setq open-line nil))
      (let ((length (length (the simple-string string))))
        (dolist (m (line-marks line))
          (if (eq (mark-%kind m) :left-inserting)
              (setf (mark-charpos m) length)
              (setf (mark-charpos m) 0))))
      (setf (line-chars line) string))))

(defun line-character (line index)
  "Return the Index'th character in Line.  If the index is the length of the
  line then #\newline is returned."
  (if (eq line open-line)
      (if (< index left-open-pos)
          (schar open-chars index)
          (let ((index (+ index (- right-open-pos left-open-pos))))
            (if (= index line-cache-length)
                #\newline
                (schar open-chars index))))
      (let ((chars (line-chars line)))
        (declare (simple-string chars))
        (if (= index (length chars))
            #\newline
            (schar chars index)))))

;;;; Marks.

(defun mark (line charpos &optional (kind :temporary))
  "Returns a mark to the Charpos'th character of the Line.  Kind is the
  kind of mark to make, one of :temporary (the default), :left-inserting
  or :right-inserting."
  (let ((mark (internal-make-mark line charpos kind)))
    (if (not (eq kind :temporary))
        (push mark (line-marks line)))
    mark))

(defun mark-kind (mark)
  "Returns the kind of the given Mark, :Temporary, :Left-Inserting, or
  :Right-Inserting.  This may be set with Setf."
  (mark-%kind mark))

(defun (setf mark-kind) (kind mark)
  "Used to set the kind of a mark."
  (let ((line (mark-line mark)))
    (cond ((eq kind :temporary)
           (setf (line-marks line) (delq mark (line-marks line)))
           (setf (mark-%kind mark) kind))
          ((or (eq kind :left-inserting) (eq kind :right-inserting))
           (if (not (member mark (line-marks line)))
               (push mark (line-marks line)))
           (setf (mark-%kind mark) kind))
          (t
           (error "~S is an invalid mark type." kind)))))

(defun copy-mark (mark &optional (kind (mark-%kind mark)))
  "Returns a new mark pointing to the same position as Mark.  The kind
  of mark created may be specified by Kind, which defaults to the
  kind of the copied mark."
  (let ((mark (internal-make-mark (mark-line mark) (mark-charpos mark) kind)))
    (if (not (eq kind :temporary))
        (push mark (line-marks (mark-line mark))))
    mark))

(defun delete-mark (mark)
  "Deletes the Mark.  This should be done to any mark that may not be
  temporary which is no longer needed."
  (if (not (eq (mark-%kind mark) :temporary))
      (let ((line (mark-line mark)))
        (when line
          (setf (line-marks line) (delq mark (line-marks line))))
        nil))
  (setf (mark-line mark) nil))

(defun move-to-position (mark charpos &optional (line (mark-line mark)))
  "Changes the Mark to point to the given character position on the Line,
  which defaults to the line the mark is currently on."
  (change-line mark line)
  (setf (mark-charpos mark) charpos)
  mark)

;;;; Regions.

(defun region (start end)
  "Returns a region constructed from the marks Start and End."
  (let ((l1 (mark-line start))
        (l2 (mark-line end)))
    (unless (eq (line-%buffer l1) (line-%buffer l2))
      (error "Can't make a region with lines of different buffers."))
    (unless (if (eq l1 l2)
                (<= (mark-charpos start) (mark-charpos end))
                (< (line-number l1) (line-number l2)))
      (error "Start ~S is after end ~S." start end)))
  (internal-make-region start end))

;;; The *Disembodied-Buffer-Counter* exists to give that are not in any buffer
;;; unique buffer slots.

(defvar *disembodied-buffer-counter* 0
  "``Buffer'' given to lines in regions not in any buffer.")

(defun make-empty-region ()
  "Returns a region with start and end marks pointing to the start of one empty
  line.  The start mark is right-inserting and the end mark is left-inserting."
  (let* ((line (make-line :chars ""  :number 0
                          :%buffer (incf *disembodied-buffer-counter*)))
         (start (mark line 0 :right-inserting))
         (end (mark line 0 :left-inserting)))
    (internal-make-region start end)))

;;; Line-Increment is the default difference for line numbers when we don't
;;; know any better.

(defconstant line-increment 256 "Default difference for line numbers.")

;;; Renumber-Region is used internally to keep line numbers in ascending order.
;;; The lines in the region are numbered starting with the given Start value
;;; by increments of the given Step value.  It returns the region.

(defun renumber-region (region &optional (start 0) (step line-increment))
  (do ((line (mark-line (region-start region)) (line-next line))
       (last-line (mark-line (region-end region)))
       (number start (+ number step)))
      ((eq line last-line)
       (setf (line-number line) number)
       region)
    (setf (line-number line) number))
  region)

;;; Renumber-Region-Containing renumbers the region containing the given line.

(defun renumber-region-containing (line)
  (cond ((line-buffer line)
         (renumber-region (buffer-region (line-%buffer line))))
        (t
         (do ((line line (line-previous line))
              (number 0 (- number line-increment)))
             ((null line))
           (setf (line-number line) number))
         (do ((line (line-next line) (line-next line))
              (number line-increment (+ number line-increment)))
             ((null line))
           (setf (line-number line) number)))))


;;; Number-Line numbers a newly created line.  The line has to have a previous
;;; line.
(defun number-line (line)
  (let ((prev (line-number (line-previous line)))
        (next (line-next line)))
    (if (null next)
        (setf (line-number line) (+ prev line-increment))
        (let ((new (+ prev (truncate (- (line-number next) prev) 2))))
          (if (= new prev)
              (renumber-region-containing line)
              (setf (line-number line) new))))))



;;;; Buffers.

;;; BUFFER-SIGNATURE is the exported interface to the internal function,
;;; BUFFER-MODIFIED-TICK
;;;
(defun buffer-signature (buffer)
  "Returns an arbitrary number which reflects the buffers current
  \"signature.\" The value returned by buffer-signature is guaranteed
  to be eql to the value returned by a previous call of buffer-signature
  iff the buffer has not been modified between the calls."
  (unless (bufferp buffer)
    (error "~S is not a buffer." buffer))
  (buffer-modified-tick buffer))



;;;; Predicates:


(defun start-line-p (mark)
  "Returns T if the Mark points before the first character in a line, Nil
  otherwise."
  (= (mark-charpos mark) 0))

(defun end-line-p (mark)
  "Returns T if the Mark points after the last character in a line, Nil
  otherwise."
  (= (mark-charpos mark) (line-length (mark-line mark))))

(defun empty-line-p (mark)
  "Returns T if the line pointer to by Mark contains no characters, Nil
  or otherwise."
  (let ((line (mark-line mark)))
    (if (eq line open-line)
        (and (= left-open-pos 0) (= right-open-pos line-cache-length))
        (= (length (line-chars line)) 0))))

;;; blank-between-positions  --  Internal
;;;
;;;    Check if a line is blank between two positions.  Used by blank-XXX-p.
;;;
(eval-when (:compile-toplevel :execute)
(defmacro check-range (chars start end)
  `(do ((i ,start (1+ i)))
       ((= i ,end) t)
     (when (zerop (character-attribute :whitespace (schar ,chars i)))
       (return nil)))))
;;;
(defun blank-between-positions (line start end)
  (if (eq line open-line)
      (let ((gap (- right-open-pos left-open-pos)))
        (cond ((>= start left-open-pos)
               (check-range open-chars (+ start gap) (+ end gap)))
              ((<= end left-open-pos)
               (check-range open-chars start end))
              (t
               (and (check-range open-chars start left-open-pos)
                    (check-range open-chars right-open-pos (+ end gap))))))
      (let ((chars (line-chars line)))
        (check-range chars start end))))

(defun blank-line-p (line)
  "True if line contains only characters with a :whitespace attribute of 1."
  (blank-between-positions line 0 (line-length line)))

(defun blank-before-p (mark)
  "True is all of the characters before Mark on the line it is on have a
  :whitespace attribute of 1."
  (blank-between-positions (mark-line mark) 0 (mark-charpos mark)))

(defun blank-after-p (mark)
  "True if all characters on the part part of the line after Mark have
  a :whitespace attribute of 1."
  (let ((line (mark-line mark)))
    (blank-between-positions line (mark-charpos mark)
                             (line-length line))))

(defun same-line-p (mark1 mark2)
  "Returns T if Mark1 and Mark2 point to the same line, Nil otherwise."
  (eq (mark-line mark1) (mark-line mark2)))

(defun mark< (mark1 mark2)
  "Returns T if Mark1 points to a character before Mark2, Nil otherwise."
  (if (not (eq (line-%buffer (mark-line mark1))
               (line-%buffer (mark-line mark2))))
      (error "Marks in different buffers have no relation."))
  (or (< (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
           (< (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark<= (mark1 mark2)
  "Returns T if Mark1 points to a character at or before Mark2, Nil otherwise."
  (if (not (eq (line-%buffer (mark-line mark1))
               (line-%buffer (mark-line mark2))))
      (error "Marks in different buffers have no relation."))
  (or (< (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
           (<= (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark> (mark1 mark2)
  "Returns T if Mark1 points to a character after Mark2, Nil otherwise."
  (if (not (eq (line-%buffer (mark-line mark1))
               (line-%buffer (mark-line mark2))))
      (error "Marks in different buffers have no relation."))
  (or (> (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
           (> (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark>= (mark1 mark2)
  "Returns T if Mark1 points to a character at or after Mark2, Nil otherwise."
  (if (not (eq (line-%buffer (mark-line mark1))
               (line-%buffer (mark-line mark2))))
      (error "Marks in different buffers have no relation."))
  (or (> (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
           (>= (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark= (mark1 mark2)
  "Returns T if both marks point to the same position, Nil otherwise."
  (and (eq (mark-line mark1) (mark-line mark2))
       (= (mark-charpos mark1) (mark-charpos mark2))))

(defun mark/= (mark1 mark2)
  "Returns T if both marks point to different positions, Nil otherwise."
  (not (and (eq (mark-line mark1) (mark-line mark2))
            (= (mark-charpos mark1) (mark-charpos mark2)))))

(defun line< (line1 line2)
  "Returns T if Line1 comes before Line2, NIL otherwise."
  (if (neq (line-%buffer line1) (line-%buffer line2))
      (error "Lines in different buffers have no relation."))
  (< (line-number line1) (line-number line2)))

(defun line<= (line1 line2)
  "Returns T if Line1 comes before or is the same as Line2, NIL otherwise."
  (if (neq (line-%buffer line1) (line-%buffer line2))
      (error "Lines in different buffers have no relation."))
  (<= (line-number line1) (line-number line2)))

(defun line>= (line1 line2)
  "Returns T if Line1 comes after or is the same as Line2, NIL otherwise."
  (if (neq (line-%buffer line1) (line-%buffer line2))
      (error "Lines in different buffers have no relation."))
  (>= (line-number line1) (line-number line2)))

(defun line> (line1 line2)
  "Returns T if Line1 comes after Line2, NIL otherwise."
  (if (neq (line-%buffer line1) (line-%buffer line2))
      (error "Lines in different buffers have no relation."))
  (> (line-number line1) (line-number line2)))

(defun lines-related (line1 line2)
  "Returns T if an order relation exists between Line1 and Line2."
  (eq (line-%buffer line1) (line-%buffer line2)))

(defun first-line-p (mark)
  "Returns T if the line pointed to by mark has no previous line,
  Nil otherwise."
  (null (line-previous (mark-line mark))))

(defun last-line-p (mark)
  "Returns T if the line pointed to by mark has no next line,
  Nil otherwise."
  (null (line-next (mark-line mark))))
