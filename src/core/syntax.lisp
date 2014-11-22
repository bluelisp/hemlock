;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Hemlock syntax table routines.
;;;
;;; Written by Rob MacLachlan.
;;;

(in-package :hemlock-internals)


;;;; Character attribute caching.
;;;
;;;    In order to permit the %SP-Find-Character-With-Attribute sub-primitive
;;; to be used for a fast implementation of find-attribute and
;;; reverse-find-attribute, there must be some way of translating
;;; attribute/test-function pairs into a attribute 'character-set and a mask.
;;;    What we do is maintain a eq-hash-cache of attribute/test-function
;;; pairs.  If the desired pair is not in the cache then we reclaim an old
;;; attribute bit in the bucket we hashed to and stuff it by calling the
;;; test function on the value of the attribute for all characters.

(defvar *character-attribute-cache* ()
  "This is the cache used to translate attribute/test-function pairs to
  attribute character-set/mask pairs for find-attribute and reverse-find-attribute.")

(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant character-attribute-cache-size 13
  "The number of buckets in the *character-attribute-cache*.")
(defconstant character-attribute-bucket-size 3
  "The number of bits to use in each bucket of the
  *character-attribute-cache*.")
); eval-when (:compile-toplevel :execute :load-toplevel)

;;;    In addition, since a common pattern in code which uses find-attribute
;;; is to repeatedly call it with the same function and attribute, we
;;; remember the last attribute/test-function pair that was used, and check
;;; if it is the same pair beforehand, thus often avoiding the hastable lookup.
;;;
(defvar *last-find-attribute-attribute* ()
  "The attribute which we last did a find-attribute on.")
(defvar *last-find-attribute-function* ()
  "The last test-function used for find-attribute.")
(defvar *last-find-attribute-char-set* ()
  "The %SP-Find-Character-With-Attribute character-set corresponding to the last
  attribute/function pair used for find-attribute.")
(defvar *last-find-attribute-mask* ()
  "The the mask to use with *last-find-attribute-char-set* to do a search
  for the last attribute/test-function pair.")
(defvar *last-find-attribute-end-wins* ()
  "The the value of End-Wins for the last attribute/test-function pair.")


(defvar *character-attributes* (make-hash-table :test #'eq)
  "A hash table which translates character attributes to their values.")
(defvar *last-character-attribute-requested* nil
  "The last character attribute which was asked for, Do Not Bind.")
(defvar *value-of-last-character-attribute-requested* nil
  "The value of the most recent character attribute, Do Not Bind.")

(declaim (special *character-attribute-names*))


;;; Each bucket contains a list of character-attribute-bucket-size
;;; bit-descriptors.
;;;
(defstruct (bit-descriptor)
  function                    ; The test on the attribute.
  attribute                   ; The attribute this is a test of.
  (mask 0 :type fixnum)       ; The mask for the corresponding bit.
  char-set                    ; The 'character-set the bit is in.
  end-wins)                   ; Is this test true of buffer ends?

;;;
;;; In a descriptor for an unused bit, the function is nil, preventing a
;;; hit.  Whenever we change the value of an attribute for some character,
;;; we need to flush the cache of any entries for that attribute.  Currently
;;; we do this by mapping down the list of all bit descriptors.  Note that
;;; we don't have to worry about GC, since this is just a hint.
;;;
(defvar *all-bit-descriptors* () "The list of all the bit descriptors.")

(eval-when (:compile-toplevel :execute)
(defmacro allocate-bit (char-set bit-num)
  `(progn
    (when (= ,bit-num 8)
      (setf ,bit-num 0)
      (setf ,char-set (make-character-set
                       :page0 (make-array 256 :element-type '(mod 256))
                       :table (make-hash-table)
                       :default 0)))
    (car (push (make-bit-descriptor
                :char-set ,char-set
                :mask (ash 1 (prog1 ,bit-num (incf ,bit-num))))
               *all-bit-descriptors*)))))
;;;
(defun %init-syntax-table ()
  (let ((tab (make-array character-attribute-cache-size))
        (bit-num 8)
        char-set)
    (setq *character-attribute-cache* tab)
    (dotimes (c character-attribute-cache-size)
      (setf (svref tab c)
            (do ((i 0 (1+ i))
                 (res ()))
                ((= i character-attribute-bucket-size) res)
              (push (allocate-bit char-set bit-num) res))))))

(eval-when (:compile-toplevel :execute)

(defmacro hash-it (attribute function)
  `(abs (rem (logxor (ash (sxhash ,attribute) -3)
                     (sxhash ,function))
             character-attribute-cache-size)))

;;; CACHED-ATTRIBUTE-LOOKUP  --  Internal
;;;
;;;    Sets 'char-set and 'mask such that they can be used as arguments
;;; to %sp-find-character-with-attribute to effect a search with attribute
;;; Attribute and test Function.  If the function and attribute
;;; are the same as the last ones then we just set them to that, otherwise
;;; we do the hash-cache lookup and update the *last-find-attribute-<mumble>*
;;;
(defmacro cached-attribute-lookup (attribute function char-set mask end-wins)
  `(if (and (eq ,function *last-find-attribute-function*)
            (eq ,attribute *last-find-attribute-attribute*))
       (setq ,char-set *last-find-attribute-char-set*
             ,mask *last-find-attribute-mask*
             ,end-wins *last-find-attribute-end-wins*)
       (let ((bit (svref *character-attribute-cache*
                         (hash-it ,attribute ,function))))
         ,(do ((res `(multiple-value-setq (,char-set ,mask ,end-wins)
                       (new-cache-attribute ,attribute ,function))
                    `(let ((b (car bit)))
                       (cond
                        ((and (eq (bit-descriptor-function b)
                                  ,function)
                              (eq (bit-descriptor-attribute b)
                                  ,attribute))
                         (setq ,char-set (bit-descriptor-char-set b)
                               ,mask (bit-descriptor-mask b)
                               ,end-wins (bit-descriptor-end-wins b)))
                        (t
                         (setq bit (cdr bit)) ,res))))
               (count 0 (1+ count)))
              ((= count character-attribute-bucket-size) res))
         (setq *last-find-attribute-attribute* ,attribute
               *last-find-attribute-function* ,function
               *last-find-attribute-char-set* ,char-set
               *last-find-attribute-mask* ,mask
               *last-find-attribute-end-wins* ,end-wins))))
); eval-when (:compile-toplevel :execute)

;;; NEW-CACHE-ATTRIBUTE  --  Internal
;;;
;;;    Pick out an old attribute to punt out of the cache and put in the
;;; new one.  We pick a bit off of the end of the bucket and pull it around
;;; to the beginning to get a degree of LRU'ness.
;;;
(defun new-cache-attribute (attribute function)
  (let* ((hash (hash-it attribute function))
         (values (or (gethash attribute *character-attributes*)
                     (error "~S is not a defined character attribute."
                            attribute)))
         (bucket (svref *character-attribute-cache* hash))
         (bit (nthcdr (- character-attribute-bucket-size 2) bucket))
         (end-wins (funcall function (attribute-descriptor-end-value values))))
    (shiftf bit (cdr bit) nil)
    (setf (svref *character-attribute-cache* hash) bit
          (cdr bit) bucket  bit (car bit))
    (setf (bit-descriptor-attribute bit) attribute
          (bit-descriptor-function bit) function
          (bit-descriptor-end-wins bit) end-wins)
    (let ((char-set (attribute-descriptor-char-set values))
          (mask (bit-descriptor-mask bit))
          (fun (bit-descriptor-function bit))
          (cs (bit-descriptor-char-set bit)))
      (declare (type character-set char-set cs))
      (do ((i 0 (1+ i)))
          ((= i 256))
        (if (funcall fun (char-set-ref char-set i))
            (setf (char-set-ref cs i) (logior (char-set-ref cs i) mask))
            (setf (char-set-ref cs i) (logandc2 (char-set-ref cs i) mask))))
      (with-hash-table-iterator (next (character-set-table cs))
        (loop
           (multiple-value-bind (more code value)
               (next)
             (unless more
               (return))
             (if (funcall fun value)
                 (setf (char-set-ref cs code) (logior (char-set-ref cs code) mask))
                 (setf (char-set-ref cs code) (logandc2 (char-set-ref cs code) mask))))))
      (let ((default (character-set-default char-set)))
        (if (funcall fun default)
            (setf (character-set-default cs)
                  (logior (character-set-default cs) mask))
            (setf (character-set-default cs)
                  (logandc2 (character-set-default cs) mask))))
      (values cs mask end-wins))))


(defun %print-attribute-descriptor (object stream depth)
  (declare (ignore depth))
  (format stream "#<Hemlock Attribute-Descriptor ~S>"
          (attribute-descriptor-name object)))

;;; DEFATTRIBUTE  --  Public
;;;
;;;    Make a new char-set of some type and enter it in the table.
;;;
(defun defattribute (name documentation &optional (type '(mod 2))
                          (initial-value 0))
  "Define a new Hemlock character attribute with named Name with
  the supplied Documentation, Type and Initial-Value.  Type
  defaults to (mod 2) and Initial-Value defaults to 0."
  (setq name (coerce name 'simple-string))
  (let* ((attribute (string-to-keyword name))
         (new (make-attribute-descriptor
               :char-set (make-character-set
                        :page0 (make-array 256
                                           :element-type type
                                           :initial-element initial-value)
                        :table (make-hash-table)
                        :default initial-value)
               :name name
               :keyword attribute
               :documentation documentation
               :end-value initial-value)))
    (when (gethash attribute *character-attributes*)
      (warn "Character Attribute ~S is being redefined." name))
    (setf (getstring name *character-attribute-names*) attribute)
    (setf (gethash attribute *character-attributes*) new))
  name)

;;; WITH-ATTRIBUTE  --  Internal
;;;
;;;    Bind obj to the attribute descriptor corresponding to symbol,
;;; giving error if it is not a defined attribute.
;;;
(eval-when (:compile-toplevel :execute)
(defmacro with-attribute (symbol &body forms)
  `(let ((obj (gethash ,symbol *character-attributes*)))
     (unless obj
       (error "~S is not a defined character attribute." ,symbol))
     ,@forms))
); eval-when (:compile-toplevel :execute)

(defun character-attribute-name (attribute)
  "Return the string-name of the character-attribute Attribute."
  (with-attribute attribute
    (attribute-descriptor-name obj)))

(defun character-attribute-documentation (attribute)
  "Return the documentation for the character-attribute Attribute."
  (with-attribute attribute
    (attribute-descriptor-documentation obj)))

(defun character-attribute-hooks (attribute)
  "Return the hook-list for the character-attribute Attribute.  This can
  be set with Setf."
  (with-attribute attribute
    (attribute-descriptor-hooks obj)))

(defun (setf character-attribute-hooks) (new-value attribute)
  "Set the hook list for a Hemlock character attribute."
  (with-attribute attribute
    (setf (attribute-descriptor-hooks obj) new-value)))

(declaim (special *last-character-attribute-requested*
                    *value-of-last-character-attribute-requested*))

;;; CHARACTER-ATTRIBUTE  --  Public
;;;
;;;    Return the value of a character attribute for some character.
;;;
(declaim (inline character-attribute))
(defun character-attribute (attribute character)
  "Return the value of the the character-attribute Attribute for Character.
  If Character is Nil then return the end-value."
  (if (and (eq attribute *last-character-attribute-requested*) character)
      (char-set-ref *value-of-last-character-attribute-requested*
                    (char-code character))
      (sub-character-attribute attribute character)))
;;;
(defun sub-character-attribute (attribute character)
  (with-attribute attribute
    (setq *last-character-attribute-requested* attribute)
    (let ((char-set (attribute-descriptor-char-set obj)))
      (setq *value-of-last-character-attribute-requested* char-set)
      (if character
          (char-set-ref char-set (char-code character))
          (attribute-descriptor-end-value obj)))))

;;; CHARACTER-ATTRIBUTE-P
;;;
;;;    Look up attribute in table.
;;;
(defun character-attribute-p (symbol)
  "Return true if Symbol is the symbol-name of a character-attribute, Nil
  otherwise."
  (not (null (gethash symbol *character-attributes*))))


;;; (SETF CHARACTER-ATTRIBUTE)  --  Internal
;;;
;;;    Set the value of a character attribute.
;;;
(defun (setf character-attribute) (new-value attribute character)
  "Set the value for a character attribute."
  (with-attribute attribute
    (invoke-hook hemlock::character-attribute-hook attribute character new-value)
    (invoke-hook (attribute-descriptor-hooks obj) attribute character new-value)
    (cond
     ;;
     ;; Setting the value for a real character.
     (character
      (let ((char-set (attribute-descriptor-char-set obj))
            (code (char-code character)))
        (declare (type character-set char-set))
        (dolist (bit *all-bit-descriptors*)
          (when (eq (bit-descriptor-attribute bit) attribute)
            (let ((cs (bit-descriptor-char-set bit)))
              (declare (type character-set cs))
              (setf (char-set-ref cs code)
                    (if (funcall (bit-descriptor-function bit) new-value)
                        (logior (bit-descriptor-mask bit)
                                (char-set-ref cs code))
                        (logandc1 (bit-descriptor-mask bit)
                                  (char-set-ref cs code)))))))
        (setf (char-set-ref char-set code) new-value)))
     ;;
     ;; Setting the magical end-value.
     (t
      (setf (attribute-descriptor-end-value obj) new-value)
      (dolist (bit *all-bit-descriptors*)
        (when (eq (bit-descriptor-attribute bit) attribute)
          (setf (bit-descriptor-end-wins bit)
                (funcall (bit-descriptor-function bit) new-value))))
      new-value))))

(eval-when (:compile-toplevel :execute)
;;; swap-one-attribute  --  Internal
;;;
;;;    Install the mode-local values described by Vals for Attribute, whose
;;; representation char-set is Value.
;;;
 (defmacro swap-one-attribute (attribute value vals hooks)
  `(progn
    ;; Fix up any cached attribute char-sets.
    (dolist (bit *all-bit-descriptors*)
      (when (eq ,attribute (bit-descriptor-attribute bit))
        (let ((fun (bit-descriptor-function bit))
              (cs (bit-descriptor-char-set bit))
              (mask (bit-descriptor-mask bit)))
          (declare (type character-set cs)
                   (fixnum mask))
          (dolist (char ,vals)
            (setf (char-set-ref cs (car char))
                  (if (funcall fun (cdr char))
                      (logior mask (char-set-ref cs (car char)))
                      (logandc1 mask (char-set-ref cs (car char)))))))))
    ;; Invoke the attribute-hook.
    (dolist (hook ,hooks)
      (dolist (char ,vals)
        (funcall hook ,attribute (code-char (car char)) (cdr char))))
    ;; Fix up the value char-set.
    (dolist (char ,vals)
      (rotatef (char-set-ref ,value (car char)) (cdr char)))))
); eval-when (:compile-toplevel :execute)


;;; SWAP-CHAR-ATTRIBUTES  --  Internal
;;;
;;;    Swap the current values of character attributes and the ones
;;;specified by "mode".  This is used in Set-Major-Mode.
;;;
(defun swap-char-attributes (mode)
  (dolist (attribute (mode-object-character-attributes mode))
    (let* ((obj (car attribute))
           (sym (attribute-descriptor-keyword obj))
           (value (attribute-descriptor-char-set obj))
           (hooks (attribute-descriptor-hooks obj)))
      (declare (type character-set value))
      (swap-one-attribute sym value (cdr attribute) hooks))))



(declaim (special *mode-names* *current-buffer*))

;;; SHADOW-ATTRIBUTE  --  Public
;;;
;;;    Stick mode character attribute information in the mode object.
;;;
(defun shadow-attribute (attribute character value mode)
  "Make a mode specific character attribute value.  The value of
  Attribute for Character when we are in Mode will be Value."
  (let ((desc (gethash attribute *character-attributes*))
        (obj (getstring mode *mode-names*)))
    (unless desc
      (error "~S is not a defined Character Attribute." attribute))
    (unless obj (error "~S is not a defined Mode." mode))
    (let* ((current (assoc desc (mode-object-character-attributes obj)))
           (code (char-code character))
           (hooks (attribute-descriptor-hooks desc))
           (char-set (attribute-descriptor-char-set desc))
           (cons (cons code value)))
      (declare (type character-set char-set))
      (if current
          (let ((old (assoc code (cdr current))))
            (if old
                (setf (cdr old) value  cons old)
                (push cons (cdr current))))
          (push (list desc cons)
                (mode-object-character-attributes obj)))
      (when (member obj (buffer-mode-objects *current-buffer*))
        (let ((vals (list cons)))
          (swap-one-attribute attribute char-set vals hooks)))
      (invoke-hook hemlock::shadow-attribute-hook attribute character value mode)))
  attribute)

;;; UNSHADOW-ATTRIBUTE  --  Public
;;;
;;;    Nuke a mode character attribute.
;;;
(defun unshadow-attribute (attribute character mode)
  "Make the value of Attribte for Character no longer shadowed in Mode."
  (let ((desc (gethash attribute *character-attributes*))
        (obj (getstring mode *mode-names*)))
    (unless desc
      (error "~S is not a defined Character Attribute." attribute))
    (unless obj
      (error "~S is not a defined Mode." mode))
    (invoke-hook hemlock::shadow-attribute-hook mode attribute character)
    (let* ((value (attribute-descriptor-char-set desc))
           (hooks (attribute-descriptor-hooks desc))
           (current (assoc desc (mode-object-character-attributes obj)))
           (char (assoc (char-code character) (cdr current))))
      (declare (type character-set value))
      (unless char
        (error "Character Attribute ~S is not defined for character ~S ~
               in Mode ~S." attribute character mode))
      (when (member obj (buffer-mode-objects *current-buffer*))
        (let ((vals (list char)))
          (swap-one-attribute attribute value vals hooks)))
      (setf (cdr current) (delete char (the list (cdr current))))))
  attribute)


;;; NOT-ZEROP, the default test function for find-attribute etc.
;;;
(defun not-zerop (n)
  (not (zerop n)))

;;; find-attribute  --  Public
;;;
;;;    Do hairy cache lookup to find a find-character-with-attribute style
;;; char-set that we can use to do the search.
;;;
(eval-when (:compile-toplevel :execute)
(defmacro normal-find-attribute (line start result char-set mask)
  `(let ((chars (line-chars ,line)))
     (setq ,result (%sp-find-character-with-attribute
                   chars ,start (strlen chars) ,char-set ,mask))))
;;;
(defmacro cache-find-attribute (start result char-set mask)
  `(let ((gap (- right-open-pos left-open-pos)))
     (declare (fixnum gap))
     (cond
      ((>= ,start left-open-pos)
       (setq ,result
             (%sp-find-character-with-attribute
              open-chars (+ ,start gap) line-cache-length ,char-set ,mask))
       (when ,result (decf ,result gap)))
      ((setq ,result (%sp-find-character-with-attribute
                      open-chars ,start left-open-pos ,char-set ,mask)))
      (t
       (setq ,result
             (%sp-find-character-with-attribute
              open-chars right-open-pos line-cache-length ,char-set ,mask))
       (when ,result (decf ,result gap))))))
); eval-when (:compile-toplevel :execute)
;;;
(defun find-attribute (mark attribute &optional (test #'not-zerop))
  "Find the next character whose attribute value satisfies test."
  (let ((charpos (mark-charpos mark))
        (line (mark-line mark))
        (mask 0)
        char-set end-wins)
    (declare (type (or character-set null) char-set)
             (fixnum mask)
             (type (or fixnum null) charpos))
    (cached-attribute-lookup attribute test char-set mask end-wins)
    (cond
     ((cond
       ((eq line open-line)
        (when (cache-find-attribute charpos charpos char-set mask)
          (setf (mark-charpos mark) charpos) mark))
       (t
        (when (normal-find-attribute line charpos charpos char-set mask)
          (setf (mark-charpos mark) charpos) mark))))
     ;; Newlines win and there is one.
     ((and (not (zerop (logand mask (char-set-ref char-set (char-code #\newline)))))
           (line-next line))
      (move-to-position mark (line-length line) line))
     ;; We can ignore newlines.
     (t
      (do (prev)
          (())
        (setq prev line  line (line-next line))
        (cond
         ((null line)
          (if end-wins
              (return (line-end mark prev))
              (return nil)))
         ((eq line open-line)
          (when (cache-find-attribute 0 charpos char-set mask)
            (return (move-to-position mark charpos line))))
         (t
          (when (normal-find-attribute line 0 charpos char-set mask)
            (return (move-to-position mark charpos line))))))))))


;;; REVERSE-FIND-ATTRIBUTE  --  Public
;;;
;;;    Line find-attribute, only goes backwards.
;;;
(eval-when (:compile-toplevel :execute)
(defmacro rev-normal-find-attribute (line start result char-set mask)
  `(let ((chars (line-chars ,line)))
     (setq ,result (%sp-reverse-find-character-with-attribute
                    chars 0 ,(or start '(strlen chars)) ,char-set ,mask))))
;;;
(defmacro rev-cache-find-attribute (start result char-set mask)
  `(let ((gap (- right-open-pos left-open-pos)))
     (declare (fixnum gap))
     (cond
      ,@(when start
          `(((<= ,start left-open-pos)
             (setq ,result
                   (%sp-reverse-find-character-with-attribute
                    open-chars 0 ,start ,char-set ,mask)))))
      ((setq ,result (%sp-reverse-find-character-with-attribute
                      open-chars right-open-pos
                      ,(if start `(+ ,start gap) 'line-cache-length)
                      ,char-set ,mask))
       (decf ,result gap))
      (t
       (setq ,result
             (%sp-reverse-find-character-with-attribute
              open-chars 0 left-open-pos ,char-set ,mask))))))

); eval-when (:compile-toplevel :execute)
;;;
(defun reverse-find-attribute (mark attribute &optional (test #'not-zerop))
  "Find the previous character whose attribute value satisfies test."
  (let* ((charpos (mark-charpos mark))
         (line (mark-line mark)) char-set mask end-wins)
    (declare (type (or character-set null) char-set)
             (type (or fixnum null) charpos))
    (cached-attribute-lookup attribute test char-set mask end-wins)
    (cond
     ((cond
       ((eq line open-line)
        (when (rev-cache-find-attribute charpos charpos char-set mask)
          (setf (mark-charpos mark) (1+ charpos)) mark))
       (t
        (when (rev-normal-find-attribute line charpos charpos char-set mask)
          (setf (mark-charpos mark) (1+ charpos)) mark))))
     ;; Newlines win and there is one.
     ((and (line-previous line)
           (not (zerop (logand mask (char-set-ref char-set (char-code #\newline))))))
      (move-to-position mark 0 line))
     (t
      (do (next)
          (())
        (setq next line  line (line-previous line))
        (cond
         ((null line)
          (if end-wins
              (return (line-start mark next))
              (return nil)))
         ((eq line open-line)
          (when (rev-cache-find-attribute nil charpos char-set mask)
            (return (move-to-position mark (1+ charpos) line))))
         (t
          (when (rev-normal-find-attribute line nil charpos char-set mask)
            (return (move-to-position mark (1+ charpos) line))))))))))
