;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    This file contains definitions of various types of streams used
;;; in Hemlock.  They are implementation dependant, but should be
;;; portable to all implementations based on Spice Lisp with little
;;; difficulty.
;;;
;;; Written by Skef Wholey and Rob MacLachlan.
;;;

(in-package :hemlock-internals)

(defclass hemlock-output-stream (#-scl fundamental-character-output-stream
                                 #+scl character-output-stream

                                 #-scl fundamental-character-input-stream
                                 #+scl character-input-stream)
  ((mark
    :initform nil
    :accessor hemlock-output-stream-mark
    :documentation "The mark we insert at.")
   (input-string
    :initform nil)
   (input-pos
    :initform 0)
   (out
    :accessor old-lisp-stream-out)
   (sout
    :accessor old-lisp-stream-sout)))

;; this should suffice for now:
(defmethod stream-write-char ((stream hemlock-output-stream) char)
  (funcall (old-lisp-stream-out stream) stream char))

(defmethod print-object ((object hemlock-output-stream) stream)
  (write-string "#<Hemlock output stream>" stream))

(defun make-hemlock-output-stream (mark &optional (buffered :line))
  "Returns an output stream whose output will be inserted at the Mark.
  Buffered, which indicates to what extent the stream may be buffered
  is one of the following:
   :None  -- The screen is brought up to date after each stream operation.
   :Line  -- The screen is brought up to date when a newline is written.
   :Full  -- The screen is not updated except explicitly via Force-Output."
  (modify-hemlock-output-stream (make-instance 'hemlock-output-stream) mark
                                buffered))


(defun modify-hemlock-output-stream (stream mark buffered)
  (unless (and (markp mark)
               (member (mark-kind mark) '(:right-inserting :left-inserting)))
    (error "~S is not a permanent mark." mark))
  (setf (hemlock-output-stream-mark stream) mark)
  (case buffered
    (:none
     (setf (old-lisp-stream-out stream) #'hemlock-output-unbuffered-out
           (old-lisp-stream-sout stream) #'hemlock-output-unbuffered-sout))
    (:line
     (setf (old-lisp-stream-out stream) #'hemlock-output-line-buffered-out
           (old-lisp-stream-sout stream) #'hemlock-output-line-buffered-sout))
    (:full
     (setf (old-lisp-stream-out stream) #'hemlock-output-buffered-out
           (old-lisp-stream-sout stream) #'hemlock-output-buffered-sout))
    (t
     (error "~S is a losing value for Buffered." buffered)))
  stream)

(defmacro with-left-inserting-mark ((var form) &body forms)
  (let ((change (gensym)))
    `(let* ((,var ,form)
            (,change (eq (mark-kind ,var) :right-inserting)))
       (unwind-protect
           (progn
             (when ,change
               (setf (mark-kind ,var) :left-inserting))
             ,@forms)
         (when ,change
           (setf (mark-kind ,var) :right-inserting))))))

(defun hemlock-output-unbuffered-out (stream character)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-character mark character)
    (redisplay-windows-from-mark mark)))

(defun hemlock-output-unbuffered-sout (stream string start end)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-string mark string start end)
    (redisplay-windows-from-mark mark)))

(defun hemlock-output-buffered-out (stream character)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-character mark character)))

(defun hemlock-output-buffered-sout (stream string start end)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-string mark string start end)))

(defun hemlock-output-line-buffered-out (stream character)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-character mark character)
    (when (char= character #\newline)
      (redisplay-windows-from-mark mark))))

(defun hemlock-output-line-buffered-sout (stream string start end)
  (declare (simple-string string))
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-string mark string start end)
    (when (find #\newline string :start start :end end)
      (redisplay-windows-from-mark mark))))

#+NIL
(defmethod excl:stream-line-length ((stream hemlock-output-stream))
  (let* ((buffer (line-buffer (mark-line (hemlock-output-stream-mark stream)))))
       (when buffer
         (do ((w (buffer-windows buffer) (cdr w))
              (min most-positive-fixnum (min (window-width (car w)) min)))
             ((null w)
              (if (/= min most-positive-fixnum) min))))))

(defmethod stream-finish-output ((stream hemlock-output-stream))
  (redisplay-windows-from-mark (hemlock-output-stream-mark stream)))

(defmethod stream-force-output ((stream hemlock-output-stream))
  (redisplay-windows-from-mark (hemlock-output-stream-mark stream)))

(defmethod close ((stream hemlock-output-stream) &key abort)
  (declare (ignore abort))
  (setf (hemlock-output-stream-mark stream) nil))

(defmethod stream-line-column ((stream hemlock-output-stream))
  (mark-charpos (hemlock-output-stream-mark stream)))


;;; input methods: although called HEMLOCK-OUTPUT-STREAM, the following
;;; methods allow the stream to used for input, too.  Don't do this
;;; at home, because it enters the command loop recursively in a potentially
;;; bad way, but it can be very useful for debugging purposes;

(defvar hi::*reading-lispbuf-input* nil)

(defun ensure-output-stream-input (stream)
  (with-slots (input-string input-pos mark) stream
    (unless (and input-string (< input-pos (length input-string)))
      (setf input-string
            (catch 'hi::lispbuf-input
              (let ((hi::*reading-lispbuf-input* t)
                    (buffer (line-buffer (mark-line mark))))
                (move-mark
                 (variable-value 'hemlock::buffer-input-mark :buffer buffer)
                 (buffer-point buffer))
                (%command-loop))))
      (check-type input-string string)
      (setf input-pos 0))))

(defmethod stream-read-char ((stream hemlock-output-stream))
  (ensure-output-stream-input stream)
  (stream-read-char-no-hang stream))

(defmethod stream-read-char-no-hang ((stream hemlock-output-stream))
  (with-slots (input-string input-pos) stream
    (if (and input-string (< input-pos (length input-string)))
        (prog1
            (elt input-string input-pos)
          (incf input-pos))
        :eof)))

(defmethod stream-listen ((stream hemlock-output-stream))
  (with-slots (input-string input-pos) stream
    (and input-string (< input-pos (length input-string)))))

(defmethod stream-unread-char ((stream hemlock-output-stream) char)
  (with-slots (input-pos) stream
    (unless (plusp input-pos)
      (error "nothing to unread"))
    (decf input-pos)))

(defmethod stream-clear-input ((stream hemlock-output-stream))
  (with-slots (input-string input-pos) stream
    (unless (and input-string (< input-pos (length input-string)))
      (setf input-string nil)))
  nil)

;;; end of input methods, back in sane code


(defclass hemlock-region-stream (#-scl fundamental-character-input-stream
                                 #+scl character-input-stream)
  ;;
  ;; The region we read from.
  ((region :initarg :region
           :accessor hemlock-region-stream-region)
   ;;
   ;; The mark pointing to the next character to read.
   (mark :initarg :mark
         :accessor hemlock-region-stream-mark)) )

(defmethod print-object ((object hemlock-region-stream) stream)
  (declare (ignorable object))
  (write-string "#<Hemlock region stream>" stream))

(defun make-hemlock-region-stream (region)
  "Returns an input stream that will return successive characters from the
  given Region when asked for input."
  (make-instance 'hemlock-region-stream
                 :region region
                 :mark (copy-mark (region-start region) :right-inserting)))

(defun modify-hemlock-region-stream (stream region)
  (setf (hemlock-region-stream-region stream) region)
  (let* ((mark (hemlock-region-stream-mark stream))
         (start (region-start region))
         (start-line (mark-line start)))
    ;; Make sure it's dead.
    (delete-mark mark)
    (setf (mark-line mark) start-line  (mark-charpos mark) (mark-charpos start))
    (push mark (line-marks start-line)))
  stream)

(defmethod stream-read-char ((stream hemlock-region-stream))
  (let ((mark (hemlock-region-stream-mark stream)))
    (cond ((mark< mark
                  (region-end (hemlock-region-stream-region stream)))
           (prog1 (next-character mark) (mark-after mark)))
          (t :eof))))

(defmethod stream-listen ((stream hemlock-region-stream))
  (mark< (hemlock-region-stream-mark stream)
         (region-end (hemlock-region-stream-region stream))))

(defmethod stream-unread-char ((stream hemlock-region-stream) char)
  (let ((mark (hemlock-region-stream-mark stream)))
    (unless (mark> mark
                   (region-start (hemlock-region-stream-region stream)))
      (error "Nothing to unread."))
    (unless (char= char (previous-character mark))
      (error "Unreading something not read: ~S" char))
    (mark-before mark)))

(defmethod stream-clear-input ((stream hemlock-region-stream))
  (move-mark
   (hemlock-region-stream-mark stream)
   (region-end (hemlock-region-stream-region stream)))
  nil)

(defmethod close ((stream hemlock-region-stream) &key abort)
  (declare (ignorable abort))
  (delete-mark (hemlock-region-stream-mark stream))
  (setf (hemlock-region-stream-region stream) nil))

#+excl
(defmethod excl:stream-read-char-no-hang ((stream hemlock-region-stream))
  (stream-read-char stream))

#||
(defmethod excl::stream-file-position ((stream hemlock-output-stream) &optional pos)
  (assert (null pos))
  (mark-charpos (hemlock-output-stream-mark stream)))

(defun region-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation

    (:file-position
     (let ((start (region-start (hemlock-region-stream-region stream)))
           (mark (hemlock-region-stream-mark stream)))
       (cond (arg1
              (move-mark mark start)
              (character-offset mark arg1))
             (t
              (count-characters (region start mark)))))) ))
||#


;;;; Stuff to support keyboard macros.

(defclass kbdmac-stream (editor-input)
  ((buffer :initarg :buffer
           :initform nil
           :accessor kbdmac-stream-buffer
           :documentation "The simple-vector that holds the characters.")
   (index  :initarg :index
           :initform nil
           :accessor kbdmac-stream-index
           :documentation "Index of the next character.")))

(defun make-kbdmac-stream ()
  (make-instance 'kbdmac-stream))

(defmethod get-key-event ((stream kbdmac-stream) &optional ignore-abort-attempts-p)
  (declare (ignore ignore-abort-attempts-p))
  (let ((index (kbdmac-stream-index stream)))
    (setf (kbdmac-stream-index stream) (1+ index))
    (setq *last-key-event-typed*
          (svref (kbdmac-stream-buffer stream) index))))

(defmethod unget-key-event (ignore (stream kbdmac-stream))
  (declare (ignore ignore))
  (if (plusp (kbdmac-stream-index stream))
      (decf (kbdmac-stream-index stream))
      (error "Nothing to unread.")))

(defmethod listen-editor-input ((stream kbdmac-stream))
  (declare (ignore stream))
  t)

;; clear-editor-input ?? --GB

;;; MODIFY-KBDMAC-STREAM  --  Internal
;;;
;;;    Bash the kbdmac-stream Stream so that it will return the Input.
;;;
(defun modify-kbdmac-stream (stream input)
  (setf (kbdmac-stream-index stream) 0)
  (setf (kbdmac-stream-buffer stream) input)
  stream)
