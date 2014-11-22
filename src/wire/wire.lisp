;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; **********************************************************************
;;;
;;; This file contains an interface to internet domain sockets.
;;;
;;; Written by William Lott.
;;;

(in-package :hemlock.wire)

;;; Stuff that needs to be ported:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +buffer-size+ 2048)
  (defconstant +initial-cache-size+ 16)
  (defconstant +funcall0-op+ 0)
  (defconstant +funcall1-op+ 1)
  (defconstant +funcall2-op+ 2)
  (defconstant +funcall3-op+ 3)
  (defconstant +funcall4-op+ 4)
  (defconstant +funcall5-op+ 5)
  (defconstant +funcall-op+ 6)
  (defconstant +number-op+ 7)
  (defconstant +string-op+ 8)
  (defconstant +symbol-op+ 9)
  (defconstant +save-op+ 10)
  (defconstant +lookup-op+ 11)
  (defconstant +remote-op+ 12)
  (defconstant +cons-op+ 13)
  (defconstant +bignum-op+ 14))


(defvar *current-wire* nil
  "The wire the form we are currently evaluating came across.")

(defvar *this-host* nil
  "Unique identifier for this host.")
(defvar *this-pid* nil
  "Unique identifier for this process.")

(defvar *object-to-id* (make-hash-table :test 'eq)
  "Hash table mapping local objects to the corresponding remote id.")
(defvar *id-to-object* (make-hash-table :test 'eql)
  "Hash table mapping remote id's to the curresponding local object.")
(defvar *next-id* 0
  "Next available id for remote objects.")

(defstruct device
  (wire nil))

(defstruct (stream-device
             (:include device)
             (:conc-name device-)
             (:constructor make-stream-device (stream)))
  (stream (error "missing argument") :type stream))

(defmethod print-object ((object stream-device) stream)
  (print-unreadable-object (object stream)
    (format stream "~A" (device-stream object))))

(defstruct (wire
            (:constructor %make-wire (device))
            (:print-function
             (lambda (wire stream depth)
               (declare (ignore depth))
               (print-unreadable-object (wire stream)
                 (format stream "~A" (wire-device wire))))))
  (device (error "missing argument") :type device)

  (ibuf (make-array +buffer-size+ :element-type '(unsigned-byte 8)))
  (ibuf-offset 0)
  (ibuf-end 0)

  (obuf (make-array +buffer-size+ :element-type '(unsigned-byte 8)))
  (obuf-end 0)

  (object-cache (make-array +initial-cache-size+))
  (cache-index 0)
  (object-hash (make-hash-table :test 'eq))

  (encoding :utf-8))

(defun make-wire (device)
  (let ((wire (%make-wire device)))
    (setf (device-wire device) wire)
    wire))

(defstruct (remote-object
            (:constructor %make-remote-object (host pid id))
            (:print-function
             (lambda (obj stream depth)
               (declare (ignore depth))
               (format stream "#<Remote Object: [~x:~a] ~s>"
                       (remote-object-host obj)
                       (remote-object-pid obj)
                       (remote-object-id obj)))))
  host
  pid
  id)

(define-condition wire-error (error)
  ((wire :reader wire-error-wire :initarg :wire))
  (:report (lambda (condition stream)
             (format stream "There is a problem with ~A."
                     (wire-error-wire condition)))))

(define-condition wire-eof (wire-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Recieved EOF on ~A."
                     (wire-error-wire condition)))))

(define-condition wire-io-error (wire-error)
  ((when :reader wire-io-error-when :initarg :when :initform "using")
   (msg :reader wire-io-error-msg :initarg :msg :initform "Failed."))
  (:report (lambda (condition stream)
             (format stream "Error ~A ~A: ~A."
                     (wire-io-error-when condition)
                     (wire-error-wire condition)
                     (wire-io-error-msg condition)))))

(defclass serializable-object () ())
(defgeneric serialize (serializable-object))
(defgeneric deserialize-with-type (type data))


;;; Remote Object Randomness

;;; REMOTE-OBJECT-LOCAL-P -- public
;;;
;;;   First, make sure the *this-host* and *this-pid* are set. Then test to
;;; see if the remote object's host and pid fields are *this-host* and
;;; *this-pid*

(defun remote-object-local-p (remote)
  "Returns T iff the given remote object is defined locally."
  (declare (type remote-object remote))
  (unless *this-host*
    (setf *this-host* (unix-gethostid))
    (setf *this-pid* (unix-getpid)))
  (and (eql (remote-object-host remote) *this-host*)
       (eql (remote-object-pid remote) *this-pid*)))

;;; REMOTE-OBJECT-EQ -- public
;;;
;;;   Remote objects are considered EQ if they refer to the same object, ie
;;; Their host, pid, and id fields are the same (eql, cause they are all
;;; numbers).

(defun remote-object-eq (remote1 remote2)
  "Returns T iff the two objects refer to the same (eq) object in the same
  process."
  (declare (type remote-object remote1 remote2))
  (and (eql (remote-object-host remote1)
            (remote-object-host remote2))
       (eql (remote-object-pid remote1)
            (remote-object-pid remote2))
       (eql (remote-object-id remote1)
            (remote-object-id remote2))))

;;; REMOTE-OBJECT-VALUE --- public
;;;
;;;   First assure that the remote object is defined locally. If so, look up
;;; the id in *id-to-objects*.
;;; table. This will only happen if FORGET-REMOTE-TRANSLATION has been called
;;; on the local object.

(defun remote-object-value (remote)
  "Return the associated value for the given remote object. It is an error if
  the remote object was not created in this process or if
  FORGET-REMOTE-TRANSLATION has been called on this remote object."
  (declare (type remote-object remote))
  (unless (remote-object-local-p remote)
    (error "~S is defined is a different process." remote))
  (multiple-value-bind
      (value found)
      (gethash (remote-object-id remote)
               *id-to-object*)
    (unless found
      (cerror
       "Use the value of NIL"
       "No value for ~S -- FORGET-REMOTE-TRANSLATION was called to early."
       remote))
    value))

;;; MAKE-REMOTE-OBJECT --- public
;;;
;;;   Convert the given local object to a remote object. If the local object is
;;; alread entered in the *object-to-id* hash table, just use the old id.
;;; Otherwise, grab the next id and put add both mappings to the two hash
;;; tables.

(defun make-remote-object (local)
  "Convert the given local object to a remote object."
  (unless *this-host*
    (setf *this-host* (unix-gethostid))
    (setf *this-pid* (unix-getpid)))
  (let ((id (gethash local *object-to-id*)))
    (unless id
      (setf id *next-id*)
      (setf (gethash local *object-to-id*) id)
      (setf (gethash id *id-to-object*) local)
      (incf *next-id*))
    (%make-remote-object *this-host* *this-pid* id)))

;;; FORGET-REMOTE-TRANSLATION -- public
;;;
;;;   Remove any translation information about the given object. If there is
;;; currenlt no translation for the object, don't bother doing anything.
;;; Otherwise remove it from the *object-to-id* hashtable, and remove the id
;;; from the *id-to-object* hashtable.

(defun forget-remote-translation (local)
  "Forget the translation from the given local to the corresponding remote
object. Passing that remote object to remote-object-value will new return NIL."
  (let ((id (gethash local *object-to-id*)))
    (when id
      (remhash local *object-to-id*)
      (remhash id *id-to-object*)))
  (values))


;;; Wire input routeins.

;;; WIRE-LISTEN -- public
;;;
;;;   If nothing is in the current input buffer, select on the file descriptor.

(defgeneric device-listen (device))

(defun wire-listen (wire)
  "Return T iff anything is in the input buffer or available on the socket."
  (or (< (wire-ibuf-offset wire) (wire-ibuf-end wire))
      #+(or)
      (progn
        (dispatch-events-no-hang)
        (< (wire-ibuf-offset wire) (wire-ibuf-end wire)))
      (device-listen (wire-device wire))))

(defmethod device-listen ((device stream-device))
  (listen (device-stream device)))

;;; WIRE-GET-BYTE -- public
;;;
;;;   Return the next byte.

(defun wire-get-byte (wire)
  "Return the next byte from the wire."
  (when (<= (wire-ibuf-end wire)
            (wire-ibuf-offset wire))
    (fill-input-buffer wire))
  (prog1
      (elt (wire-ibuf wire)
           (wire-ibuf-offset wire))
    (incf (wire-ibuf-offset wire))))

;;; FILL-INPUT-BUFFER -- Internal
;;;
;;;   Fill the input buffer from the socket. If we get an error reading, signal
;;; a wire-io-error. If we get an EOF, signal a wire-eof error. If we get any
;;; data, set the ibuf-end index.

(defun fill-input-buffer (wire)
  "Read data off the socket, filling the input buffer. The buffer is cleared
   first. If fill-input-buffer returns, it is guarenteed that there will be at
   least one byte in the input buffer. If EOF was reached, as wire-eof error
   is signaled."
  (setf (wire-ibuf-offset wire) 0
        (wire-ibuf-end wire) 0)
  (incf (wire-ibuf-end wire)
        (device-read (wire-device wire)
                     (or (wire-ibuf wire)
                         (error 'wire-eof :wire wire))))
  (when (zerop (wire-ibuf-end wire))
    (error 'wire-eof :wire wire))
  (values))

(defmethod device-read ((device stream-device) buffer)
  (read-sequence (device-stream device) buffer))

;;; APPEND-TO-INPUT-BUFFER -- Internal
;;;
;;;   Add new data to the input buffer.  Used asynchonous devices, where
;;; fill-input-buffer itself it just there to re-enter the event loop.

(defun append-to-input-buffer (wire bytes)
  (let* ((oldpos (wire-ibuf-end wire))
         (newpos (+ oldpos (length bytes))))
    (setf (wire-ibuf-end wire) newpos)
    (when (> newpos (length (wire-ibuf wire)))
      (let ((old (wire-ibuf wire)))
        (setf (wire-ibuf wire)
              (make-array newpos :element-type '(unsigned-byte 8)))
        (replace (wire-ibuf wire) old)))
    (replace (wire-ibuf wire) bytes :start1 oldpos)))

;;; APPEND-TO-INPUT-BUFFER -- External.
;;;
;;;   The externally-visible interface of the above function for device
;;; classes.

(defun device-append-to-input-buffer (device bytes)
  (append-to-input-buffer (device-wire device) bytes))

;;; WIRE-GET-NUMBER -- public
;;;
;;;   Just read four bytes and pack them together with normal math ops.

(defun wire-get-number (wire &optional (signed t))
  "Read a number off the wire. Numbers are 4 bytes in network order.
The optional argument controls weather or not the number should be considered
signed (defaults to T)."
  (let* ((b1 (wire-get-byte wire))
         (b2 (wire-get-byte wire))
         (b3 (wire-get-byte wire))
         (b4 (wire-get-byte wire))
         (unsigned
          (+ b4 (* 256 (+ b3 (* 256 (+ b2 (* 256 b1))))))))
    (if (and signed (> b1 127))
        (logior (ash -1 32) unsigned)
        unsigned)))

;;; WIRE-GET-BIGNUM -- public
;;;
;;; Extracts a number, which might be a bignum.
;;;
(defun wire-get-bignum (wire)
  "Reads an arbitrary integer sent by WIRE-OUTPUT-BIGNUM from the wire and
   return it."
  (let ((count-and-sign (wire-get-number wire)))
    (do ((count (abs count-and-sign) (1- count))
         (result 0 (+ (ash result 32) (wire-get-number wire nil))))
        ((not (plusp count))
         (if (minusp count-and-sign)
             (- result)
             result)))))

;;; WIRE-GET-STRING -- public
;;;
;;;   Use WIRE-GET-NUMBER to read the length, then keep pulling stuff out of
;;; the input buffer and re-filling it with FILL-INPUT-BUFFER until we've read
;;; the entire string.

(defun wire-get-string (wire)
  "Reads a string from the wire. The first four bytes spec the size in bytes."
  (let* ((nbytes (wire-get-number wire))
         (bytes (make-array nbytes :element-type '(unsigned-byte 8)))
         (offset 0))
    (declare (integer nbytes offset))
    (loop
      (let ((avail (- (wire-ibuf-end wire)
                      (wire-ibuf-offset wire)))
            (ibuf (wire-ibuf wire)))
        (declare (integer avail))
        (cond ((<= nbytes avail)
               (replace bytes
                        ibuf
                        :start1 offset
                        :start2 (wire-ibuf-offset wire))
               (incf (wire-ibuf-offset wire) nbytes)
               (return nil))
              ((zerop avail)
               (fill-input-buffer wire))
              (t
               (replace bytes
                        ibuf
                        :start1 offset
                        :start2 (wire-ibuf-offset wire)
                        :end2 (wire-ibuf-end wire))
               (incf offset avail)
               (decf nbytes avail)
               (incf (wire-ibuf-offset wire) avail)))))
    (babel:octets-to-string bytes :encoding (wire-encoding wire))))

;;; WIRE-GET-OBJECT -- public
;;;
;;;   First, read a byte to determine the type of the object to read. Then,
;;; depending on the type, call WIRE-GET-NUMBER, WIRE-GET-STRING, or whatever
;;; to read the necessary data. Note, funcall objects are funcalled.

(defun wire-get-object (wire)
  "Reads the next object from the wire and returns it."
  (let ((identifier (wire-get-byte wire))
        (*current-wire* wire))
    (declare (fixnum identifier))
    (cond ((eql identifier +lookup-op+)
           (let ((index (wire-get-number wire))
                 (cache (wire-object-cache wire)))
             (declare (integer index))
             (declare (simple-vector cache))
             (when (< index (length cache))
               (svref cache index))))
          ((eql identifier +number-op+)
           (wire-get-number wire))
          ((eql identifier +bignum-op+)
           (wire-get-bignum wire))
          ((eql identifier +string-op+)
           (wire-get-string wire))
          ((eql identifier +symbol-op+)
           (let* ((symbol-name (wire-get-string wire))
                  (package-name (wire-get-string wire))
                  (package (find-package package-name)))
             (unless package
               (error "Attempt to read symbol, ~A, of wire into non-existent ~
                       package, ~A."
                      symbol-name package-name))
             (intern symbol-name package)))
          ((eql identifier +cons-op+)
           (cons (wire-get-object wire)
                 (wire-get-object wire)))
          ((eql identifier +remote-op+)
           (let ((host (wire-get-number wire nil))
                 (pid (wire-get-number wire))
                 (id (wire-get-number wire)))
             (%make-remote-object host pid id)))
          ((eql identifier +save-op+)
           (let ((index (wire-get-number wire))
                 (cache (wire-object-cache wire)))
             (declare (integer index))
             (declare (simple-vector cache))
             (when (>= index (length cache))
               (do ((newsize (* (length cache) 2)
                             (* newsize 2)))
                   ((< index newsize)
                    (let ((newcache (make-array newsize)))
                      (declare (simple-vector newcache))
                      (replace newcache cache)
                      (setf cache newcache)
                      (setf (wire-object-cache wire) cache)))))
             (setf (svref cache index)
                   (wire-get-object wire))))
          ((eql identifier +funcall0-op+)
           (funcall (wire-get-object wire)))
          ((eql identifier +funcall1-op+)
           (funcall (wire-get-object wire)
                    (wire-get-object wire)))
          ((eql identifier +funcall2-op+)
           (funcall (wire-get-object wire)
                    (wire-get-object wire)
                    (wire-get-object wire)))
          ((eql identifier +funcall3-op+)
           (funcall (wire-get-object wire)
                    (wire-get-object wire)
                    (wire-get-object wire)
                    (wire-get-object wire)))
          ((eql identifier +funcall4-op+)
           (funcall (wire-get-object wire)
                    (wire-get-object wire)
                    (wire-get-object wire)
                    (wire-get-object wire)
                    (wire-get-object wire)))
          ((eql identifier +funcall5-op+)
           (funcall (wire-get-object wire)
                    (wire-get-object wire)
                    (wire-get-object wire)
                    (wire-get-object wire)
                    (wire-get-object wire)
                    (wire-get-object wire)))
          ((eql identifier +funcall-op+)
           (let ((arg-count (wire-get-byte wire))
                 (function (wire-get-object wire))
                 (args '())
                 (last-cons nil)
                 (this-cons nil))
             (loop
               (when (zerop arg-count)
                 (return nil))
               (setf this-cons (cons (wire-get-object wire)
                                     nil))
               (if (null last-cons)
                 (setf args this-cons)
                 (setf (cdr last-cons) this-cons))
               (setf last-cons this-cons)
               (decf arg-count))
             (apply function args))))))


;;; Wire output routines.

;;; WIRE-FORCE-OUTPUT -- internal
;;;
;;;   Output any stuff remaining in the output buffer.

(defun wire-force-output (wire)
  "Send any info still in the output buffer down the wire and clear it. Nothing
harmfull will happen if called when the output buffer is empty."
  (unless (zerop (wire-obuf-end wire))
    (device-write (wire-device wire)
                  (wire-obuf wire)
                  (wire-obuf-end wire))
    (setf (wire-obuf-end wire) 0))
  (values))

(defmethod device-write
    ((device stream-device) buffer &optional (end (length buffer)))
  (write-sequence (device-stream device) buffer :end end))

;;; WIRE-OUTPUT-BYTE -- public
;;;
;;;   Stick the byte in the output buffer. If there is no space, flush the
;;; buffer using WIRE-FORCE-OUTPUT.

(defun wire-output-byte (wire byte)
  "Output the given (8-bit) byte on the wire."
  (declare (integer byte))
  (let ((fill-pointer (wire-obuf-end wire))
        (obuf (wire-obuf wire)))
    (when (>= fill-pointer (length obuf))
      (wire-force-output wire)
      (setf fill-pointer 0))
    (setf (elt obuf fill-pointer) byte)
    (setf (wire-obuf-end wire) (1+ fill-pointer)))
  (values))

;;; WIRE-OUTPUT-NUMBER -- public
;;;
;;;   Output the number. Note, we don't care if the number is signed or not,
;;; because we just crank out the low 32 bits.
;;;
(defun wire-output-number (wire number)
  "Output the given (32-bit) number on the wire."
  (declare (integer number))
  (wire-output-byte wire (+ 0 (ldb (byte 8 24) number)))
  (wire-output-byte wire (ldb (byte 8 16) number))
  (wire-output-byte wire (ldb (byte 8 8) number))
  (wire-output-byte wire (ldb (byte 8 0) number))
  (values))

;;; WIRE-OUTPUT-BIGNUM -- public
;;;
;;; Output an arbitrary integer.
;;;
(defun wire-output-bignum (wire number)
  "Outputs an arbitrary integer, but less effeciently than WIRE-OUTPUT-NUMBER."
  (do ((digits 0 (1+ digits))
       (remaining (abs number) (ash remaining -32))
       (words nil (cons (ldb (byte 32 0) remaining) words)))
      ((zerop remaining)
       (wire-output-number wire
                           (if (minusp number)
                               (- digits)
                               digits))
       (dolist (word words)
         (wire-output-number wire word)))))

;;; WIRE-OUTPUT-STRING -- public
;;;
;;;   Output the string. Strings are represented by the length as a number,
;;; followed by the bytes of the string.
;;;
(defun wire-output-string (wire string)
  "Output the given string. First output the length using WIRE-OUTPUT-NUMBER,
then output the bytes."
  (declare (simple-string string))
  (let* ((bytes (babel:string-to-octets string :encoding (wire-encoding wire)))
         (nbytes (length bytes)))
    (wire-output-number wire nbytes)
    (let* ((obuf (wire-obuf wire))
           (obuf-end (wire-obuf-end wire))
           (available (- (length obuf) obuf-end)))
      (declare (integer available))
      (cond ((>= available nbytes)
             (replace obuf bytes :start1 obuf-end)
             (incf (wire-obuf-end wire) nbytes))
            ((> nbytes (length obuf))
             (wire-force-output wire)
             (device-write (wire-device wire) bytes))
            (t
             (wire-force-output wire)
             (replace obuf bytes)
             (setf (wire-obuf-end wire) nbytes)))))
  (values))

;;; WIRE-OUTPUT-FUNCALL -- public
;;;
;;;   Send the funcall down the wire. Arguments are evaluated locally in the
;;; lexical environment of the WIRE-OUTPUT-FUNCALL.

(defmacro wire-output-funcall (wire-form function &rest args)
  "Send the function and args down the wire as a funcall."
  (let ((num-args (length args))
        (wire (gensym)))
    `(let ((,wire ,wire-form))
       ,@(if (> num-args 5)
            `((wire-output-byte ,wire +funcall-op+)
              (wire-output-byte ,wire ,num-args))
            `((wire-output-byte ,wire ,(+ +funcall0-op+ num-args))))
       (wire-output-object ,wire ,function)
       ,@(mapcar #'(lambda (arg)
                     `(wire-output-object ,wire ,arg))
                 args)
       (values))))

;;; WIRE-OUTPUT-OBJECT -- public
;;;
;;;   Output the given object. If the optional argument is non-nil, cache
;;; the object to enhance the performance of sending it multiple times.
;;; Caching defaults to yes for symbols, and nil for everything else.

(defun wire-output-object
    (wire object &optional (cache-it (symbolp object)))
  "Output the given object on the given wire. If cache-it is T, enter this
object in the cache for future reference."
  (let ((cache-index (gethash object
                              (wire-object-hash wire))))
    (cond
     (cache-index
      (wire-output-byte wire +lookup-op+)
      (wire-output-number wire cache-index))
     (t
      (when cache-it
        (wire-output-byte wire +save-op+)
        (let ((index (wire-cache-index wire)))
          (wire-output-number wire index)
          (setf (gethash object (wire-object-hash wire))
                index)
          (setf (wire-cache-index wire) (1+ index))))
      (typecase object
        (integer
         (cond ((typep object '(signed-byte 32))
                (wire-output-byte wire +number-op+)
                (wire-output-number wire object))
               (t
                (wire-output-byte wire +bignum-op+)
                (wire-output-bignum wire object))))
        (simple-string
         (wire-output-byte wire +string-op+)
         (wire-output-string wire object))
        (symbol
         (wire-output-byte wire +symbol-op+)
         (wire-output-string wire (symbol-name object))
         (wire-output-string wire (package-name (symbol-package object))))
        (cons
         (wire-output-byte wire +cons-op+)
         (wire-output-object wire (car object))
         (wire-output-object wire (cdr object)))
        (remote-object
         (wire-output-byte wire +remote-op+)
         (wire-output-number wire (remote-object-host object))
         (wire-output-number wire (remote-object-pid object))
         (wire-output-number wire (remote-object-id object)))
        (serializable-object
         (multiple-value-bind (type data)
             (serialize object)
           (wire-output-funcall wire
                                'deserialize-with-type
                                type
                                data)))
        (t
         (error "Error: Cannot output objects of type ~s across a wire."
                (type-of object)))))))
  (values))
