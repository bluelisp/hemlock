;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock-ext)

(defconstant hi::char-code-limit 256)
(defconstant char-code-limit 256)

(defmacro file-comment (&rest ignore)
  (declare (ignore ignore))
  nil)

(defun skip-whitespace (&optional (stream *standard-input*))
  (peek-char t stream))


;;; These are just stubs for now:

(defun quit ()
  )

(defun sap-ref-8 (vec index)
  (declare (ignore vec index))
  (error "SAP-REF-8 called.") )

(defvar hi::*command-line-switches* nil)

(defun hi::get-terminal-name ()
  "vt100")

(defun default-directory ()
  (let* ((p (hemlock::buffer-default-directory (current-buffer)))
         (p (and p (namestring p))))
    (if (and p
             (handler-case
                 (eq (iolib.os:file-kind p) :directory)
               (iolib.pathnames:invalid-file-path () nil)))
        p
        (isys:getcwd))))

(defun find-buffer (name)
  (getstring name hi::*buffer-names*))

(defun maybe-rename-buffer (buffer new-name)
  (unless (find-buffer new-name)
    (setf (buffer-name buffer) new-name)))

(defun rename-buffer-uniquely (buffer new-name)
  (or (maybe-rename-buffer buffer new-name)
      (iter:iter
       (iter:for i from 2)
       (iter:until
        (maybe-rename-buffer buffer (format nil "~A<~D>" new-name i))))))

;;;;;;;;;;;;

(defstruct (object-set (:constructor make-object-set (name &optional default-handler)))
  name
  default-handler
  (table (make-hash-table)))

(defvar *xwindow-hash* (make-hash-table :test #'eq))

(defun hi::add-xwindow-object (window object object-set)
  (setf (gethash window *xwindow-hash*) (list object object-set)))

(defun hi::remove-xwindow-object (window)
  (remhash window *xwindow-hash*))

(defun lisp--map-xwindow (window)
  ;; -> object object-set
  (values-list (gethash window *xwindow-hash*)))


;; CLX integration: define this macro unconditionally.  The function
;; called by its expansion is defined by them hemlock.clx system only.
(defmacro with-clx-event-handling ((display handler) &rest body)
  "Evaluates body in a context where events are handled for the display
   by calling handler on the display.  This destroys any previously established
   handler for display."
  `(call-with-clx-event-handling (lambda () ,@body) ,display ,handler))





;;;; Key and button service.

(defun serve-key-press (object-set fun)
  "Associate a method in the object-set with :key-press events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :key-press (object-set-table object-set)) fun))

(defun serve-key-release (object-set fun)
  "Associate a method in the object-set with :key-release events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :key-release (object-set-table object-set)) fun))

(defun serve-button-press (object-set fun)
  "Associate a method in the object-set with :button-press events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :button-press (object-set-table object-set)) fun))

(defun serve-button-release (object-set fun)
  "Associate a method in the object-set with :button-release events.  The
   method is called on the object the event occurred, event key, event window,
   root, child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :button-release (object-set-table object-set)) fun))



;;;; Mouse service.

(defun serve-motion-notify (object-set fun)
  "Associate a method in the object-set with :motion-notify events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, hint-p, and
   send-event-p."
  (setf (gethash :motion-notify (object-set-table object-set)) fun))

(defun serve-enter-notify (object-set fun)
  "Associate a method in the object-set with :enter-notify events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, mode, kind,
   and send-event-p."
  (setf (gethash :enter-notify (object-set-table object-set)) fun))

(defun serve-leave-notify (object-set fun)
  "Associate a method in the object-set with :leave-notify events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, mode, kind,
   and send-event-p."
  (setf (gethash :leave-notify (object-set-table object-set)) fun))



;;;; Keyboard service.

(defun serve-focus-in (object-set fun)
  "Associate a method in the object-set with :focus-in events.  The method
   is called on the object the event occurred, event key, event window, mode,
   kind, and send-event-p."
  (setf (gethash :focus-in (object-set-table object-set)) fun))

(defun serve-focus-out (object-set fun)
  "Associate a method in the object-set with :focus-out events.  The method
   is called on the object the event occurred, event key, event window, mode,
   kind, and send-event-p."
  (setf (gethash :focus-out (object-set-table object-set)) fun))



;;;; Exposure service.

(defun serve-exposure (object-set fun)
  "Associate a method in the object-set with :exposure events.  The method
   is called on the object the event occurred, event key, event window, x, y,
   width, height, count, and send-event-p."
  (setf (gethash :exposure (object-set-table object-set)) fun))

(defun serve-graphics-exposure (object-set fun)
  "Associate a method in the object-set with :graphics-exposure events.  The
   method is called on the object the event occurred, event key, event window,
   x, y, width, height, count, major, minor, and send-event-p."
  (setf (gethash :graphics-exposure (object-set-table object-set)) fun))

(defun serve-no-exposure (object-set fun)
  "Associate a method in the object-set with :no-exposure events.  The method
   is called on the object the event occurred, event key, event window, major,
   minor, and send-event-p."
  (setf (gethash :no-exposure (object-set-table object-set)) fun))



;;;; Structure service.

(defun serve-visibility-notify (object-set fun)
  "Associate a method in the object-set with :visibility-notify events.  The
   method is called on the object the event occurred, event key, event window,
   state, and send-event-p."
  (setf (gethash :visibility-notify (object-set-table object-set)) fun))

(defun serve-create-notify (object-set fun)
  "Associate a method in the object-set with :create-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, x, y, width, height, border-width, override-redirect-p, and
   send-event-p."
  (setf (gethash :create-notify (object-set-table object-set)) fun))

(defun serve-destroy-notify (object-set fun)
  "Associate a method in the object-set with :destroy-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, and send-event-p."
  (setf (gethash :destroy-notify (object-set-table object-set)) fun))

(defun serve-unmap-notify (object-set fun)
  "Associate a method in the object-set with :unmap-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, configure-p, and send-event-p."
  (setf (gethash :unmap-notify (object-set-table object-set)) fun))

(defun serve-map-notify (object-set fun)
  "Associate a method in the object-set with :map-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, override-redirect-p, and send-event-p."
  (setf (gethash :map-notify (object-set-table object-set)) fun))

(defun serve-map-request (object-set fun)
  "Associate a method in the object-set with :map-request events.  The
   method is called on the object the event occurred, event key, event window,
   window, and send-event-p."
  (setf (gethash :map-request (object-set-table object-set)) fun))

(defun serve-reparent-notify (object-set fun)
  "Associate a method in the object-set with :reparent-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, parent, x, y, override-redirect-p, and send-event-p."
  (setf (gethash :reparent-notify (object-set-table object-set)) fun))

(defun serve-configure-notify (object-set fun)
  "Associate a method in the object-set with :configure-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, x, y, width, height, border-width, above-sibling,
   override-redirect-p, and send-event-p."
  (setf (gethash :configure-notify (object-set-table object-set)) fun))

(defun serve-gravity-notify (object-set fun)
  "Associate a method in the object-set with :gravity-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, x, y, and send-event-p."
  (setf (gethash :gravity-notify (object-set-table object-set)) fun))

(defun serve-resize-request (object-set fun)
  "Associate a method in the object-set with :resize-request events.  The
   method is called on the object the event occurred, event key, event window,
   width, height, and send-event-p."
  (setf (gethash :resize-request (object-set-table object-set)) fun))

(defun serve-configure-request (object-set fun)
  "Associate a method in the object-set with :configure-request events.  The
   method is called on the object the event occurred, event key, event window,
   window, x, y, width, height, border-width, stack-mode, above-sibling,
   value-mask, and send-event-p."
  (setf (gethash :configure-request (object-set-table object-set)) fun))

(defun serve-circulate-notify (object-set fun)
  "Associate a method in the object-set with :circulate-notify events.  The
   method is called on the object the event occurred, event key, event window,
   window, place, and send-event-p."
  (setf (gethash :circulate-notify (object-set-table object-set)) fun))

(defun serve-circulate-request (object-set fun)
  "Associate a method in the object-set with :circulate-request events.  The
   method is called on the object the event occurred, event key, event window,
   window, place, and send-event-p."
  (setf (gethash :circulate-request (object-set-table object-set)) fun))



;;;; Misc. service.

(defun serve-property-notify (object-set fun)
  "Associate a method in the object-set with :property-notify events.  The
   method is called on the object the event occurred, event key, event window,
   atom, state, time, and send-event-p."
  (setf (gethash :property-notify (object-set-table object-set)) fun))

(defun serve-selection-clear (object-set fun)
  "Associate a method in the object-set with :selection-clear events.  The
   method is called on the object the event occurred, event key, event window,
   selection, time, and send-event-p."
  (setf (gethash :selection-clear (object-set-table object-set)) fun))

(defun serve-selection-request (object-set fun)
  "Associate a method in the object-set with :selection-request events.  The
   method is called on the object the event occurred, event key, event window,
   requestor, selection, target, property, time, and send-event-p."
  (setf (gethash :selection-request (object-set-table object-set)) fun))

(defun serve-selection-notify (object-set fun)
  "Associate a method in the object-set with :selection-notify events.  The
   method is called on the object the event occurred, event key, event window,
   selection, target, property, time, and send-event-p."
  (setf (gethash :selection-notify (object-set-table object-set)) fun))

(defun serve-colormap-notify (object-set fun)
  "Associate a method in the object-set with :colormap-notify events.  The
   method is called on the object the event occurred, event key, event window,
   colormap, new-p, installed-p, and send-event-p."
  (setf (gethash :colormap-notify (object-set-table object-set)) fun))

(defun serve-client-message (object-set fun)
  "Associate a method in the object-set with :client-message events.  The
   method is called on the object the event occurred, event key, event window,
   format, data, and send-event-p."
  (setf (gethash :client-message (object-set-table object-set)) fun))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hi::%sp-byte-blt (src start dest dstart end)
  (loop for s from start
        for d from dstart below end
        do
        (setf (aref dest d) (aref src s))))


;;(trace object-set-event-handler hi::invoke-scheduled-events hi::next-scheduled-event-wait serve-event)

(defun hi::%sp-find-character-with-attribute (string start end table mask)
  ;;(declare (type (simple-array (mod 256) char-code-max) table))
  (declare (simple-string string))
  (declare (fixnum start end))
  "%SP-Find-Character-With-Attribute  String, Start, End, Table, Mask
  The codes of the characters of String from Start to End are used as indices
  into the Table, which is a U-Vector of 8-bit bytes. When the number picked
  up from the table bitwise ANDed with Mask is non-zero, the current
  index into the String is returned. The corresponds to SCANC on the Vax."
  (do ((index start (1+ index)))
      ((= index end) nil)
    (declare (fixnum index))
    (if (/= (logand (aref table (char-code (elt string index))) mask) 0)
        (return index))))

(defun hi::%sp-reverse-find-character-with-attribute (string start end table
                                                          mask)
  ;;(declare (type (simple-array (mod 256) char-code-max) table))
  (declare (simple-string string))
  (declare (fixnum start end))
  "Like %SP-Find-Character-With-Attribute, only sdrawkcaB."
  (do ((index (1- end) (1- index)))
      ((< index start) nil)
    (declare (fixnum index))
    (if (/= (logand (aref table (char-code (elt string index))) mask) 0)
        (return index))))

(defun hi::%sp-find-character (string start end character)
  "%SP-Find-Character  String, Start, End, Character
  Searches String for the Character from Start to End.  If the character is
  found, the corresponding index into String is returned, otherwise NIL is
  returned."
  (position character string :start start :end end))

(defun delq (item list)
  (delete item list))

(defun memq (item list)
  (member item list))

(defun assq (item alist)
  (assoc item alist))

(defun concat (&rest args)
  (apply #'concatenate 'string args))


;;;; complete-file

(defun complete-file (pathname &key (defaults *default-pathname-defaults*)
                      ignore-types)
  (let ((files (complete-file-directory pathname defaults)))
    (cond ((null files)
           (values nil nil))
          ((null (cdr files))
           (values (car files)
                   t))
          (t
           (let ((good-files
                  (delete-if #'(lambda (pathname)
                                 (and (simple-string-p
                                       (pathname-type pathname))
                                      (member (pathname-type pathname)
                                              ignore-types
                                              :test #'string=)))
                             files)))
             (cond ((null good-files))
                   ((null (cdr good-files))
                    (return-from complete-file
                      (values (car good-files)
                              t)))
                   (t
                    (setf files good-files)))
             (let ((common (file-namestring (car files))))
               (dolist (file (cdr files))
                 (let ((name (file-namestring file)))
                   (dotimes (i (min (length common) (length name))
                             (when (< (length name) (length common))
                               (setf common name)))
                     (unless (char= (schar common i) (schar name i))
                       (setf common (subseq common 0 i))
                       (return)))))
               (values (merge-pathnames common pathname)
                       nil)))))))

;;; COMPLETE-FILE-DIRECTORY-ARG -- Internal.
;;;
(defun complete-file-directory (pathname defaults)
  (let* ((namestring
          (namestring
           (merge-pathnames pathname (directory-namestring defaults))))
         (directory
          (if (eq (iolib.os::get-file-kind namestring t) :directory)
              namestring
              (iolib.pathnames:file-path-directory namestring :namestring t))))
    (delete-if-not (lambda (candidate)
                     (search namestring candidate))
                   (append
                    (when (probe-file namestring)
                      (list namestring))
                    (mapcar (lambda (f)
                              (iolib.pathnames:file-path-namestring
                               (iolib.pathnames:merge-file-paths
                                f directory)))
                            (iolib.os:list-directory directory))))))

;;; Ambiguous-Files  --  Public
;;;
(defun ambiguous-files (pathname
                        &optional (defaults *default-pathname-defaults*))
  "Return a list of all files which are possible completions of Pathname.
   We look in the directory specified by Defaults as well as looking down
   the search list."
  (complete-file-directory pathname defaults))


;;;; CLISP fixage

;;;;;;

(defun set-file-permissions (pathname permissions)
  ;; implementation defined, currently a nop
  ;; Code from the original WRITE-FILE function:
;    #+NIL
;    (when access
;      (multiple-value-bind
;         (winp code)
;         ;; Must do a TRUENAME in case the file has never been written.
;         ;; It may have Common Lisp syntax that Unix can't handle.
;         ;; If this is ever moved to the beginning of this function to use
;         ;; Unix CREAT to create the file protected initially, they TRUENAME
;         ;; will signal an error, and LISP::PREDICT-NAME will have to be used.
;         (unix:unix-chmod (namestring (truename pathname)) access)
;       (unless winp
;         (error "Could not set access code: ~S"
;                (unix:get-unix-error-msg code)))))
  (declare (ignore pathname permissions))
  nil)
