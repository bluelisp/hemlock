;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains most of the junk that needs to be in the compiler
;;; to compile Hemlock commands.
;;;
;;; Written by Rob MacLachlin and Bill Chiles.
;;;

(in-package :hemlock-internals)

(declaim (special *editor-input*)) ; defined in input --amb

;;;; Macros used for manipulating Hemlock variables.

(defmacro invoke-hook (place &rest args)
  "Call the functions in place with args.  If place is a symbol, then this
   interprets it as a Hemlock variable rather than a Lisp variable, using its
   current value as the list of functions."
  (let ((f (gensym)))
    `(dolist (,f ,(if (symbolp place) `(%value ',place) place))
       (funcall ,f ,@args))))

(defmacro value (name)
  "Return the current value of the Hemlock variable name."
  `(%value ',name))

(defmacro setv (name new-value)
  "Set the current value of the Hemlock variable name, calling any hook
   functions with new-value before setting the value."
  `(%set-value ',name ,new-value))

;;; WITH-VARIABLE-OBJECT  --  Internal
;;;
;;;    Look up the variable object for name and bind it to obj, giving error
;;; if there is no such variable.
;;;
(defmacro with-variable-object (name &body forms)
  `(let ((obj (get ,name 'hemlock-variable-value)))
     (unless obj (undefined-variable-error ,name))
     ,@forms))

(defmacro hlet (binds &rest forms)
  "Hlet ({Var Value}*) {Form}*
   Similar to Let, only it creates temporary Hemlock variable bindings.  Each
   of the vars have the corresponding value during the evaluation of the
   forms."
  (let ((lets ())
        (sets ())
        (unsets ()))
    (dolist (bind binds)
      (let ((n-obj (gensym))
            (n-val (gensym))
            (n-old (gensym)))
        (push `(,n-val ,(second bind)) lets)
        (push `(,n-old (variable-object-value ,n-obj)) lets)
        (push `(,n-obj (with-variable-object ',(first bind) obj)) lets)
        (push `(setf (variable-object-value ,n-obj) ,n-val) sets)
        (push `(setf (variable-object-value ,n-obj) ,n-old) unsets)))
    `(let* ,lets
       (unwind-protect
         (progn ,@sets nil ,@forms)
         ,@unsets))))



;;;; A couple funs to hack strings to symbols.

(eval-when (:compile-toplevel :execute :load-toplevel)

;;; Convert the symbol name 'string to the standard character case for the
;;; lisp implementation which is uppercase for standard CL.
(defun canonical-case (string)
  #-scl
  (nstring-upcase string)
  #+scl
  (if (eq ext:*case-mode* :upper)
      (nstring-upcase string)
      (nstring-downcase string)))

(defun bash-string-to-symbol (name suffix)
  (intern (nsubstitute #\- #\space (canonical-case
                                    (concatenate 'simple-string
                                                 name (symbol-name suffix))))))

;;; string-to-variable  --  Exported
;;;
;;;    Return the symbol which corresponds to the string name
;;; "string".
(defun string-to-variable (string)
  "Returns the symbol name of a Hemlock variable from the corresponding string
   name."
  (intern (nsubstitute #\- #\space (canonical-case string))
          (find-package :hemlock)))

); eval-when

;;; string-to-keyword  --  Internal
;;;
;;;    Mash a string into a Keyword.
;;;
(defun string-to-keyword (string)
  (intern (nsubstitute #\- #\space (canonical-case (string-upcase string)))
          (find-package :keyword)))


;;;; Macros to add and delete hook functions.

;;; add-hook  --  Exported
;;;
;;;    Add a hook function to a hook, defining a variable if
;;; necessary.
;;;
(defmacro add-hook (place hook-fun)
  "Add-Hook Place Hook-Fun
  Add Hook-Fun to the list stored in Place.  If place is a symbol then it
  it is interpreted as a Hemlock variable rather than a Lisp variable."
  (if (symbolp place)
      `(pushnew ,hook-fun (value ,place))
      `(pushnew ,hook-fun ,place)))

;;; remove-hook  --  Public
;;;
;;;    Delete a hook-function from somewhere.
;;;
(defmacro remove-hook (place hook-fun)
  "Remove-Hook Place Hook-Fun
  Remove Hook-Fun from the list in Place.  If place is a symbol then it
  it is interpreted as a Hemlock variable rather than a Lisp variable."
  (if (symbolp place)
      `(setf (value ,place) (delete ,hook-fun (value ,place)))
      `(setf ,place (delete ,hook-fun ,place))))



;;;; DEFCOMMAND.

;;; Defcommand  --  Public
;;;
(defmacro defcommand (name lambda-list command-doc function-doc
                           &body forms)
  "Defcommand Name Lambda-List Command-Doc Function-Doc {Declaration}* {Form}*

  Define a new Hemlock command named Name.  Lambda-List becomes the
  lambda-list, Function-Doc the documentation, and the Forms the
  body of the function which implements the command.  The first
  argument, which must be present, is the prefix argument.  The name
  of this function is derived by replacing all spaces in the name with
  hyphens and appending \"-COMMAND\".  Command-Doc becomes the
  documentation for the command.  See the command implementor's manual
  for further details.

  An example:
    (defcommand \"Forward Character\" (p)
      \"Move the point forward one character.
       With prefix argument move that many characters, with negative argument
       go backwards.\"
      \"Move the point of the current buffer forward p characters.\"
      (unless (character-offset (buffer-point (current-buffer)) (or p 1))
        (editor-error)))"

  (unless (stringp function-doc)
    (error "Command function documentation is not a string: ~S."
                  function-doc))
  (when (atom lambda-list)
    (if lambda-list
        (error "Command argument list is not a list ~S." lambda-list)
        (error "Command argument list is empty: prefix argument is required.")))
  (let (command-name function-name)
    (cond ((listp name)
           (setq command-name (car name)  function-name (cadr name))
           (unless (symbolp function-name)
             (error "Function name is not a symbol: ~S" function-name)))
          (t
           (setq command-name name
                 function-name (bash-string-to-symbol name '-command))))
    (unless (stringp command-name)
      (error "Command name is not a string: ~S." name))
    `(eval-when (:load-toplevel :execute)
       (defun ,function-name ,lambda-list
         ,function-doc
         (declare (ignorable ,(car lambda-list)))
         ,@forms)
       (make-command ',name ,command-doc ',function-name)
       ',function-name)))



;;;; PARSE-FORMS

;;; Parse-Forms  --  Internal
;;;
;;;    Used for various macros to get the declarations out of a list of
;;; forms.
;;;
(eval-when (:compile-toplevel :execute :load-toplevel)
(defmacro parse-forms ((decls-var forms-var forms) &body gorms)
  "Parse-Forms (Decls-Var Forms-Var Forms) {Form}*
  Binds Decls-Var to leading declarations off of Forms and Forms-Var
  to what is left."
  `(do ((,forms-var ,forms (cdr ,forms-var))
        (,decls-var ()))
       ((or (atom ,forms-var) (atom (car ,forms-var))
            (not (eq (caar ,forms-var) 'declare)))
        ,@gorms)
     (push (car ,forms-var) ,decls-var)))
)



;;;; WITH-MARK and USE-BUFFER.

(defmacro with-mark (mark-bindings &rest forms)
  "With-Mark ({(Mark Pos [Kind])}*) {declaration}* {form}*
  With-Mark binds a variable named Mark to a mark specified by Pos.  This
  mark is :temporary, or of kind Kind.  The forms are then evaluated."
  (do ((bindings mark-bindings (cdr bindings))
       (let-slots ())
       (cleanup ()))
      ((null bindings)
       (if cleanup
           (parse-forms (decls forms forms)
             `(let ,(nreverse let-slots)
                ,@decls
                (unwind-protect
                  (progn ,@forms)
                  ,@cleanup)))
           `(let ,(nreverse let-slots) ,@forms)))
    (let ((name (caar bindings))
          (pos (cadar bindings))
          (type (or (caddar bindings) :temporary)))
      (cond ((not (eq type :temporary))
             (push `(,name (copy-mark ,pos ,type)) let-slots)
             (push `(delete-mark ,name) cleanup))
            (t
             (push `(,name (copy-mark ,pos :temporary)) let-slots))))))

#|SAve this shit in case we want WITH-MARKto no longer cons marks.
(defconstant with-mark-total 50)
(defvar *with-mark-free-marks* (make-array with-mark-total))
(defvar *with-mark-next* 0)

(defmacro with-mark (mark-bindings &rest forms)
  "WITH-MARK ({(Mark Pos [Kind])}*) {declaration}* {form}*
   WITH-MARK evaluates each form with each Mark variable bound to a mark
   specified by the respective Pos, a mark.  The created marks are of kind
   :temporary, or of kind Kind."
  (do ((bindings mark-bindings (cdr bindings))
       (let-slots ())
       (cleanup ()))
      ((null bindings)
       (let ((old-next (gensym)))
         (parse-forms (decls forms forms)
           `(let ((*with-mark-next* *with-mark-next*)
                  (,old-next *with-mark-next*))
              (let ,(nreverse let-slots)
                ,@decls
                (unwind-protect
                    (progn ,@forms)
                  ,@cleanup))))))
       (let ((name (caar bindings))
             (pos (cadar bindings))
             (type (or (caddar bindings) :temporary)))
         (push `(,name (mark-for-with-mark ,pos ,type)) let-slots)
         (if (eq type :temporary)
             (push `(delete-mark ,name) cleanup)
             ;; Assume mark is on free list and drop its hold on data.
             (push `(setf (mark-line ,name) nil) cleanup)))))

;;; MARK-FOR-WITH-MARK -- Internal.
;;;
;;; At run time of a WITH-MARK form, this returns an appropriate mark at the
;;; position mark of type kind.  First it uses one from the vector of free
;;; marks, possibly storing one in the vector if we need more marks than we
;;; have before, and that need is still less than the total free marks we are
;;; willing to hold onto.  If we're over the free limit, just make one for
;;; throwing away.
;;;
(defun mark-for-with-mark (mark kind)
  (let* ((line (mark-line mark))
         (charpos (mark-charpos mark))
         (mark (cond ((< *with-mark-next* with-mark-total)
                      (let ((m (svref *with-mark-free-marks* *with-mark-next*)))
                        (cond ((markp m)
                               (setf (mark-line m) line)
                               (setf (mark-charpos m) charpos)
                               (setf (mark-%kind m) kind))
                              (t
                               (setf m (internal-make-mark line charpos kind))
                               (setf (svref *with-mark-free-marks*
                                            *with-mark-next*)
                                     m)))
                        (incf *with-mark-next*)
                        m))
                     (t (internal-make-mark line charpos kind)))))
    (unless (eq kind :temporary)
      (push mark (line-marks (mark-line mark))))
    mark))
|#

(defmacro use-buffer (buffer &body forms)
  "Use-Buffer Buffer {Form}*
  Has The effect of making Buffer the current buffer during the evaluation
  of the Forms.  For restrictions see the manual."
  (let ((gensym (gensym)))
    `(let ((,gensym *current-buffer*)
           (*current-buffer* ,buffer))
       (unwind-protect
        (progn
         (use-buffer-set-up ,gensym)
         ,@forms)
        (use-buffer-clean-up ,gensym)))))



;;;; EDITOR-ERROR.

(defun print-editor-error (condx s)
    (apply #'format s (editor-error-format-string condx)
            (editor-error-format-arguments condx)))

(define-condition editor-error (error)
  ((format-string :initform "" :initarg :format-string
                  :reader editor-error-format-string)
   (format-arguments :initform '() :initarg :format-arguments
                     :reader editor-error-format-arguments))
  (:report print-editor-error))
;;;
(setf (documentation 'editor-error-format-string 'function)
      "Returns the FORMAT control string of the given editor-error condition.")
(setf (documentation 'editor-error-format-arguments 'function)
      "Returns the FORMAT arguments for the given editor-error condition.")

(defun editor-error (&rest args)
  "This function is called to signal minor errors within Hemlock;
   these are errors that a normal user could encounter in the course of editing
   such as a search failing or an attempt to delete past the end of the buffer.
   This function SIGNAL's an editor-error condition formed from args.  Hemlock
   invokes commands in a dynamic context with an editor-error condition handler
   bound.  This default handler beeps or flashes (or both) the display.  If
   args were supplied, it also invokes MESSAGE on them.  The command in
   progress is always aborted, and this function never returns."
  (let ((condx (make-condition 'editor-error
                               :format-string (car args)
                               :format-arguments (cdr args))))
    (signal condx)
    (error "Unhandled editor-error was signaled -- ~A." condx)))



;;;; Do-Strings

(defmacro do-strings ((string-var value-var table &optional result) &body forms)
  "Do-Strings (String-Var Value-Var Table [Result]) {declaration}* {form}*
  Iterate over the strings in a String Table.  String-Var and Value-Var
  are bound to the string and value respectively of each successive entry
  in the string-table Table in alphabetical order.  If supplied, Result is
  a form to evaluate to get the return value."
  (let ((value-nodes (gensym))
        (num-nodes (gensym))
        (value-node (gensym))
        (i (gensym)))
    `(let ((,value-nodes (string-table-value-nodes ,table))
           (,num-nodes (string-table-num-nodes ,table)))
       (dotimes (,i ,num-nodes ,result)
         (declare (fixnum ,i))
         (let* ((,value-node (svref ,value-nodes ,i))
                (,value-var (value-node-value ,value-node))
                (,string-var (value-node-proper ,value-node)))
           (declare (simple-string ,string-var))
           ,@forms)))))



;;;; COMMAND-CASE

;;; COMMAND-CASE  --  Public
;;;
;;;    Grovel the awful thing and spit out the corresponding Cond.  See Echo
;;; for the definition of COMMAND-CASE-HELP and logical char stuff.
;;;
(eval-when (:compile-toplevel :execute :load-toplevel)
(defun command-case-tag (tag key-event char)
  (cond ((and (characterp tag) (standard-char-p tag))
         `(and ,char (char= ,char ,tag)))
        ((and (symbolp tag) (keywordp tag))
         `(logical-key-event-p ,key-event ,tag))
        (t
         (error "Tag in COMMAND-CASE is not a standard character or keyword: ~S"
                tag))))
); eval-when
;;;
(defmacro command-case ((&key (change-window t)
                              (prompt "Command character: ")
                              (help "Choose one of the following characters:")
                              (bind (gensym)))
                        &body forms)
  "This is analogous to the Common Lisp CASE macro.  Commands such as \"Query
   Replace\" use this to get a key-event, translate it to a character, and
   then to dispatch on the character to the specified case.  The syntax is
   as follows:
      (COMMAND-CASE ( {key value}* )
        {( {( {tag}* )  |  tag}  help  {form}* )}*
        )
   Each tag is either a character or a logical key-event.  The user's typed
   key-event is compared using either 'ext:logical-key-event-p or 'char= of
   'ext:key-event-char.

   The legal keys of the key/value pairs are :help, :prompt, :change-window,
   and :bind.  See the manual for details."
  (do* ((forms forms (cdr forms))
        (form (car forms) (car forms))
        (cases ())
        (bname (gensym))
        (again (gensym))
        (n-prompt (gensym))
        (n-change (gensym))
        (bind-char (gensym))
        (docs ())
        (t-case `(t (beep) (reprompt))))
       ((atom forms)
        `(macrolet ((reprompt ()
                      `(progn
                         (setf ,',bind
                               (prompt-for-key-event* ,',n-prompt ,',n-change))
                         (setf ,',bind-char (hemlock-ext:key-event-char ,',bind))
                         (go ,',again))))
           (block ,bname
             (let* ((,n-prompt ,prompt)
                    (,n-change ,change-window)
                    (,bind (prompt-for-key-event* ,n-prompt ,n-change))
                    (,bind-char (hemlock-ext:key-event-char ,bind)))
               (tagbody
                ,again
                (return-from
                 ,bname
                 (cond ,@(nreverse cases)
                       ((logical-key-event-p ,bind :abort)
                        (editor-error))
                       ((logical-key-event-p ,bind :help)
                        (command-case-help ,help ',(nreverse docs))
                        (reprompt))
                       ,t-case)))))))

    (cond ((atom form)
           (error "Malformed Command-Case clause: ~S" form))
          ((eq (car form) t)
           (setq t-case form))
          ((or (< (length form) 2)
               (not (stringp (second form))))
           (error "Malformed Command-Case clause: ~S" form))
          (t
           (let ((tag (car form))
                 (rest (cddr form)))
             (cond ((atom tag)
                    (push (cons (command-case-tag tag bind bind-char) rest)
                          cases)
                    (setq tag (list tag)))
                   (t
                    (do ((tag tag (cdr tag))
                         (res ()
                              (cons (command-case-tag (car tag) bind bind-char)
                                    res)))
                        ((null tag)
                         (push `((or ,@res) . ,rest) cases)))))
             (push (cons tag (second form)) docs))))))



;;;; Some random macros used everywhere.

(defmacro strlen (str) `(length (the simple-string ,str)))
(defmacro neq (a b) `(not (eq ,a ,b)))



;;;; Stuff from here on is implementation dependant.



;;;; WITH-INPUT & WITH-OUTPUT macros.

(defvar *free-hemlock-output-streams* ()
  "This variable contains a list of free Hemlock output streams.")

(defmacro with-output-to-mark ((var mark &optional (buffered ':line))
                               &body gorms)
  "With-Output-To-Mark (Var Mark [Buffered]) {Declaration}* {Form}*
  During the evaluation of Forms, Var is bound to a stream which inserts
  output at the permanent mark Mark.  Buffered is the same as for
  Make-Hemlock-Output-Stream."
  (parse-forms (decls forms gorms)
    `(let ((,var (pop *free-hemlock-output-streams*)))
       ,@decls
       (if ,var
           (modify-hemlock-output-stream ,var ,mark ,buffered)
           (setq ,var (make-hemlock-output-stream ,mark ,buffered)))
       (unwind-protect
         (multiple-value-prog1 ,@forms
           (finish-output ,var))
         (setf (hemlock-output-stream-mark ,var) nil)
         (push ,var *free-hemlock-output-streams*)))))

(defvar *free-hemlock-region-streams* ()
  "This variable contains a list of free Hemlock input streams.")

(defmacro with-input-from-region ((var region) &body gorms)
  "With-Input-From-Region (Var Region) {Declaration}* {Form}*
  During the evaluation of Forms, Var is bound to a stream which
  returns input from Region."
  (parse-forms (decls forms gorms)
    `(let ((,var (pop *free-hemlock-region-streams*)))
       ,@decls
       (if ,var
           (setq ,var (modify-hemlock-region-stream ,var ,region))
           (setq ,var (make-hemlock-region-stream ,region)))
       (unwind-protect
         (progn ,@forms)
         (delete-mark (hemlock-region-stream-mark ,var))
         (push ,var *free-hemlock-region-streams*)))))


(defmacro with-pop-up-display ((var &key height (buffer-name "Random Typeout"))
                               &body body)
  "Execute body in a context with var bound to a stream.  Output to the stream
   appears in the buffer named buffer-name.  The pop-up display appears after
   the body completes, but if you supply :height, the output is line buffered,
   displaying any current output after each line."
  (when (and (numberp height) (zerop height))
    (editor-error "I doubt that you really want a window with no height"))
  `(invoke-with-pop-up-display (lambda (,var)
                                 ,@body)
    ,buffer-name ,height))


;;;; Error handling stuff.

(declaim (special *echo-area-stream*))

;;; LISP-ERROR-ERROR-HANDLER is in Macros.Lisp instead of Rompsite.Lisp because
;;; it uses WITH-POP-UP-DISPLAY, and Macros is compiled after Rompsite.  It
;;; binds an error condition handler to get us out of here on a recursive error
;;; (we are already handling one if we are here).  Since COMMAND-CASE uses
;;; EDITOR-ERROR for logical :abort characters, and this is a subtype of ERROR,
;;; we bind an editor-error condition handler just inside of the error handler.
;;; This keeps us from being thrown out into the debugger with supposedly
;;; recursive errors occuring.  What we really want in this case is to simply
;;; get back to the command loop and forget about the error we are currently
;;; handling.
;;;

;; Diese Logik ist wohl zu clever, als dass sie aktuell in phemlock
;; funktionieren koennte.
#+(or)
(defun lisp-error-error-handler (condition &optional internalp)
  ;;(invoke-debugger condition)
  (handler-bind ((editor-error #'(lambda (condx)
                                   (declare (ignore condx))
                                   (beep)
                                   (throw 'command-loop-catcher nil)))
                 (error #'(lambda (condition)
                            (declare (ignore condition))
                            (let ((device (device-hunk-device
                                           (window-hunk (current-window)))))
                              (device-exit device))
                            (invoke-debugger
                             (make-condition
                              'simple-condition
                              :format-control
                              "Error in error handler; Hemlock broken.")))))
    (princ condition)
    (clear-echo-area)
    (clear-editor-input *editor-input*)
    (beep)
    (if internalp (write-string "Internal error: " *echo-area-stream*))
    (princ condition *echo-area-stream*)
    (finish-output *echo-area-stream*)
    (let* ((*editor-input* *real-editor-input*)
           (key-event (get-key-event *editor-input*)))
      (if (eq key-event #k"?")
          (loop
            (command-case (:prompt "Debug: "
                           :help
                           "Type one of the Hemlock debug command characters:")
              (#\d "Enter a break loop."
               (let ((device (device-hunk-device
                              (window-hunk (current-window)))))
                 (device-exit device)
                 (unwind-protect
                     (with-simple-restart
                         (continue "Return to Hemlock's debug loop.")
                       (invoke-debugger condition))
                    (device-init device))))
              #|| GB
              (#\b "Do a stack backtrace."
                 (with-pop-up-display (*debug-io* :height 100)
                 (debug:backtrace)))
              ||#
              (#\e "Show the error."
               (with-pop-up-display (*standard-output*)
                 (princ condition)))
              ((#\q :exit) "Throw back to Hemlock top-level."
               (throw 'editor-top-level-catcher nil))
              #||
              (#\r "Try to restart from this error."
               (let ((cases (compute-restarts)))
                 (declare (list cases))
                 (with-pop-up-display (s :height (1+ (length cases)))
                   (debug::show-restarts cases s))
                 (invoke-restart-interactively
                  (nth (prompt-for-integer :prompt "Restart number: ")
                       cases))))
              ||#
              ))
          (unget-key-event key-event *editor-input*))
      (throw 'editor-top-level-catcher nil))))

(defvar *debug-on-error* nil
  "If true, do not attempt to handle errors and go directly to the
   Lisp's low-level debugger.  Note that this variable does not enable
   Hemlock-specific debugging features.  To debug Hemlock interactively,
   use slime or slave debugging.")

(defvar *stack-trace-on-error* nil
  "If true (the default), handle errors by showing a stack trace in a buffer.

   Otherwise Hemlock will also attempt to show a stack trace, but will do
   so on standard output.")

(defun simple-print-restarts (&optional (s *standard-output*))
  (format s "~&Restart actions (select using :continue)~%")
  (let ((restarts (compute-restarts)))
    (dotimes (i (length restarts))
      (format s "~&~2D: ~A~%" i (nth i restarts)))))

(defun lisp-error-error-handler (condition &optional internalp)
  (declare (ignore internalp))
  (let ((message-only-p (typep condition 'editor-error)))
    (cond
     (*debug-on-error*
      (invoke-debugger condition))
     ((and *stack-trace-on-error* (not message-only-p))
      (with-pop-up-display (s :height 100)
        (format s "An error of type ~A has been caught and ignored:~%  ~A~%~%"
                (type-of condition)
                condition)
        (format s "The stack trace at the time of the error was:~%~%")
        (hemlock::simple-backtrace s)
        (format s "~%Restarts would have been:~%~%")
        (simple-print-restarts s)
        (hemlock::beginning-of-buffer-command nil))
      (throw 'command-loop-catcher nil))
     (message-only-p
      (message "Error: ~A" condition)
      (throw 'command-loop-catcher nil))
     (t
      (warn "ignoring error: ~A" condition)
      (message "Error: ~A" condition)
      (hemlock::simple-backtrace *standard-output*)
      (throw 'command-loop-catcher nil)))))

(defmacro handle-lisp-errors (&body body)
  "Handle-Lisp-Errors {Form}*
  If a Lisp error happens during the evaluation of the body, then it is
  handled in some fashion.  This should be used by commands which may
  get a Lisp error due to some action of the user."
  `(handler-bind ((error #'lisp-error-error-handler))
     ,@body))

(defvar *connection-backend* :iolib)

(defmacro with-new-event-loop ((&optional) &body body)
  `(invoke-with-new-event-loop *connection-backend* (lambda () ,@body)))

(defmacro with-existing-event-loop (event-base &body body)
  `(invoke-with-existing-event-loop *connection-backend*
                                    ,event-base
                                    (lambda () ,@body)))

(defmacro later (&body body)
  `(invoke-later *connection-backend* (lambda () ,@body)))
