;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Hemlock Documentation and Help commands.
;;; Written by Rob MacLachlan and Bill Chiles.
;;;

(in-package :hemlock)



;;;; Help.

(defcommand "Help" (p)
  "Give helpful information.
  This command dispatches to a number of other documentation commands,
  on the basis of a character command."
  "Prompt for a single character command to dispatch to another helping
  function."
  (declare (ignore p))
  (command-case (:prompt "Doc (Help for Help): "
                 :help "Type a Help option to say what kind of help you want:")
    (#\a "List all commands, variables and attributes Apropos a keyword."
     (apropos-command nil))
    (#\d "Describe a command, given its name."
     (describe-command-command nil))
    (#\g "Generic describe, any Hemlock thing (e.g., variables, keys, attributes)."
     (generic-describe-command nil))
    (#\v "Describe variable and show its values."
     (describe-and-show-variable-command nil))
    (#\c "Describe the command bound to a Character."
     (describe-key-command nil))
    (#\l "List the last 60 characters typed."
     (what-lossage-command nil))
    (#\m "Describe a mode."
     (describe-mode-command nil))
    (#\p "Describe commands with mouse/pointer bindings."
     (describe-pointer-command nil))
    (#\w "Find out Where a command is bound."
     (where-is-command nil))
    (#\t "Describe a Lisp object."
     (editor-describe-command nil))
    ((#\q :no) "Quits, You don't really want help.")))

(defcommand "Where Is" (p)
  "Find what key a command is bound to.
   Prompts for the command to look for, and says what environment it is
   available in."
  "List places where a command is bound."
  (declare (ignore p))
  (multiple-value-bind (nam cmd)
                       (prompt-for-keyword (list *command-names*)
                                           :prompt "Command: "
                                           :help "Name of command to look for.")
    (let ((bindings (command-bindings cmd)))
      (with-pop-up-display (s)
        (cond
         ((null bindings)
          (format s "~S may only be invoked as an extended command.~%" nam))
         (t
          (format s "~S may be invoked in the following ways:~%" nam)
          (print-command-bindings bindings s)))))))



;;;; Apropos.

(defcommand "Apropos" (p)
  "List things whose names contain a keyword."
  "List things whose names contain a keyword."
  (declare (ignore p))
  (let* ((str (prompt-for-string
                :prompt "Apropos keyword: "
                :help
                "String to look for in command, variable and attribute names."))
         (coms (find-containing str *command-names*))
         (vars (mapcan #'(lambda (table)
                           (let ((res (find-containing str table)))
                             (when res (list (cons table res)))))
                       (current-variable-tables)))
         (attr (find-containing str *character-attribute-names*)))
    (if (or coms vars attr)
        (apropos-command-output str coms vars attr)
        (message "No command, attribute or variable name contains ~S." str))))

(defun apropos-command-output (str coms vars attr)
  (declare (list coms vars attr))
  (with-pop-up-display (s)
    (let* ((w (hi::stream-line-length s))
           (div (make-string w :initial-element #\-))
           (col (truncate (* 2/3 w))))
      (flet ((write-padded (string)
               (write-string string s)
               (write-string (make-string (max 3 (- col (length string)))
                                          :initial-element #\space)
                             s)))
        (when coms
          (format s "COMMANDS with ~S in their names~%" str)
          (dolist (com coms)
            (let ((obj (getstring com *command-names*)))
              (write-padded com)
              (print-command-bindings (command-bindings obj) s)
              (terpri s)
              (print-short-doc (command-documentation obj) s))))
        (when vars
          (when coms
            (write-line div s))
          (format s "VARIABLES with ~S in their names~%" str)
          (dolist (stuff vars)
            (let ((table (car stuff)))
              (dolist (var (cdr stuff))
                (let ((obj (getstring var table)))
                  (write-padded var)
                  (let ((*print-level* 2) (*print-length* 3))
                    (prin1 (variable-value obj) s))
                  (terpri s)
                  (print-short-doc (variable-documentation obj) s))))))
        (when attr
          (when (or vars coms)
            (write-line div s))
          (format s "ATTRIBUTES with ~S in their names~%" str)
          (dolist (att attr)
            (let ((obj (getstring att *character-attribute-names*)))
              (write-line att s)
              (print-short-doc (character-attribute-documentation obj) s))))))))

;;; PRINT-SHORT-DOC takes doc, a function or string, and gets it out on stream.
;;; If doc is a string, this only outputs up to the first newline.  All output
;;; is preceded by two spaces.
;;;
(defun print-short-doc (doc stream)
  (let ((str (typecase doc
               (function
                (funcall doc :short))
               (string
                (let ((end (or (position #\newline (the simple-string doc))
                               (length doc))))
                  (if (zerop end)
                      "(no documentation)"
                      (subseq doc 0 end))))
               (t
                (error "Bad documentation: ~S" doc)))))
    (write-string "  " stream)
    (write-line str stream)))



;;;; Describe command, key, pointer.

(defcommand "Describe Command" (p)
  "Describe a command.
  Prompts for a command and then prints out it's full documentation."
  "Print out the command documentation for a command which is prompted for."
  (declare (ignore p))
  (multiple-value-bind (nam com)
                       (prompt-for-keyword
                        (list *command-names*)
                        :prompt "Describe command: "
                        :help "Name of a command to document.")
    (let ((bindings (command-bindings com)))
      (with-pop-up-display (s)
        (format s "Documentation for ~S:~%   ~A~%"
                nam (command-documentation com))
        (cond ((not bindings)
               (write-line
                "This can only be invoked as an extended command." s))
              (t
               (write-line
                "This can be invoked in the following ways:" s)
               (write-string "   " s)
               (print-command-bindings bindings s)
               (terpri s)))))))

(defcommand "Describe Key" (p)
  "Prompt for a sequence of characters.  When the first character is typed that
   terminates a key binding in the current context, describe the command bound
   to it.  When the first character is typed that no longer allows a correct
   key to be entered, tell the user that this sequence is not bound to
   anything."
  "Print out the command documentation for a key
  which is prompted for."
  (declare (ignore p))
  (let (#+echo-area-is-separate-window
        (old-window (current-window)))
    (unwind-protect
        (progn
          #+echo-area-is-separate-window
          (setf (current-window) hi::*echo-area-window*)
          (hi::display-prompt-nicely "Describe key: " nil)
          (setf (fill-pointer hi::*prompt-key*) 0)
          (loop
            (let ((key-event (get-key-event hi::*editor-input*)))
              (vector-push-extend key-event hi::*prompt-key*)
              (let ((res (get-command hi::*prompt-key* :current)))
                (hemlock-ext:print-pretty-key-event key-event *echo-area-stream*)
                (write-char #\space *echo-area-stream*)
                (finish-output *echo-area-stream*)
                (cond ((commandp res)
                       (with-pop-up-display (s)
                         (hemlock-ext:print-pretty-key (copy-seq hi::*prompt-key*) s)
                         (format s " is bound to ~S.~%" (command-name res))
                         (format s "Documentation for this command:~%   ~A"
                                 (command-documentation res)))
                       (return))
                      ((not (eq res :prefix))
                       (with-pop-up-display (s :height 1)
                         (hemlock-ext:print-pretty-key (copy-seq hi::*prompt-key*) s)
                         (write-string " is not bound to anything." s))
                       (return)))))))
      #+echo-area-is-separate-window
      (setf (current-window) old-window))))

(defcommand "Describe Pointer" (p)
  "Describe commands with any key binding that contains a \"mouse\" character
   (modified or not).  Does not describe the command \"Illegal\"."
  "Describe commands with any key binding that contains a \"mouse\" character
   (modified or not).  Does not describe the command \"Illegal\"."
  (declare (ignore p))
  (let ((illegal-command (getstring "Illegal" *command-names*)))
    (with-pop-up-display (s)
      (dolist (cmd (get-mouse-commands))
        (unless (eq cmd illegal-command)
          (format s "Documentation for ~S:~%   ~A~%"
                  (command-name cmd)
                  (command-documentation cmd))
          (write-line
           "This can be invoked in the following ways:" s)
          (write-string "   " s)
          (print-command-bindings (command-bindings cmd) s)
          (terpri s) (terpri s))))))

(defun get-mouse-commands ()
  (let ((result nil))
    (do-strings (name cmd *command-names* result)
      (declare (ignore name))
      (dolist (b (command-bindings cmd))
        (let ((key (car b)))
          (declare (simple-vector key))
          (when (dotimes (i (length key) nil)
                  (when (member (hemlock-ext:make-key-event (svref key i))
                                (list #k"Leftdown" #k"Leftup" #k"Middledown"
                                      #k"Middleup" #k"Rightdown" #k"Rightup"))
                    (push cmd result)
                    (return t)))
            (return)))))))



;;;; Generic describe variable, command, key, attribute.

(defvar *generic-describe-kinds*
  (list (make-string-table :initial-contents
                           '(("Variable" . :variable)
                             ("Command" . :command)
                             ("Key" . :key)
                             ("Attribute" . :attribute)))))

(defcommand "Generic Describe" (p)
  "Describe some Hemlock thing.
  First prompt for the kind of thing, then prompt for the thing to describe.
  Currently supported kinds of things are variables, commands, keys and
  character attributes."
  "Prompt for some Hemlock thing to describe."
  (declare (ignore p))
  (multiple-value-bind (ignore kwd)
                       (prompt-for-keyword *generic-describe-kinds*
                                           :default "Variable"
                                           :help "Kind of thing to describe."
                                           :prompt "Kind: ")
    (declare (ignore ignore))
    (case kwd
      (:variable
       (describe-and-show-variable-command nil))
      (:command (describe-command-command ()))
      (:key (describe-key-command ()))
      (:attribute
       (multiple-value-bind (name attr)
                            (prompt-for-keyword
                             (list *character-attribute-names*)
                             :help "Name of character attribute to describe."
                             :prompt "Attribute: ")
         (print-full-doc name (character-attribute-documentation attr)))))))

;;; PRINT-FULL-DOC displays whole documentation string in a pop-up window.
;;; Doc may be a function that takes at least one arg, :short or :full.
;;;
(defun print-full-doc (nam doc)
  (typecase doc
    (function (funcall doc :full))
    (simple-string
     (with-pop-up-display (s)
       (format s "Documentation for ~S:~%  ~A" nam doc)))
    (t (error "Bad documentation: ~S" doc))))



;;;; Describing and show variables.

(defcommand "Show Variable" (p)
  "Display the values of a Hemlock variable."
  "Display the values of a Hemlock variable."
  (declare (ignore p))
  (multiple-value-bind (name var)
                       (prompt-for-variable
                        :help "Name of variable to describe."
                        :prompt "Variable: ")
    (let ((buffer (current-buffer)))
      (with-pop-up-display (s)
        (show-variable s name var buffer)))))

(defcommand "Describe and Show Variable" (p)
  "Describe in full and show all of variable's value.
   Variable is prompted for."
  "Describe in full and show all of variable's value."
  (declare (ignore p))
  (multiple-value-bind (name var)
                       (prompt-for-variable
                        :help "Name of variable to describe."
                        :prompt "Variable: ")
    (let ((buffer (current-buffer)))
      (with-pop-up-display (s)
        (format s "Documentation for ~S:~%  ~A~&~%"
                name (variable-documentation var))
        (show-variable s name var buffer)))))

(defun show-variable (s name var buffer)
  (when (hemlock-bound-p var :global)
    (format s "Global value of ~S:~%  ~S~%"
            name (variable-value var :global)))
  (when (hemlock-bound-p var :buffer buffer)
    (format s "Value of ~S in buffer ~A:~%  ~S~%"
            name (buffer-name buffer)
            (variable-value var :buffer buffer)))
  (do-strings (mode-name val *mode-names*)
    (declare (ignore val))
    (when (hemlock-bound-p var :mode mode-name)
      (format s "Value of ~S in ~S Mode:~%  ~S~%"
              name mode-name
              (variable-value var :mode mode-name)))))



;;;; Describing modes.

(defvar *describe-mode-ignore* (list "Illegal" "Do Nothing"))

(defcommand "Describe Mode" (p &optional name)
  "Describe a mode showing special bindings for that mode."
  "Describe a mode showing special bindings for that mode."
  (declare (ignore p))
  (let* ((name (or name
                   (prompt-for-keyword (list *mode-names*)
                                       :prompt "Mode: "
                                       :help "Enter mode to describe."
                                       :default
                                       (car (buffer-modes (current-buffer))))))
         (doc (mode-documentation name))
         (bindings nil)
         (type (if (mode-major-p name) "major" "minor"))
         (max 0))
    (map-bindings (lambda (key cmd)
                    (unless (member (command-name cmd)
                                    *describe-mode-ignore*
                                    :test #'string-equal)
                      (let ((str (key-to-string key)))
                        (setf max (max max (length str)))
                        (push (cons str cmd) bindings))))
                  :mode name)
    (if (or doc bindings)
        (with-pop-up-display (s)
          (format s "~A is a ~A mode.~%" name type)
          (when doc
            (write-line doc s)
            (terpri s))
          (dolist (cell bindings)
            (destructuring-bind (str . cmd) cell
              (write-string str s)
              (write-string (make-string (- max (length str)) :initial-element #\Space) s)
              (write-string "  - " s)
              (print-short-doc (command-documentation cmd) s))))
        (message "~A is a ~A mode." name type))))

(defun key-to-string (key)
  (with-output-to-string (s)
    (hemlock-ext:print-pretty-key key s)))



;;;; Printing bindings and last N characters typed.

(defcommand "What Lossage" (p)
  "Display the last 60 characters typed."
  "Display the last 60 characters typed."
  (declare (ignore p))
  (with-pop-up-display (s :height 7)
    (let ((num (ring-length *key-event-history*)))
      (format s "The last ~D characters typed:~%" num)
      (do ((i (1- num) (1- i)))
          ((minusp i))
        (hemlock-ext:print-pretty-key-event (ring-ref *key-event-history* i) s)
        (write-char #\space s)))))

(defun print-command-bindings (bindings stream)
  (let ((buffer ())
        (mode ())
        (global ()))
    (dolist (b bindings)
      (case (second b)
        (:global (push (first b) global))
        (:mode
         (let ((m (assoc (third b) mode :test #'string=)))
           (if m
               (push (first b) (cdr m))
               (push (list (third b) (first b)) mode))))
        (t
         (let ((f (assoc (third b) buffer)))
           (if f
               (push (first b) (cdr f))
               (push (list (third b) (first b)) buffer))))))
    (when global
      (print-some-keys global stream)
      (write-string "; " stream))
    (dolist (b buffer)
      (format stream "Buffer ~S: " (buffer-name (car b)))
      (print-some-keys (cdr b) stream)
      (write-string "; " stream))
    (dolist (m mode)
      (write-string (car m) stream)
      (write-string ": " stream)
      (print-some-keys (cdr m) stream)
      (write-string "; " stream))))

;;; PRINT-SOME-KEYS prints the list of keys onto Stream.
;;;
(defun print-some-keys (keys stream)
  (do ((key keys (cdr key)))
      ((null (cdr key))
       (hemlock-ext:print-pretty-key (car key) stream))
    (hemlock-ext:print-pretty-key (car key) stream)
    (write-string ", " stream)))
