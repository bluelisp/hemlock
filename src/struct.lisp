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
;;; Structures and assorted macros for Hemlock.
;;;


;;;; Marks.

(defclass mark ()
  ((line :initarg :line :accessor mark-line :documentation "pointer to line")
   (charpos
    :initarg :charpos
    :accessor mark-charpos
    :documentation "The character position of a Hemlock mark.
  A mark's character position is the index within the line of the character
  following the mark.")
   (%kind :initarg :%kind :accessor mark-%kind :documentation "type of mark"))
  (:documentation
   "A Hemlock mark object.  See Hemlock Command Implementor's Manual for details."))

(defun markp (object)
  "Returns true if its argument is a Hemlock mark object, false otherwise."
  (typep object 'mark))

(defun internal-make-mark (line charpos %kind)
  (make-instance 'mark :line line :charpos charpos :%kind %kind))

(defclass font-mark (mark)
  ((font :initarg :font :accessor font-mark-font)))

(defun internal-make-font-mark (line charpos %kind font)
  (make-instance 'font-mark :line line :charpos charpos :%kind %kind :font font))

(defun font-mark-p (object)
  (typep object 'font-mark))

(defmacro fast-font-mark-p (s)
  `(typep ,s 'font-mark))


;;;; Regions, buffers, modeline fields.

;;; The region object:
;;;
(defstruct (region (:print-function %print-hregion)
                   (:predicate regionp)
                   (:copier nil)
                   (:constructor internal-make-region (start end)))
  "A Hemlock region object.  See Hemlock Command Implementor's Manual for details."
  start                                 ; starting mark
  end)                                  ; ending mark

(setf (documentation 'regionp 'function)
  "Returns true if its argument is a Hemlock region object, Nil otherwise.")
(setf (documentation 'region-end 'function)
  "Returns the mark that is the end of a Hemlock region.")
(setf (documentation 'region-start 'function)
  "Returns the mark that is the start of a Hemlock region.")


;;; The buffer object:
;;;
(defstruct (buffer (:constructor internal-make-buffer)
                   (:print-function %print-hbuffer)
                   (:copier nil)
                   (:predicate bufferp))
  "A Hemlock buffer object.  See Hemlock Command Implementor's Manual for details."
  %name                       ; name of the buffer (a string)
  %region                     ; the buffer's region
  %pathname                   ; associated pathname
  modes                       ; list of buffer's mode names
  mode-objects                ; list of buffer's mode objects
  bindings                    ; buffer's command table
  point                       ; current position in buffer
  (%writable t)               ; t => can alter buffer's region
  (modified-tick -2)          ; The last time the buffer was modified.
  (unmodified-tick -1)        ; The last time the buffer was unmodified
  windows                     ; List of all windows into this buffer.
  var-values                  ; the buffer's local variables
  variables                   ; string-table of local variables
  write-date                  ; File-Write-Date for pathname.
  active-region-p             ; The buffer-signature when current region is active.
  display-start               ; Window display start when switching to buf.
  %modeline-fields            ; List of modeline-field-info's.
  (delete-hook nil)           ; List of functions to call upon deletion.
  (widget nil)                ; for virtual buffers, the Qt widget to show
  (tag-line-number -1))       ; tags valid for earlier lines only

(defun virtual-buffer-p (buffer)
  (and (buffer-widget buffer) t))

(setf (documentation 'buffer-modes 'function)
  "Return the list of the names of the modes active in a given buffer.")
(setf (documentation 'buffer-point 'function)
  "Return the mark that is the current focus of attention in a buffer.")
(setf (documentation 'buffer-windows 'function)
  "Return the list of windows that are displaying a given buffer.")
(setf (documentation 'buffer-variables 'function)
  "Return the string-table of the variables local to the specifed buffer.")
(setf (documentation 'buffer-write-date 'function)
  "Return in universal time format the write date for the file associated
   with the buffer.  If the pathname is set, then this should probably
   be as well.  Should be NIL if the date is unknown or there is no file.")
(setf (documentation 'buffer-delete-hook 'function)
  "This is the list of buffer specific functions that Hemlock invokes when
   deleting this buffer.")


;;; Modeline fields.
;;;
(defstruct (modeline-field (:print-function print-modeline-field)
                           (:constructor %make-modeline-field
                                         (%name %function %width)))
  "This is one item displayed in a Hemlock window's modeline."
  %name         ; EQL name of this field.
  %function     ; Function that returns a string for this field.
  %width)       ; Width to display this field in.

(setf (documentation 'modeline-field-p 'function)
      "Returns true if its argument is a modeline field object, nil otherwise.")

(defstruct (modeline-field-info (:print-function print-modeline-field-info)
                                (:conc-name ml-field-info-)
                                (:constructor make-ml-field-info (field)))
  field
  (start nil)
  (end nil))



;;;; The mode object.

(defstruct (mode-object (:predicate modep)
                        (:copier nil)
                        (:print-function %print-hemlock-mode))
  name                   ; name of this mode
  setup-function         ; setup function for this mode
  cleanup-function       ; Cleanup function for this mode
  bindings               ; The mode's command table.
  transparent-p          ; Are key-bindings transparent?
  hook-name              ; The name of the mode hook.
  major-p                ; Is this a major mode?
  precedence             ; The precedence for a minor mode.
  character-attributes   ; Mode local character attributes
  variables              ; String-table of mode variables
  var-values             ; Alist for saving mode variables
  documentation)         ; Introductory comments for mode describing commands.

(defun %print-hemlock-mode (object stream depth)
  (declare (ignore depth))
  (write-string "#<Hemlock Mode \"" stream)
  (write-string (mode-object-name object) stream)
  (write-string "\">" stream))



;;;; Variables.

;;; This holds information about Hemlock variables, and the system stores
;;; these structures on the property list of the variable's symbolic
;;; representation under the 'hemlock-variable-value property.
;;;
(defstruct (variable-object
            (:print-function
             (lambda (object stream depth)
               (declare (ignore depth))
               (format stream "#<Hemlock Variable-Object ~S>"
                       (variable-object-name object))))
            (:copier nil)
            (:constructor make-variable-object (documentation name)))
  value         ; The value of this variable.
  hooks         ; The hook list for this variable.
  down          ; The variable-object for the previous value.
  documentation ; The documentation.
  name)         ; The string name.



;;;; Windows, dis-lines, and font-changes.

;;; The window object:
;;;
(defstruct (window (:constructor internal-make-window)
                   (:predicate windowp)
                   (:copier nil)
                   (:print-function %print-hwindow))
  "This structure implements a Hemlock window."
  tick                          ; The last time this window was updated.
  %buffer                       ; buffer displayed in this window.
  height                        ; Height of window in lines.
  width                         ; Width of the window in characters.
  old-start                     ; The charpos of the first char displayed.
  first-line                    ; The head of the list of dis-lines.
  last-line                     ; The last dis-line displayed.
  first-changed                 ; The first changed dis-line on last update.
  last-changed                  ; The last changed dis-line.
  spare-lines                   ; The head of the list of unused dis-lines
  (old-lines 0)                 ; Slot used by display to keep state info
  hunk                          ; The device hunk that displays this window.
  display-start                 ; first character position displayed
  display-end                   ; last character displayed
  point                         ; Where the cursor is in this window.
  modeline-dis-line             ; Dis-line for modeline display.
  modeline-buffer               ; Complete string of all modeline data.
  modeline-buffer-len           ; Valid chars in modeline-buffer.
  display-recentering)          ; Tells whether redisplay recenters window
                                ;    regardless of whether it is current.

(setf (documentation 'windowp 'function)
  "Returns true if its argument is a Hemlock window object, Nil otherwise.")
(setf (documentation 'window-height 'function)
  "Return the height of a Hemlock window in character positions.")
(setf (documentation 'window-width 'function)
  "Return the width of a Hemlock window in character positions.")
(setf (documentation 'window-display-start 'function)
  "Return the mark which points before the first character displayed in
   the supplied window.")
(setf (documentation 'window-display-end 'function)
  "Return the mark which points after the last character displayed in
   the supplied window.")
(setf (documentation 'window-point 'function)
 "Return the mark that points to where the cursor is displayed in this
  window.  When the window is made current, the Buffer-Point of this window's
  buffer is moved to this position.  While the window is current, redisplay
  makes this mark point to the same position as the Buffer-Point of its
  buffer.")
(setf (documentation 'window-display-recentering 'function)
 "This determines whether redisplay recenters window regardless of whether it
  is current.  This is SETF'able.")

;; Now this is bogus: Both dis-line and window-dis-line use
;; "DIS-LINE-" as conc-name. ACL complains about e.g. DIS-LINE-CHARS
;; being redefined and rightly so. My guess is this is some hangover
;; from elder versions of this fine software.

;; Since there also is no constructor for dis-line they can't be
;; possibly created. I'll just do away with dis-line and just offer
;; window-dis-line.

;; --GB 2002-11-07

;; Temporary workaround:
(deftype dis-line () 'window-dis-line)  ;make the tty code happy

;; FWIW, I think that the Allegro warning is permitted by the spec, but
;; does not indicate a problem.  The code was fine.  CLHS on defstruct
;; says:
;;
;;     Whether or not the :conc-name option is explicitly supplied, the
;;     following rule governs name conflicts of generated reader (or
;;     accessor) names: For any structure type S1 having a reader
;;     function named R for a slot named X1 that is inherited by another
;;     structure type S2 that would have a reader function with the same
;;     name R for a slot named X2, no definition for R is generated by
;;     the definition of S2; instead, the definition of R is inherited
;;     from the definition of S1. (In such a case, if X1 and X2 are
;;     different slots, the implementation might signal a style
;;     warning.)
;;
;; Should we revert to the old definition?  Would it make more sense to have
;; a subclass for each backend?
;;   --DFL


#||
(defstruct (dis-line (:copier nil)
                     (:constructor nil))
  chars                       ; The line-image to be displayed.
  (length 0 :type fixnum)     ; Length of line-image.
  font-changes)               ; Font-Change structures for changes in this line.

(defstruct (window-dis-line (:copier nil)
                            (:include dis-line)
                            (:constructor make-window-dis-line (chars))
                            (:conc-name dis-line-))
  old-chars                   ; Line-Chars of line displayed.
  line                        ; Line displayed.
  (flags 0 :type fixnum)      ; Bit flags indicate line status.
  (delta 0 :type fixnum)      ; # lines moved from previous position.
  (position 0 :type fixnum)   ; Line # to be displayed on.
  (end 0 :type fixnum))       ; Index after last logical character displayed.

||#

(defstruct (window-dis-line (:copier nil)
                            (:constructor make-window-dis-line (chars))
                            (:conc-name dis-line-))
  chars                       ; The line-image to be displayed.
  (length 0 :type fixnum)     ; Length of line-image.
  font-changes                ; Font-Change structures for changes in this line.
  ;; TTY (and/or CLX?) backend stuff:
  old-chars                   ; Line-Chars of line displayed.
  line                        ; Line displayed.
  (flags 0 :type fixnum)      ; Bit flags indicate line status.
  (delta 0 :type fixnum)      ; # lines moved from previous position.
  (position 0 :type fixnum)   ; Line # to be displayed on.
  (end 0 :type fixnum)        ; Index after last logical character displayed.
  (tick 0)
  (tag nil :type (or null tag))
  (tag-ticks -1 :type fixnum))

(defstruct (font-change (:copier nil)
                        (:constructor make-font-change (next)))
  x                           ; X position that change takes effect.
  font                        ; Index into font-map of font to use.
  next                        ; The next Font-Change on this dis-line.
  mark)                       ; Font-Mark responsible for this change.



;;;; Font family.

(defstruct font-family
  map                   ; Font-map for hunk.
  height                ; Height of char box includung VSP.
  width                 ; Width of font.
  baseline              ; Pixels from top of char box added to Y.
  cursor-width          ; Pixel width of cursor.
  cursor-height         ; Pixel height of cursor.
  cursor-x-offset       ; Added to pos of UL corner of char box to get
  cursor-y-offset)      ; UL corner of cursor blotch.



;;;; Attribute descriptors.

(defstruct (attribute-descriptor
            (:copier nil)
            (:print-function %print-attribute-descriptor))
  "This structure is used internally in Hemlock to describe a character
  attribute."
  name
  keyword
  documentation
  char-set
  hooks
  end-value)



;;;; Commands.

(defstruct (command (:constructor internal-make-command
                                  (%name documentation function))
                    (:copier nil)
                    (:predicate commandp)
                    (:print-function %print-hcommand))
  %name                            ;The name of the command
  documentation                    ;Command documentation string or function
  function                         ;The function which implements the command
  %bindings)                       ;Places where command is bound

(setf (documentation 'commandp 'function)
  "Returns true if its argument is a Hemlock command object, Nil otherwise.")
(setf (documentation 'command-documentation 'function)
  "Return the documentation for a Hemlock command, given the command-object.
  Command documentation may be either a string or a function.  This may
  be set with Setf.")



;;;; Random typeout streams.

;;; These streams write to random typeout buffers for WITH-POP-UP-DISPLAY.
;;;

(defclass random-typeout-stream (#-scl fundamental-character-output-stream
                                 #+scl ext:character-output-stream)
  ((mark         :initarg :mark
                 :initform nil
                 :accessor random-typeout-stream-mark
                 :documentation "The buffer point of the associated buffer.")
   (window       :initarg :window
                 :initform nil
                 :accessor random-typeout-stream-window
                 :documentation "The hemlock window all this shit is in.")
   (more-mark    :initarg :more-mark
                 :initform nil
                 :accessor random-typeout-stream-more-mark
                 :documentation "The mark that is not displayed when we need to more.")
   (no-prompt    :initarg :no-prompt
                 :initform nil
                 :accessor random-typeout-stream-no-prompt
                 :documentation "T when we want to exit, still collecting output.")
   (first-more-p :initarg :first-more-p
                 :initform t
                 :accessor random-typeout-stream-first-more-p
                 :documentation "T until the first time we more. Nil after.")
   (line-buffered-p :documentation "whether line buffered") ))

(defun make-random-typeout-stream (mark)
  (make-instance 'random-typeout-stream
                 #+scl #+scl :in-buffer lisp::*empty-string*
                 #+scl #+scl :out-buffer lisp::*empty-string*
                 :mark mark))

(defmethod print-object ((object random-typeout-stream) stream)
  (format stream "#<Hemlock Random-Typeout-Stream ~S>"
          (ignore-errors
            (buffer-name
             (line-buffer (mark-line (random-typeout-stream-mark object)))))))


;;;; Redisplay devices.

;;; Devices contain monitor specific redisplay methods referenced by
;;; redisplay independent code.


(defgeneric device-init (device)
  (:documentation "called whenever going into the editor."))

(defgeneric device-make-window (device start modelinep window font-family
                                ask-user x y width-arg height-arg proportion))

(defgeneric device-exit (device))

(defgeneric device-smart-redisplay (device window)
  (:documentation "redisplay a window on this device."))

(defgeneric device-dumb-redisplay (device window)
  (:documentation "fun to redisplay a window on this device."))

(defgeneric device-after-redisplay (device)
  (:documentation "call at the end of redisplay entry points."))

(defgeneric device-clear (device)
  (:documentation "clear the entire display."))

(defgeneric device-note-read-wait (device on-off)
  (:documentation "somehow note on display that input is expected."))

(defgeneric device-force-output (device)
  (:documentation "force any output possibly buffered."))

(defgeneric device-finish-output (device window)
  (:documentation "force output and hand until done."))

(defgeneric device-put-cursor (device hunk x y)
  (:documentation "put the cursor at (x,y) or (column,line)."))

(defgeneric device-show-mark (device window x y time)
  (:documentation "display the screens cursor at a certain mark."))

(defgeneric device-next-window (device window)
  (:documentation "return the next window"))

(defgeneric device-previous-window (device window)
  (:documentation "return the previous window"))

(defgeneric device-delete-window (device window)
  (:documentation "remove a window from the screen."))

(defgeneric device-random-typeout-full-more (device stream)
  (:documentation "do full-buffered  more-prompting."))

(defgeneric device-random-typeout-line-more (device stream n)
  (:documentation "keep line-buffered streams up to date."))

(defgeneric device-random-typeout-setup (device stream n)
  (:documentation "prepare for random typeout."))

(defgeneric device-random-typeout-cleanup (device stream degree)
  (:documentation "clean up after random typeout."))

(defgeneric device-beep (device stream)
  (:documentation "beep or flash the screen."))

(defmethod device-beep ((device t) stream)
  (declare (ignore stream)))

(defclass device ()
  ((name
    :initarg :name
    :initform nil
    :accessor device-name
    :documentation "simple-string such as \"concept\" or \"lnz\"")
   (bottom-window-base
    :initarg :bottom-window-base
    :initform nil
    :accessor device-bottom-window-base
    :documentation "bottom text line of bottom window.")
   (hunks
    :initarg :hunks
    :initform nil
    :accessor device-hunks
    :documentation "list of hunks on the screen.") ))

;; These seem to have default do-nothing methods ...

(defmethod device-clear ((device device))
  nil)

(defmethod device-after-redisplay ((device device))
  nil)

(defmethod device-note-read-wait ((device device) on-off)
  (declare (ignorable device on-off))
  nil)

(defmethod device-force-output ((device device))
  nil)

(defmethod device-finish-output ((device device) window)
  (declare (ignorable device window))
  nil)

;;

(defmethod print-object ((object device) stream)
  (print-device object stream 0))

(defun %make-device (&rest initargs)    ;### not used
  (apply #'make-instance 'device initargs))

(defun print-device (obj str n)
  (declare (ignore n))
  (print-unreadable-object (obj str :type t :identity t)
    (format str "~S" (device-name obj))))


(defclass tty-device (device)
  ((dumbp :initarg :dumbp :initform nil :accessor tty-device-dumbp)
                                        ; t if it does not have line insertion and deletion.
   (lines :initarg :lines :initform nil :accessor tty-device-lines)
                                        ; number of lines on device.
   (columns :initarg :columns :initform nil :accessor tty-device-columns)
                                        ; number of columns per line.
   (display-string :initarg :display-string :initform nil :accessor tty-device-display-string)
                                        ; fun to display a string of characters at (x,y).
                                        ; args: hunk x y string &optional start end
   (standout-init :initarg :standout-init :initform nil :accessor tty-device-standout-init)
                                        ; fun to put terminal in standout mode.
                                        ; args: hunk
   (standout-end :initarg :standout-end :initform nil :accessor tty-device-standout-end)
                                        ; fun to take terminal out of standout mode.
                                        ; args: hunk
   (clear-lines :initarg :clear-lines :initform nil :accessor tty-device-clear-lines)
                                        ; fun to clear n lines starting at (x,y).
                                        ; args: hunk x y n
   (clear-to-eol :initarg :clear-to-eol :initform nil :accessor tty-device-clear-to-eol)
                                        ; fun to clear to the end of a line from (x,y).
                                        ; args: hunk x y
   (clear-to-eow :initarg :clear-to-eow :initform nil :accessor tty-device-clear-to-eow)
                                        ; fun to clear to the end of a window from (x,y).
                                        ; args: hunk x y
   (open-line :initarg :open-line :initform nil :accessor tty-device-open-line)
                                        ; fun to open a line moving lines below it down.
                                        ; args: hunk x y &optional n
   (delete-line :initarg :delete-line :initform nil :accessor tty-device-delete-line)
                                        ; fun to delete a line moving lines below it up.
                                        ; args: hunk x y &optional n
   (insert-string :initarg :insert-string :initform nil :accessor tty-device-insert-string)
                                        ; fun to insert a string in the middle of a line.
                                        ; args: hunk x y string &optional start end
   (delete-char :initarg :delete-char :initform nil :accessor tty-device-delete-char)
                                        ; fun to delete a character from the middle of a line.
                                        ; args: hunk x y &optional n
   (cursor-x :initarg :cursor-x :initform 0 :accessor tty-device-cursor-x) ; column the cursor is in.
   (cursor-y :initarg :cursor-y :initform 0 :accessor tty-device-cursor-y) ; line the cursor is on.
   (standout-init-string :initarg :standout-init-string :initform nil :accessor tty-device-standout-init-string)
                                        ; string to put terminal in standout mode.
   (standout-end-string :initarg :standout-end-string :initform nil :accessor tty-device-standout-end-string)
                                        ; string to take terminal out of standout mode.
   (clear-to-eol-string :initarg :clear-to-eol-string :initform nil :accessor tty-device-clear-to-eol-string)
                                        ; string to cause device to clear to eol at (x,y).
   (clear-string :initarg :clear-string :initform nil :accessor tty-device-clear-string)
                                        ; string to cause device to clear entire screen.
   (open-line-string :initarg :open-line-string :initform nil :accessor tty-device-open-line-string)
                                        ; string to cause device to open a blank line.
   (delete-line-string :initarg :delete-line-string :initform nil :accessor tty-device-delete-line-string)
                                        ; string to cause device to delete a line, moving
                                        ; lines below it up.
   (insert-init-string :initarg :insert-init-string :initform nil :accessor tty-device-insert-init-string)
                                        ; string to put terminal in insert mode.
   (insert-char-init-string :initarg :insert-char-init-string :initform nil :accessor tty-device-insert-char-init-string)
                                        ; string to prepare terminal for insert-mode character.
   (insert-char-end-string :initarg :insert-char-end-string :initform nil :accessor tty-device-insert-char-end-string)
                                        ; string to affect terminal after insert-mode character.
   (insert-end-string :initarg :insert-end-string :initform nil :accessor tty-device-insert-end-string)
                                        ; string to take terminal out of insert mode.
   (delete-init-string :initarg :delete-init-string :initform nil :accessor tty-device-delete-init-string)
                                        ; string to put terminal in delete mode.
   (delete-char-string :initarg :delete-char-string :initform nil :accessor tty-device-delete-char-string)
                                        ; string to delete a character.
   (delete-end-string :initarg :delete-end-string :initform nil :accessor tty-device-delete-end-string)
                                        ; string to take terminal out of delete mode.
   (init-string :initarg :init-string :initform nil :accessor tty-device-init-string)
                                        ; device init string.
   (cm-end-string :initarg :cm-end-string :initform nil :accessor tty-device-cm-end-string)
                                        ; takes device out of cursor motion mode.
   (cm-x-add-char :initarg :cm-x-add-char :initform nil :accessor tty-device-cm-x-add-char) ; char-code to unconditionally add to x coordinate.
   (cm-y-add-char :initarg :cm-y-add-char :initform nil :accessor tty-device-cm-y-add-char) ; char-code to unconditionally add to y coordinate.
   (cm-x-condx-char :initarg :cm-x-condx-char :initform nil :accessor tty-device-cm-x-condx-char) ; char-code threshold for adding to x coordinate.
   (cm-y-condx-char :initarg :cm-y-condx-char :initform nil :accessor tty-device-cm-y-condx-char) ; char-code threshold for adding to y coordinate.
   (cm-x-condx-add-char :initarg :cm-x-condx-add-char :initform nil :accessor tty-device-cm-x-condx-add-char) ; char-code to conditionally add to x coordinate.
   (cm-y-condx-add-char :initarg :cm-y-condx-add-char :initform nil :accessor tty-device-cm-y-condx-add-char) ; char-code to conditionally add to y coordinate.
   (cm-string1 :initarg :cm-string1 :initform nil :accessor tty-device-cm-string1)
                                        ; initial substring of cursor motion string.
   (cm-string2 :initarg :cm-string2 :initform nil :accessor tty-device-cm-string2)
                                        ; substring of cursor motion string between coordinates.
   (cm-string3 :initarg :cm-string3 :initform nil :accessor tty-device-cm-string3)
                                        ; substring of cursor motion string after coordinates.
   (cm-one-origin :initarg :cm-one-origin :initform nil :accessor tty-device-cm-one-origin)
                                        ; non-nil if need to add one to coordinates.
   (cm-reversep :initarg :cm-reversep :initform nil :accessor tty-device-cm-reversep)
                                        ; non-nil if need to reverse coordinates.
   (cm-x-pad :initarg :cm-x-pad :initform nil :accessor tty-device-cm-x-pad) ; nil, 0, 2, or 3 for places to pad.
                                        ; 0 sends digit-chars.
   (cm-y-pad :initarg :cm-y-pad :initform nil :accessor tty-device-cm-y-pad) ; nil, 0, 2, or 3 for places to pad.
                                        ; 0 sends digit-chars.
   (screen-image :initarg :screen-image :initform nil :accessor tty-device-screen-image)
                                        ; vector device-lines long of strings
                                        ; device-columns long.
   ;;
   ;; This terminal's baud rate, or NIL for infinite.
   (speed :initarg :speed :initform nil :type (or (unsigned-byte 24) null)
          :accessor tty-device-speed)))

(defun %make-tty-device (&rest initargs)
  (apply #'make-instance 'tty-device initargs))


;;;; Device screen hunks and window-group.

;;; Window groups are used to keep track of the old width and height of a group
;;; so that when a configure-notify event is sent, we can determine if the size
;;; of the window actually changed or not.
;;;
;;; Window groups belong to the X11 backend. --GB
(defstruct (window-group (:print-function %print-window-group)
                         (:constructor
                          make-window-group (xparent width height)))
  xparent
  width
  height)

(defun %print-window-group (object stream depth)
  (declare (ignore object depth))
  (format stream "#<Hemlock Window Group>"))

;;; Device-hunks are used to claim a piece of the screen and for ordering
;;; pieces of the screen.  Window motion primitives and splitting/merging
;;; primitives use hunks.  Hunks are somewhat of an interface between the
;;; portable and non-portable parts of screen management, between what the
;;; user sees on the screen and how Hemlock internals deal with window
;;; sequencing and creation.  Note: the echo area hunk is not hooked into
;;; the ring of other hunks via the next and previous fields.
;;;
(defclass device-hunk ()
  ((window
    :initarg :window
    :initform nil
    :accessor device-hunk-window
    :documentation "Window displayed in this hunk.")
   (position
    :initarg :position
    :initform nil
    :accessor device-hunk-position
    :documentation "Bottom Y position of hunk.")
   (height
    :initarg :height
    :initform nil
    :accessor device-hunk-height
    :documentation "Height of hunk in pixels or lines.")
   (next
    :initarg :next
    :initform nil
    :accessor device-hunk-next
    :documentation "Next hunks.")
   (previous
    :initarg :previous
    :initform nil
    :accessor device-hunk-previous
    :documentation "Previous hunks")
   (device
    :initarg :device
    :initform nil
    :accessor device-hunk-device
    :documentation "Display device hunk is on."))
  (:documentation
   "This structure is used internally by Hemlock's screen management system."))

(defmethod print-object ((object device-hunk) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D+~D~@[, ~S~]"
            (device-hunk-position object)
            (device-hunk-height object)
            (let* ((window (device-hunk-window object))
                   (buffer (if window (window-buffer window))))
              (if buffer (buffer-name buffer))))))


;;;; Some defsetfs:

(define-setf-expander value (var)
  "Set the value of a Hemlock variable, calling any hooks."
  (let ((svar (gensym)))
    (values
     ()
     ()
     (list svar)
     `(%set-value ',var ,svar)
     `(value ,var))))

(defsetf variable-value (name &optional (kind :current) where) (new-value)
  "Set the value of a Hemlock variable, calling any hooks."
  `(%set-variable-value ,name ,kind ,where ,new-value))

(defsetf variable-hooks (name &optional (kind :current) where) (new-value)
  "Set the list of hook functions for a Hemlock variable."
  `(%set-variable-hooks ,name ,kind ,where ,new-value))

(defsetf variable-documentation (name &optional (kind :current) where) (new-value)
  "Set a Hemlock variable's documentation."
  `(%set-variable-documentation ,name ,kind ,where ,new-value))

