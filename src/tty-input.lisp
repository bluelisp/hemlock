;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock-internals)

;;;; Editor input from a tty.

(defclass tty-editor-input (editor-input)
  ((fd :initarg :fd
       :accessor tty-editor-input-fd)))

(defun make-tty-editor-input (&rest args)
  (apply #'make-instance 'tty-editor-input args))

(defmethod get-key-event
    ((stream tty-editor-input) &optional ignore-abort-attempts-p)
  (%editor-input-method stream ignore-abort-attempts-p))

(defmethod unget-key-event (key-event (stream tty-editor-input))
  (un-event key-event stream))

(defmethod clear-editor-input ((stream tty-editor-input))
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
          (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
               *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

;;; Note that we never return NIL as long as there are events to be served with
;;; SERVE-EVENT.  Thus non-keyboard input (i.e. process output)
;;; effectively causes LISTEN to block until either all the non-keyboard input
;;; has happened, or there is some real keyboard input.
;;;
(defmethod listen-editor-input ((stream tty-editor-input))
  (process-editor-tty-input)
  nil)

(defvar *tty-translations* (make-hash-table :test #'equal))

(defun register-tty-translations ()
  (assert hemlock.terminfo:*terminfo*)
  (flet ((reg (string keysym)
           (let ((string (etypecase string
                           (character (string string))
                           (list (coerce string 'simple-string))
                           (string string))))
              (setf (gethash string *tty-translations*) keysym))))
    ;; KLUDGE: There seems to be no way get F1-F4 reliably transmit
    ;; things in the terminfo db, since some terminals transmit them
    ;; as if they were vt100 PF1-PF4, so, register these aliases here.
    ;; If they double as something else, that will override these.
    (reg '(#\Esc #\O #\P) #k"F1")
    (reg '(#\Esc #\O #\Q) #k"F2")
    (reg '(#\Esc #\O #\R) #k"F3")
    (reg '(#\Esc #\O #\S) #k"F4")
    ;; Terminfo definitions for F1-F12
    (reg hemlock.terminfo:key-f1 #k"F1")
    (reg hemlock.terminfo:key-f2 #k"F2")
    (reg hemlock.terminfo:key-f3 #k"F3")
    (reg hemlock.terminfo:key-f4 #k"F4")
    (reg hemlock.terminfo:key-f5 #k"F5")
    (reg hemlock.terminfo:key-f6 #k"F6")
    (reg hemlock.terminfo:key-f7 #k"F7")
    (reg hemlock.terminfo:key-f8 #k"F8")
    (reg hemlock.terminfo:key-f9 #k"F9")
    (reg hemlock.terminfo:key-f10 #k"F10")
    (reg hemlock.terminfo:key-f11 #k"F11")
    (reg hemlock.terminfo:key-f12 #k"F12")
    ;; Terminfo definitions for movement keys
    (reg hemlock.terminfo:key-up #k"Uparrow")
    (reg hemlock.terminfo:key-down #k"Downarrow")
    (reg hemlock.terminfo:key-right #k"Rightarrow")
    (reg hemlock.terminfo:key-left #k"Leftarrow")
    (reg hemlock.terminfo:key-home #k"Home")
    (reg hemlock.terminfo:key-end #k"End")
    (reg hemlock.terminfo:key-ic #k"Insert")
    (reg hemlock.terminfo:key-dc #k"Delete")
    (reg hemlock.terminfo:key-ppage #k"Pageup")
    (reg hemlock.terminfo:key-npage #k"Pagedown")
    (reg hemlock.terminfo:key-backspace #k"Backspace")

    (reg hemlock.terminfo:key-sr #k"Shift-Uparrow")
    (reg hemlock.terminfo:key-sf #k"Shift-Downarrow")
    (reg hemlock.terminfo:key-sright #k"Shift-Rightarrow")
    (reg hemlock.terminfo:key-sleft #k"Shift-Leftarrow")
    (reg hemlock.terminfo:key-shome #k"Shift-Home")
    (reg hemlock.terminfo:key-send #k"Shift-End")
    (reg hemlock.terminfo:key-sic #k"Shift-Insert")
    (reg hemlock.terminfo:key-sdc #k"Shift-Delete")
    (reg hemlock.terminfo:key-sprevious #k"Shift-Pageup")
    (reg hemlock.terminfo:key-snext #k"Shift-Pagedown")
    
    ;; Xterm definitions, not in terminfo.

    (reg "[1;5A" #k"Control-Uparrow")
    (reg "[1;5B" #k"Control-Downarrow")
    (reg "[1;5C" #k"Control-Rightarrow")
    (reg "[1;5D" #k"Control-Leftarrow")
    (reg "[1;5H" #k"Control-Home")
    (reg "[1;5F" #k"Control-End")
    (reg "[2;3~" #k"Control-Insert")
    (reg "[3;5~" #k"Control-Delete")
    (reg "[5;3~" #k"Control-Pageup")
    (reg "[6;3~" #k"Control-Pagedown")

    (reg "[1;3A" #k"Meta-Uparrow")
    (reg "[1;3B" #k"Meta-Downarrow")
    (reg "[1;3C" #k"Meta-Rightarrow")
    (reg "[1;3D" #k"Meta-Leftarrow")
    (reg "[1;3H" #k"Meta-Home")
    (reg "[1;3F" #k"Meta-End")
    (reg "[2;3~" #k"Meta-Insert")
    (reg "[3;3~" #k"Meta-Delete")
    (reg "[5;3~" #k"Meta-Pageup")
    (reg "[6;3~" #k"Meta-Pagedown")
    
    (reg "[1;6A" #k"Shift-Control-Uparrow")
    (reg "[1;6B" #k"Shift-Control-Downarrow")
    (reg "[1;6C" #k"Shift-Control-Rightarrow")
    (reg "[1;6D" #k"Shift-Control-Leftarrow")
    (reg "[1;6H" #k"Shift-Control-Home")
    (reg "[1;6F" #k"Shift-Control-End")
    (reg "[2;6~" #k"Shift-Control-Insert")
    (reg "[3;6~" #k"Shift-Control-Delete")
    (reg "[5;6~" #k"Shift-Control-PageUp")
    (reg "[6;6~" #k"Shift-Control-PageDown")
    (reg "[3;6~" #k"Shift-Control-Delete")

    (reg "[1;4A" #k"Shift-Meta-Uparrow")
    (reg "[1;4B" #k"Shift-Meta-Downarrow")
    (reg "[1;4C" #k"Shift-Meta-Rightarrow")
    (reg "[1;4D" #k"Shift-Meta-Leftarrow")
    (reg "[1;4H" #k"Shift-Meta-Home")
    (reg "[1;4F" #k"Shift-Meta-End")
    (reg "[2;4~" #k"Shift-Meta-Insert")
    (reg "[3;4~" #k"Shift-Meta-Delete")
    (reg "[5;4~" #k"Shift-Meta-PageUp")
    (reg "[6;4~" #k"Shift-Meta-PageDown")

    (reg "[1;7A" #k"Meta-Control-Uparrow")
    (reg "[1;7B" #k"Meta-Control-Downarrow")
    (reg "[1;7C" #k"Meta-Control-Rightarrow")
    (reg "[1;7D" #k"Meta-Control-Leftarrow")
    (reg "[1;7H" #k"Meta-Control-Home")
    (reg "[1;7F" #k"Meta-Control-End")
    (reg "[2;7~" #k"Meta-Control-Insert")
    (reg "[3;7~" #k"Meta-Control-Delete")
    (reg "[5;7~" #k"Meta-Control-PageUp")
    (reg "[6;7~" #k"Meta-Control-PageDown")

    (reg "[1;8A" #k"Shift-Meta-Control-Uparrow")
    (reg "[1;8B" #k"Shift-Meta-Control-Downarrow")
    (reg "[1;8C" #k"Shift-Meta-Control-Rightarrow")
    (reg "[1;8D" #k"Shift-Meta-Control-Leftarrow")
    (reg "[1;8H" #k"Shift-Meta-Control-Home")
    (reg "[1;8F" #k"Shift-Meta-Control-End")
    (reg "[2;8~" #k"Shift-Meta-Control-Insert")
    (reg "[3;8~" #k"Shift-Meta-Control-Delete")
    (reg "[5;8~" #k"Shift-Meta-Control-PageUp")
    (reg "[6;8~" #k"Shift-Meta-Control-PageDown")

    ;; Misc.
    ;;
    ;; Not #\return, because then C-j turns into return aka C-m.
    ;; Is this translation needed at all?
    (reg #\newline #k"Linefeed")
    ;;
    (reg #\tab #k"Tab")
    (reg #\escape #k"Escape")
    ;; Kludge: This shouldn't be needed, but otherwise C-c M-i doesn't work.
    (reg '(#\Esc #\i) #k"meta-i")))

(defun translate-tty-event (data)
  (let ((string (coerce data 'string)))
    (or (gethash string *tty-translations*)
        (when (= 1 (length string))
          (hemlock-ext:char-key-event (char string 0))))))

(defun tty-key-event (data)
  (loop with start = 0
        with length = (length data)
        while (< start length)
        do (loop for end from length downto (1+ start)
                 do (let ((event (translate-tty-event (subseq data start end))))
                      (when event
                        (q-event *real-editor-input* event)
                        (setf start end)
                        (return))))))

