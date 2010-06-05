;;; Copyright (c) 2003, 2004 Nikodemus Siivola, Julian Squires
;;; Integrated into Hemlock 2010 by David Lichteblau
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :hi)


;;;; LINEDIT-DEVICE: a minimal backend implemented as a subclass of TTY-DEVICE

(pushnew :mini hi::*available-backends*)

(defclass linedit-device (tty-device)
  ((hbuf :accessor hbuf
	 :initarg :hbuf)
   (start :initform 0
	  :accessor get-start)
   (dirty-p :initform t
	    :accessor dirty-p)
   (old-point :initform 0
	      :accessor old-point)
   (old-point-col :initform 0
		  :accessor old-point-col)
   (old-point-row :initform 1
		  :accessor old-point-row)
   (old-string :initform ""
	       :accessor old-string)
   (old-markup :initform 0
	       :accessor old-markup)
   (completer :reader editor-completer
	      :initform 'lisp-complete
	      :initarg :complete)
   (history :reader editor-history
	    :initform (make-instance 'linedit-history)
	    :initarg :history)
   (prompt :accessor editor-prompt
	   :initform ""
	   :initarg :prompt)))

(defun make-linedit-device (name)
  (change-class (make-tty-device name) 'linedit-device))

(defmethod device-show-mark ((device linedit-device) window x y time)
  (declare (ignore window x y time)))

;;; (defmethod device-finish-output ((device linedit-device) window)
;;;   (declare (ignore window))
;;;   )

(defmethod device-dumb-redisplay ((device linedit-device) window)
  (mini-redisplay device window))

(defmethod device-smart-redisplay ((device linedit-device) window)
  (mini-redisplay device window))

(defun mini-redisplay (device window)
  (declare (ignorable device))
  ;; only ever display the current window:
  (when (eq window *current-window*)
    ;; this function gets defined by linedit:
    (%redraw-line-with-markup (hi:window-buffer window)))

  ;; tell the redisplay algorithm that we did our job, otherwise it
  ;; retries forever:
  (let* ((first (window-first-line window))
         ;; (hunk (window-hunk window))
         #+nil (device (device-hunk-device hunk)))
    (setf (window-first-changed window) the-sentinel
          (window-last-changed window) first)))

(defmethod device-exit ((device linedit-device))
  (device-write-string (tty-device-cm-end-string device))
  (device-force-output device)
  (reset-input))

(defmethod device-clear ((device linedit-device))
  )

(defmethod device-put-cursor ((device linedit-device) hunk x y)
  (declare (fixnum x y))
  )


;;;; Linedit support

(defun buffer-contains-complete-form-p ()
  (let ((str (hi:region-to-string (hi:buffer-region (current-buffer)))))
    (or (ppcre:all-matches "^\\s*$" str) ;including empty string
        (ppcre:all-matches "^," str)     ;slime-style repl hack
        (handler-case
            (let ((*read-eval* nil))
              (read-from-string str)
              t)
          (error ()
            nil)))))

(defcommand "Finish Linedit" (p) "" ""
  (declare (ignore p))
  (when (and (find "Lisp" (buffer-modes (current-buffer)) :test #'string=)
             (not (buffer-contains-complete-form-p)))
    (editor-error "Not a complete form"))
  (hemlock::save-all-files-and-exit-command nil))

(defcommand "Illegal Linedit Command" (p) "" ""
  (declare (ignore p))
  (editor-error "Command not available"))

(defun empty-region-p (r)
  (hemlock::mark= (hemlock::region-start r) (hemlock::region-end r)))

(defcommand "Linedit Delete Or Eof" (p) "" ""
  (if (empty-region-p (hi:buffer-region (current-buffer)))
      (progn
        (hemlock::save-all-files-command nil)
        (throw 'linedit-eof nil))
      (hemlock::delete-next-character-command p)))

(defcommand "Linedit Clear Screen" (p) "" ""
  (device-write-string hemlock.terminfo:clear-screen))

(defun install-linedit-keys (buffer)
  (bind-key "Finish Linedit" #k"return" :buffer buffer)
  (bind-key "Finish Linedit" #k"control-m" :buffer buffer)
  (bind-key "Finish Linedit" #k"control-j" :buffer buffer)
  (bind-key "Linedit Delete Or Eof" #k"control-d" :buffer buffer)
  (bind-key "Linedit Clear Screen" #k"control-l" :buffer buffer)
  (bind-key "Linedit Previous History Entry" #k"control-p" :buffer buffer)
  (bind-key "Linedit Next History Entry" #k"control-n" :buffer buffer)
  (bind-key "Linedit Previous History Entry" #k"meta-p" :buffer buffer)
  (bind-key "Linedit Next History Entry" #k"meta-n" :buffer buffer)
  (bind-key "Illegal Linedit Command" #k"control-x" :buffer buffer)
  (bind-key "Illegal Linedit Command" #k"control-meta-c" :buffer buffer)
  (bind-key "Illegal Linedit Command" #k"control-meta-s" :buffer buffer)
  (bind-key "Illegal Linedit Command" #k"control-meta-l" :buffer buffer)
  (bind-key "Illegal Linedit Command" #k"meta-x" :buffer buffer)
  (bind-key "Linedit Isearch" #k"control-r" :buffer buffer)
  (bind-key "Linedit Inverted Isearch" #k"control-s" :buffer buffer)
  (bind-key "Illegal Linedit Command" #k"hyper-t" :buffer buffer)
  (bind-key "Illegal Linedit Command" #k"middledown" :buffer buffer)
  (bind-key "Do Nothing" #k"middleup" :buffer buffer)
  (bind-key "Illegal Linedit Command" #k"super-leftdown" :buffer buffer)
  (bind-key "Do Nothing" #k"super-leftup" :buffer buffer)
  (bind-key "Illegal Linedit Command" #k"super-rightdown" :buffer buffer)
  (bind-key "Do Nothing" #k"super-rightup" :buffer buffer)
  (bind-key "Linedit Complete" #k"control-i" :buffer buffer)
  (bind-key "Linedit Complete" #k"tab" :buffer buffer))



;;; Various utilities

(defparameter *word-delimiters* "()[]{}',` \"")

(defun word-delimiter-p (char)
  (declare (simple-string *word-delimiters*)
	   (character char))
  (find char *word-delimiters*))

(defun at-delimiter-p (string index)
  (and (< index (length string)) 
       (word-delimiter-p (char string index))))

(defun min* (&rest args)
  "Like min, except ignores NILs."
  (apply #'min (remove-if #'null args)))

(defmacro do-internal-symbols ((var package) &body forms)
  (let ((state (gensym)))
    `(do-symbols (,var ,package)
       (multiple-value-bind (,var ,state)
	   (find-symbol (symbol-name ,var) ,package)
	 (when (eq ,state :internal)
	   ,@forms)))))



;;;;
;;;; Buffer management and initialization hacks
;;;;

(defun initialize-linedit (instance string point modes)
  (setf (hbuf instance)
	(iter
	  (for i from 1)
	  (let ((buf (make-buffer (format nil "*linedit-~D*" i)
				  :modes modes)))
	    (when buf
	      (return buf)))))
  (install-linedit-keys (hbuf instance))
  (defhvar "Indent with Tabs" ""
    :buffer (hbuf instance)
    :value #'hemlock::indent-using-spaces)
  (defhvar "Indent Function" ""
    :buffer (hbuf instance)
    :value #'hemlock::spaces-to-tab-stop)
  (when string (setf (get-string instance) string))
  (when point (setf (get-point instance) point)))

(defmethod get-string ((editor linedit-device))
  (region-to-string (buffer-region (hbuf editor))))

(defmethod (setf get-string) (string (editor linedit-device))
  (let ((r (buffer-region (hbuf editor)))
	(old-point (get-point editor)))
    (delete-region r)
    (insert-string (region-start r) string)
    (setf (get-point editor) (min old-point (length string))))
  string)

(defmethod get-point ((editor linedit-device))
  (let ((mark (buffer-point (hbuf editor))))
    (+ (mark-charpos mark)
       (let ((line (line-previous (mark-line mark))))
	 (if line
	     (iter
	       (while line)
	       (summing (line-length line))
	       (setf line (line-previous line)))
	     0)))))

(defmethod (setf get-point) (point (editor linedit-device))
  (when (<= 0 point (length (get-string editor)))
    (let ((mark (buffer-point (hbuf editor))))
      (buffer-start mark)
      (character-offset mark point))))


(defmethod backend-columns ((backend linedit-device))
  ;; fixme: as the hemlock backend
  80)

(defmethod backend-lines ((backend linedit-device))
  25)

(defun read-chord ()
  (hemlock-ext:key-event-char
   (next-key-event (get-key-event *editor-input* t))))

(defmethod page ((backend linedit-device))
  (write-string "--more--")
  (force-output)
  (let ((q (read-chord backend)))
    (write-char #\Return)
    (not (equal #\q q))))

;;; FIXME: Explicit line-wrap needed
(defmethod print-in-columns ((backend linedit-device) list &key width)
  (let ((max-col (truncate (backend-columns backend) width))
	(col 0)
	(line 0)
	(pad nil))
    (newline backend)
    (dolist (item list)
      (incf col)
      ;; Padding
      (when pad
	(write-string pad)
	(setf pad nil))
      ;; Item
      (write-string item)
      ;; Maybe newline
      (cond ((= col max-col)
	     (newline backend)
	     (setf col 0)
	     (when (= (1+ (incf line)) (backend-lines backend))
	       (setf line 0)
	       (unless (page backend)
		 (return-from print-in-columns nil))))
	    (t 
	     (setf pad (make-string (- width (length item)) 
				    :initial-element #\space)))))
    ;; Optional newline
    (when pad
      (newline backend))))

(defmethod print-in-lines ((backend linedit-device) string)
  (newline backend)
  (do ((i 0 (1+ i))
       (lines 0))
      ((= i (length string)))
    (let ((c (schar string i)))
      (when (= lines (backend-lines backend))
	(setf lines 0)
	(unless (page backend)
	  (return-from print-in-lines nil)))
      (when (eql #\newline c)
	(incf lines))
      (write-char c)))
  (newline backend))

(defmethod newline ((backend linedit-device))
  (setf (dirty-p backend) t)
  (write-char #\newline)
  (write-char #\return)
  (force-output))


;;;; Linedit device redraw
;;;;
;;;; Redraw of a buffer to the TTY.  Differs from the real tty backend
;;;; in the following ways:
;;;;
;;;;  - Displays only one buffer at a time.
;;;;    (The "line" being edited.)
;;;;
;;;;  - Uses relative movement only (!), never absolute movement.
;;;;    In a REPL situation, we don't even know where on the screen we
;;;;    we are; movement relative to the starting position is the only
;;;;    concept we have.
;;;;
;;;;  - While it can display multi-line buffer contents (and being able
;;;;    to do so is a feature of major importance for a Lisp REPL...)
;;;;    we do not at the moment expect many such lines, so we don't
;;;;    optimize for long buffers, and do not support buffers larger
;;;;    than the screen at the moment.

(defun set-column-address (n current)
  ;; ti:column-address doesn't seem to be supported on revelant terminals.
  (cond ((< n current)
	 (loop repeat (- current n) 
	    do (hemlock.terminfo:tputs hemlock.terminfo:cursor-left)))
	((> n current)
	 (loop repeat (- n current) 
	    do (hemlock.terminfo:tputs hemlock.terminfo:cursor-right)))))

(defun find-row-and-col
    (region-string columns &optional (end (length region-string)))
  (let ((col 0)
	(row 0))
    (iter
      (for c in-vector region-string)
      (repeat end)
      (cond
	((or (eql c #\newline) (eql c #\return))
	 (incf row (1+ (floor (1+ col) columns)))
	 (setf col 0))
	(t
	 (incf col))))
    (values ;; 1+ includes point in row calculations
     (+ row (floor (1+ col) columns))
     (rem col columns))))

(defun move-cursor (&key col vertical clear-to-eos current-col)
  (cond
    ((>= vertical 0)
     (loop repeat vertical do (hemlock.terminfo:tputs hemlock.terminfo:cursor-up))
     (set-column-address col current-col))
    (t
     (loop repeat (abs vertical) do (hemlock.terminfo:tputs hemlock.terminfo:cursor-down))
     (set-column-address col 0)))
  (when clear-to-eos
    (hemlock.terminfo:tputs hemlock.terminfo:clr-eos)))

(defun find-col (str columns &optional (end (length str)))
  (nth-value 1 (find-row-and-col str columns end)))

(defun fix-wraparound (start end columns str)
  ;; If final character ended in the last column the point
  ;; will wrap around to the first column on the same line:
  ;; hence move down if so.
  (when (and (< start end) (zerop (find-col str columns end)))
    (hemlock.terminfo:tputs hemlock.terminfo:cursor-down)))

;;; (defun play ()
;;;   (iter
;;;     (let ((char (read-char *terminal-io*)))
;;;       (case char
;;; 	((#\x #\y)
;;; 	 (write-char char *terminal-io*))
;;; 	((#\p #\k)
;;; 	 (hemlock.terminfo:tputs hemlock.terminfo:cursor-up))
;;; 	((#\n #\j)
;;; 	 (hemlock.terminfo:tputs hemlock.terminfo:cursor-down))
;;; 	((#\f #\l)
;;; 	 (hemlock.terminfo:tputs hemlock.terminfo:cursor-right))
;;; 	((#\b #\h)
;;; 	 (hemlock.terminfo:tputs hemlock.terminfo:cursor-left))
;;; 	(#\s (hemlock.terminfo:tputs hemlock.terminfo:clr-eos))
;;; 	(#\e (hemlock.terminfo:tputs hemlock.terminfo:clr-eol))
;;; 	(#\q
;;; 	 (return)))
;;;       (force-output *terminal-io*))))

(defun mini-write-string (str start-col width)
  (let ((col start-col))
    (iter
      (for c in-vector str)
      (cond
	((member c '(#\newline #\return))
	 (force-output *terminal-io*)
	 (hemlock.terminfo:tputs hemlock.terminfo:cursor-down)
	 (setf col 0))
	((< (char-code c) 32)
	 (write-char #\? *terminal-io*)
	 (incf col))
	(t
	 (write-char c *terminal-io*)
	 (incf col))))
    (force-output *terminal-io*)
    (rem col width)))

(defun display (backend &key prompt line point markup)
  (declare (ignore markup))
  (let* ( ;; SBCL and CMUCL traditionally point *terminal-io* to /dev/tty,
         ;; and we do output on it assuming it goes to STDOUT. Binding
         ;; *terminal-io* is unportable, so do it only when needed.
;;;          #+(or sbcl cmu)
;;;            (*terminal-io* *standard-output*)
	 (columns (backend-columns backend))
	 (old-point (old-point backend))
	 (old-col (old-point-col backend))
	 (old-row (old-point-row backend))
	 (old (old-string backend))
	 (new (concat prompt line))
	 (point  (+ point (length prompt))))
    (let* ((end (length new))
	   (rows (find-row-and-col old columns)))
      (when (dirty-p backend)
	(setf old-point 0
	      old-col 0
	      old-row 0))
      (multiple-value-bind (point-row point-col)
	  (find-row-and-col new columns point)
	(let* ((diff 0 #+nil (mismatch new old))
	       (start (min* old-point point diff end)))
	  (multiple-value-bind (start-row start-col)
	      (find-row-and-col new columns start)
	    (multiple-value-bind (row col)
		(find-row-and-col new columns)
	      (multiple-value-bind (point-row point-col)
		  (find-row-and-col new columns point)
		(move-cursor
		 :col 0
		 :vertical old-row
		 :current-col old-col
		 :clear-to-eos t)
		(mini-write-string new start-col columns)
		(move-cursor
		 :col point-col
		 :vertical (- row point-row)
		 :current-col col)
		(setf	(old-point-col backend) point-col
			(old-point-row backend) point-row
			(dirty-p backend) nil)))))))
    (force-output *terminal-io*)))



;;;; LINEDIT-HISTORY

;;; Used to implement the history of line, the class LINEDIT-HISTORY
;;; offers a simple browsable from of storage.
;;;
;;; It used to be called BUFFER in linedit, but was renamed to reduce
;;; confusion with hemlock buffers.

(defclass linedit-history ()
  ((prev :accessor %linedit-history-prev :initform nil)
   (next :accessor %linedit-history-next :initform nil)
   (list :accessor %linedit-history-list :initform nil)))

(defun linedit-history-push (string linedit-history)
  (push string (%linedit-history-list linedit-history))
  (setf (%linedit-history-next linedit-history)
	nil)
  (setf (%linedit-history-prev linedit-history)
	(%linedit-history-list linedit-history)))

(defun linedit-history-previous (string linedit-history)
  (when (%linedit-history-prev linedit-history)
    (push string (%linedit-history-next linedit-history))
    (pop (%linedit-history-prev linedit-history))))

(defun linedit-history-peek (linedit-history)
  (car (%linedit-history-prev linedit-history)))

(defun linedit-history-next (string linedit-history)
  (when (%linedit-history-next linedit-history)
    (push string (%linedit-history-prev linedit-history))
    (pop (%linedit-history-next linedit-history))))

(defun linedit-history-cycle (linedit-history)
  (flet ((wrap-linedit-history ()
	   (unless (%linedit-history-prev linedit-history)
	     (setf (%linedit-history-prev linedit-history)
		   (nreverse (%linedit-history-next linedit-history))
		   (%linedit-history-next linedit-history) nil))))
    (wrap-linedit-history)
    (push (pop (%linedit-history-prev linedit-history))
	  (%linedit-history-next linedit-history))
    (wrap-linedit-history)
    t))

;; Used before and after isearch to avoid bad interaction between isearch
;; and other history commands:
(defun reset-linedit-history-position (linedit-history)
  "Re-arrange prev and next so that we are back to the most recent item."
  (setf (%linedit-history-prev linedit-history)
	(append (reverse (%linedit-history-next linedit-history))
		(%linedit-history-prev linedit-history)))
  (setf (%linedit-history-next linedit-history)
	nil))


;;;; stuff from editor.lisp, pending refactoring

(defun %redraw-line-with-markup (buffer)
  (display (current-device)
	   :prompt (editor-prompt (current-device))
	   :line (region-to-string (buffer-region buffer))
	   :point (let ((mark (buffer-point buffer)))
		    (+ (mark-charpos mark)
		       (let ((line (line-previous (mark-line mark))))
			 (if line
			     (iter
			       (while line)
			       (summing (1+ (line-length line)))
			       (setf line (line-previous line)))
			     0))))
	   :markup t))

(defun get-finished-string (editor)
  (let ((str (get-string editor)))
    (linedit-history-push str (editor-history editor))
    (newline editor)
    (save-to-history-file str)
    str))

(defun read-history-file (editor)
  (handler-case
      (with-open-file (s (merge-pathnames ".linedit-history"
					  (user-homedir-pathname))
			 :external-format :utf-8
			 :direction :input
			 :if-does-not-exist nil)
	(when s
	  (let ((history (editor-history editor)))
	    (iter
	      (let ((count-line (read-line s nil)))
		(while count-line)
		(let* ((count (parse-integer count-line))
		       (buf (make-string count)))
		  (read-sequence buf s)
		  (linedit-history-push buf history)
		  (assert (eql #\newline (read-char s)))))))))
    (file-error (c)
      (format t "Ignoring error ~A while reading history.~%" c))))

(defun save-to-history-file (str)
  (handler-case
      (with-open-file (s (merge-pathnames ".linedit-history"
					  (user-homedir-pathname))
			 :external-format :utf-8
			 :direction :output
			 :if-does-not-exist :create
			 :if-exists :append)
	(format s "~D~%~A~%" (length str) str))
    (file-error (c)
      (format t "Ignoring error ~A while writing history.~%" c))))

(defmacro with-editor-point-and-string (((point string) editor) &body forms)
  `(let ((,point (get-point ,editor))
	 (,string (get-string ,editor)))
     ,@forms))

(defun editor-word-start (editor)
  "Returns the index of the first letter of current or previous word,
if the point is just after a word, or the point."
  (with-editor-point-and-string ((point string) editor)
    (if (or (not (at-delimiter-p string point))
	    (not (and (plusp point) (at-delimiter-p string (1- point)))))
	(1+ (or (position-if 'word-delimiter-p string :end point :from-end t) 
		-1)) ; start of string
	point)))

(defun editor-previous-word-start (editor)
  "Returns the index of the first letter of current or previous word,
if the point was at the start of a word or between words."
  (with-editor-point-and-string ((point string) editor)
    (let ((tmp (cond ((at-delimiter-p string point)
		      (position-if-not 'word-delimiter-p string 
				       :end point :from-end t))
		     ((and (plusp point) (at-delimiter-p string (1- point)))
		      (position-if-not 'word-delimiter-p string
				       :end (1- point) :from-end t))
		     (t point))))
      ;; tmp is always in the word whose start we want (or NIL)
      (1+ (or (position-if 'word-delimiter-p string 
			   :end (or tmp 0) :from-end t) 
	      -1)))))

(defun editor-word-end (editor)
  "Returns the index just beyond the current word or the point if
point is not inside a word."
  (with-editor-point-and-string ((point string) editor)
    (if (at-delimiter-p string point)
	point
	(or (position-if 'word-delimiter-p string :start point)
	    (length string)))))

(defun editor-next-word-end (editor)
  "Returns the index just beyond the last letter of current or next
word, if the point was between words."
  (with-editor-point-and-string ((point string) editor)
    (let ((tmp (if (at-delimiter-p string point)
		   (or (position-if-not 'word-delimiter-p string
					:start point)
		       (length string))
		   point)))
      ;; tmp is always in the word whose end we want (or already at the end)
      (or (position-if 'word-delimiter-p string :start tmp)
	  (length string)))))

(defun editor-word (editor)
  "Returns the current word the point is in or right after, or an
empty string."
  (let ((start (editor-word-start editor))
	(end (editor-word-end editor)))
    #+nil(dbg "~&editor-word: ~S - ~S~%" start end)
    (subseq (get-string editor) start end)))

(defun editor-complete (editor)
  (funcall (editor-completer editor) (editor-word editor) editor))

(defun editor-replace-word (editor word)
  (with-editor-point-and-string ((point string) editor)
    (let ((start (editor-word-start editor))
	  (end (editor-word-end editor)))
      (setf (get-string editor)
	    (concat (subseq string 0 start) word (subseq string end))
	    (get-point editor) (+ start (length word))))))

(defun quoted-p (string index)
  (let ((quoted-p nil))
    (dotimes (n (min index (length string)) quoted-p)
      (when (eql (schar string n) #\")
       (setf quoted-p (not quoted-p))))))

(defun in-quoted-string-p (editor)
  (quoted-p (get-string editor) (get-point editor)))


;;;; stuff from main.lisp

(defvar *linedit-result* nil)

(defun window-device (window)
  (device-hunk-device (window-hunk window)))

(defun current-device ()
  (window-device (current-window)))

(defun linedit (&key modes initial-string initial-point (prompt ""))
  "Reads a single line of input with line-editing."
  (let ((editor nil))
    (hemlock:with-editor (:backend-type :mini :load-user-init nil)
      (setf editor (current-device))
      (setf (editor-prompt editor) prompt)
      (initialize-linedit editor initial-string initial-point modes)
      (read-history-file editor)
      (change-to-buffer (hbuf editor))
      (let ((*invoke-hook*
	     (lambda (command p)
	       (handler-case
		   (multiple-value-prog1
		       (funcall (command-function command) p)
		     (unless (eq *current-buffer* (hbuf editor))
		       (change-to-buffer (hbuf editor))
		       (editor-error
			"Invalid buffer found, resetting.")))
		 (editor-error (c)
		   (handler-case
		       (format *terminal-io* "~&~A~%" c)
		     (error (d)
		       (format *terminal-io*
			       "~&error ~A while printing error ~A~%"
			       (type-of d)
			       (type-of c))))
		   (setf (dirty-p editor) t))))))
	(command-loop)))
    (get-finished-string editor)))

(defun formedit (&rest keys)
  (apply #'linedit :modes '("Lisp") keys))

(defun read-interactively (&optional (error-on-eof t) eof-value)
  (read-from-string (formedit) error-on-eof eof-value))

;; it may not look like it, but this is the main exported function in
;; this file...:
(defun repl ()
  ;; FIXME: should check whether a tty is available, and fall back to
  ;; non-linedit usage otherwise.
  (let* ((prompt "")
	 (prepl::*read-command*
	  (lambda (stream)
	    (declare (ignore stream))
	    (block t
	      (catch 'linedit-eof
		(return-from t
		  (with-input-from-string (s (formedit :prompt prompt))
		    (prepl::read-command s))))
	      (terpri)
	      prepl::*eof-command*)))
	 (real-prompt-fun prepl::*prompt*)
	 (prepl::*prompt*
	  (lambda (&rest junk)
	    (declare (ignore stream))
	    (setf prompt
		  (string-trim (list #\newline)
			       (with-output-to-string (s)
				 (let ((prepl::*prompt* real-prompt-fun))
				   (prepl::prompt s)))))
	    "")))
    (prepl:repl)))

(defun advance-history (direction)
  (let* ((editor (current-device))
	 (line (funcall direction
			(get-string editor)
			(editor-history editor))))
    (cond
      (line
       (setf (get-string editor) line)
       (hemlock::goto-buffer-end))
      (t
       (beep)))))

(defcommand "Linedit Previous History Entry" (p) "" ""
  (declare (ignore p))
  (advance-history #'linedit-history-previous))

(defcommand "Linedit Next History Entry" (p) "" ""
  (declare (ignore p))
  (advance-history #'linedit-history-next))


;;;; Underlying functions for pathname completion

(defun pathname-directory-pathname (pathname)
  (make-pathname :name nil :type nil
		 :defaults pathname))

(defun underlying-directory-p (pathname)
  (case (file-kind pathname)
    (:directory t)
    (:symbolic-link 
     (file-kind (merge-pathnames (read-link pathname) pathname)))))

(defun logical-pathname-p (pathname)
  (typep (pathname pathname) 'logical-pathname))

(defun logical-pathname-complete (string)
  (values (list string) (length string)))

;;; We can't easily do zsh-style tab-completion of ~us into ~user, but
;;; at least we can expand ~ and ~user.  The other bug here at the
;;; moment is that ~nonexistant will complete to the same as ~.
(defun tilde-expand-string (string)
  "Returns the supplied string, with a prefix of ~ or ~user expanded
to the appropriate home directory."
  (if (and (> (length string) 0)
	   (eql (schar string 0) #\~))
      (flet ((chop (s) 
	       (subseq s 0 (1- (length s)))))
	(let* ((slash-index (loop for i below (length string)
				  when (eql (schar string i) #\/) 
				  return i))
	       (suffix (and slash-index (subseq string slash-index)))
	       (uname (subseq string 1 slash-index))
	       (homedir (or (cdr (assoc :home (user-info uname)))
			    (chop (namestring 
				   (or (probe-file (user-homedir-pathname))
				       (return-from tilde-expand-string 
					 string)))))))
	  (concatenate 'string homedir (or suffix ""))))
      string))

(defun directory-complete (string)
  (declare (simple-string string))
  (let* ((common nil)
	 (all nil)
	 (max 0)
	 (string (tilde-expand-string string))
	 (dir (pathname-directory-pathname string))
	 (namefun (if (relative-pathname-p string)
		      #'namestring
		      (lambda (x) (namestring (merge-pathnames x))))))
    (unless (and (underlying-directory-p dir)
		 (not (wild-pathname-p dir)))
      (return-from directory-complete (values nil 0)))
    (with-directory-iterator (next dir)
      (loop for entry = (next)
	    while entry
	    do (let* ((full (funcall namefun entry))
		      (diff (mismatch string full)))
		 #+nil (dbg "~& completed: ~A, diff: ~A~%" full diff)
		 (unless (and diff (< diff (length string)))
		   #+nil(dbg "~& common ~A mismatch ~A~&" common 
			(mismatch common full))
		   (setf common (if common
				    (subseq common 0 (mismatch common full))
				    full)
			 max (max max (length full))
			 all (cons full all))))))
    #+nil(dbg "~&common: ~A~%" common)
    (if (or (null common)
	    (<= (length common) (length string)))
	(values all max)
	(values (list common) (length common)))))

;;;; Functions for Lisp completion

(defun lisp-complete (string editor)
  (declare (simple-string string))
  (when (plusp (length string))
    (if (in-quoted-string-p editor)
	(if (logical-pathname-p string)
	    (logical-pathname-complete string)
	    (directory-complete string))
	(let* ((length (length string))
	       (first-colon (position #\: string))
	       (last-colon (position #\: string :from-end t))
	       (state (and first-colon
			   (if (< first-colon last-colon)
			       :internal
			       :external)))
	       (package (and first-colon
			     (find-package (if (plusp first-colon)
					       (string-upcase
						(subseq string 0 first-colon))
					       :keyword))))
	       (hash (make-hash-table :test #'equal))
	       (common nil)
	       (max-len 0))
       
	  (labels ((stringify (symbol)
		     (if (upper-case-p (schar string 0))
			 (string symbol)
			 (string-downcase (string symbol))))
		   (push-name (name)
		     (setf common (if common
				      (subseq name 0 (mismatch common name))
				      name)
			   max-len (max max-len (length name))
			   (gethash name hash) name))
		   (select-symbol (symbol match)
		     (let ((name (stringify symbol))
			   (end (length match)))
		       (when (and (> (length name) end)	; Skip indetical
				  (equal match (subseq name 0 end)))
			 (push-name (concat string (subseq name end)))))))
	    ;; Skip empty strings
	    (when (plusp length)
	      (if package
		  ;; Symbols with explicit package prefixes.
		  (let* ((start (1+ last-colon))
			 (match (subseq string start)))
		    (ecase state
		      (:internal (do-internal-symbols (sym package)
				   (select-symbol sym match)))
		      (:external (do-external-symbols (sym package)
				   (select-symbol sym match)))))
		
		  ;; Symbols without explicit package prefix + packges
		  (dolist (package (list-all-packages))
		    (if (eq *package* package)
			(do-symbols (sym)
			  (select-symbol sym string))
			;; Package names
			(dolist (name (cons (package-name package)
					    (package-nicknames package)))
			  (select-symbol name string))))))

	    ;; Return list of matches to caller
	    (if (> (length common) (length string))
		(values (list common) (length common))
		(let (list)
		  (maphash (lambda (key val)
			     (declare (ignore val))
			     (push key list))
			   hash)
		  (values list max-len))))))))


;;;;
;;;; Command definitions -- for completion and other features.
;;;;

;; FIXME: there used to be the help command seen below.  These days, an
;; implementation based on or similar to Describe Mode should replace
;; it.
#+nil
(defun help (chord editor)
  (declare (ignore chord))
  (let ((pairs nil)
	(max-id 0)
	(max-f 0))
    (maphash (lambda (id function)
	       (let ((f (string-downcase (symbol-name function))))
		 (push (list id f) pairs)
		 (setf max-id (max max-id (length id))
		       max-f (max max-f (length f)))))
	     (editor-commands editor))
    (print-in-columns editor
		      (mapcar (lambda (pair)
				 (destructuring-bind (id f) pair
				   (with-output-to-string (s)
				     (write-string id s)
				     (loop repeat (- (1+ max-id) (length id))
					   do (write-char #\Space s))
				     (write-string f s))))
			      (nreverse pairs))
		      :width (+ max-id max-f 2))))

(defun complete (chord editor)
  (declare (ignore chord))
  (multiple-value-bind (completions max-len) (editor-complete editor)
    (if completions
	(if (not (cdr completions))
	    (editor-replace-word editor (car completions))
	    (print-in-columns editor completions :width (+ max-len 2)))
	(beep))))

(defun apropos-word (chord editor)
  (declare (ignore chord))
  (let* ((word (editor-word editor))
	 (apropi (apropos-list word)))
    (if (null apropi)
	(beep)
	(let* ((longest 0)
	       (strings (mapcar (lambda (symbol)
				  (declare (symbol symbol))
				  (let ((str (prin1-to-string symbol)))
				    (setf longest (max longest (length str)))
				    (string-downcase str)))
				apropi)))
	  (print-in-columns editor strings :width (+ longest 2))))))

(defun describe-word (chord editor)
  (declare (ignore chord))
  (print-in-lines editor
		  (with-output-to-string (s)
		    (describe (find-symbol (string-upcase
					    (editor-word editor))) s))))

(defcommand "Linedit Complete" (p) "" ""
  (complete nil (current-device)))

(defcommand "Linedit Isearch" (p)
    "" ""
  (declare (ignore p))
  (line-isearch :backward))

(defcommand "Linedit Inverted Isearch" (p)
    "" ""
  (declare (ignore p))
  (line-isearch :forward))

(defun update-prompt-for-line-isearch (string direction failure)
  (setf (editor-prompt (current-device))
	(format nil
		"(~@[~A~]~:[reverse-i-search~;i-search~]:~A) "
		failure (eq direction :forward) string)))


;;;; Incremental search over the line history
;;;; ... most of which is copy&paste from real incremental search,
;;;; in order to support proper searching among multiple hits within
;;;; the same line.  But in addition, we magically switch buffer containts
;;;; on search failures, to find the next hit in the history instead.

(defun line-isearch (direction)
  (let ((editor (current-device)))
    (reset-linedit-history-position (editor-history editor))
    (let ((saved-prompt (editor-prompt editor)))
      (setf (last-command-type) nil)
      (update-prompt-for-line-isearch "" direction nil)
      (let* ((point (current-point))
	     (save-start (copy-mark point :temporary))
	     (saved-buffer (get-string editor)))
	(with-mark ((here point))
	  (when (eq (catch 'exit-i-search
		      (%line-isearch "" point here direction nil))
		    :control-g)
	    (setf (get-string editor)
		  saved-buffer)
	    (move-mark point save-start)
	    (invoke-hook abort-hook)
	    (setf (editor-prompt editor)
		  saved-prompt)
	    (editor-error ""))
	  (if (region-active-p)
	      (delete-mark save-start)
	      (push-buffer-mark save-start)))))
    (reset-linedit-history-position (editor-history editor))))

(defun %line-isearch (string point trailer direction failure)
  (do* ((curr-point point (copy-mark point :temporary))
	(curr-trailer (copy-mark trailer :temporary)))
       (nil)
    (let ((next-key-event (get-key-event *editor-input* t)))
      (case (%line-isearch-char-eval next-key-event
				     string
				     point
				     trailer
				     direction
				     failure)
        (:cancel
         (update-prompt-for-line-isearch string direction failure)
         (unless (zerop (length string))
           (hemlock::i-search-pattern string direction)))
        (:return-cancel
         (unless (zerop (length string)) (return :cancel))
         (beep))
        (:control-g
         (when failure (return :control-g))
         (update-prompt-for-line-isearch string direction nil)
         (unless (zerop (length string))
           (hemlock::i-search-pattern string direction))))
      (move-mark point curr-point)
      (move-mark trailer curr-trailer))))

;;;      %LINE-ISEARCH-CHAR-EVAL evaluates the last character typed and takes
;;; necessary actions.
;;;
(defun %line-isearch-char-eval (key-event
				string
				point
				trailer
				direction
				failure)
  (cond ((let ((character (hemlock-ext:key-event-char key-event)))
           (and character (standard-char-p character)))
         (%line-isearch-printed-char key-event
				     string
				     point
				     trailer
				     direction
				     failure))
        ((or (logical-key-event-p key-event :forward-search)
             (logical-key-event-p key-event :backward-search))
         (%line-isearch-control-s-or-r key-event
				       string
				       point
				       trailer
				       direction
				       failure))
        ((logical-key-event-p key-event :cancel) :return-cancel)
        ((logical-key-event-p key-event :abort)
         (unless failure
           (clear-echo-area)
           (message "Search aborted.")
           (throw 'exit-i-search :control-g))
         :control-g)
        ((logical-key-event-p key-event :quote)
         (%line-isearch-printed-char (get-key-event *editor-input* t)
				     string
				     point
				     trailer
				     direction
				     failure))
        ((and (zerop (length string)) (logical-key-event-p key-event :exit))
	 (if (eq direction :forward)
             (hemlock::forward-search-command nil)
             (hemlock::reverse-search-command nil))
         (throw 'exit-i-search nil))
        (t
         (unless (logical-key-event-p key-event :exit)
           (unget-key-event key-event *editor-input*))
         (unless (zerop (length string))
           (setf hemlock::*last-search-string* string))
         (throw 'exit-i-search nil))))

(defun %line-isearch-again (direction)
  (line-isearch-find-pattern direction))

;;;      %LINE-ISEARCH-CONTROL-S-OR-R handles repetitions in the search.  Note
;;; that there cannot be failure in the last COND branch: since the direction
;;; has just been changed, there cannot be a failure before trying a new
;;; direction.
;;;
(defun advance-history-for-isearch (str editor fun)
  (let* ((linedit-history (editor-history editor))
	 (oldval (get-string editor))
	 (val (funcall fun oldval linedit-history)))
    (iter
      (while val)
      (until (and (search str val) (not (equal val oldval))))
      (setf val (funcall fun val linedit-history)))
    (and val
	 (setf (get-string editor) val))))

(defun history-backward-for-isearch (string)
  (advance-history-for-isearch string
		   (current-device)
		   #'linedit-history-previous))

(defun history-forward-for-isearch (string)
  (advance-history-for-isearch string
		   (current-device)
		   #'linedit-history-next))

(defun %line-isearch-control-s-or-r (key-event string point trailer
				     direction failure)
  (let ((forward-direction-p (eq direction :forward))
        (forward-character-p (logical-key-event-p key-event :forward-search)))
    (cond ((zerop (length string))
           (%line-isearch-empty-string point
				       trailer
				       direction
				       forward-direction-p
				       forward-character-p))
          ((eq forward-direction-p forward-character-p)
           (cond (failure
		  (update-prompt-for-line-isearch string
						  direction
						  "failed-")
		  (%line-isearch string point trailer direction failure))
                 (t
                  (%line-isearch-find-pattern string point (move-mark trailer point)
                                          direction))))
          (t
           (let ((new-direction (if forward-character-p :forward :backward)))
             (update-prompt-for-line-isearch string new-direction nil)
             (hemlock::i-search-pattern string new-direction)
             (%line-isearch-find-pattern string point (move-mark trailer point)
                                     new-direction))))))


;;;      %LINE-ISEARCH-EMPTY-STRING handles the empty string case when a ^S
;;; or ^R is typed.  If the direction and character typed do not agree,
;;; then merely switch directions.  If there was a previous string, search
;;; for it, else flash at the guy.
;;;
(defun %line-isearch-empty-string (point trailer direction forward-direction-p
                                     forward-character-p)
  (cond ((eq forward-direction-p (not forward-character-p))
         (let ((direction (if forward-character-p :forward :backward)))
           (update-prompt-for-line-isearch "" direction nil)
           (%line-isearch "" point trailer direction nil)))
        (hemlock::*last-search-string*
         (update-prompt-for-line-isearch hemlock::*last-search-string* direction nil)
         (hemlock::i-search-pattern hemlock::*last-search-string* direction)
         (%line-isearch-find-pattern hemlock::*last-search-string* point trailer direction))
        (t (beep))))


;;;      %LINE-ISEARCH-PRINTED-CHAR handles the case of standard character input.
;;; If the direction is backwards, we have to be careful not to MARK-AFTER
;;; the end of the buffer or to include the next character at the beginning
;;; of the search.
;;;
(defun %line-isearch-printed-char (key-event string point trailer direction failure)
  (let ((tchar (hemlock-ext:key-event-char key-event)))
    (unless tchar (editor-error "Not a text character -- ~S"
				(hemlock-ext:key-event-char key-event)))
    (when (interactive)
      (insert-character (buffer-point *echo-area-buffer*) tchar)
      (force-output *echo-area-stream*))
    (let ((new-string (concatenate 'simple-string string (string tchar))))
      (update-prompt-for-line-isearch new-string direction nil)
      (hemlock::i-search-pattern new-string direction)
      (cond (failure (%line-isearch new-string point trailer direction failure))
            ((and (eq direction :backward) (next-character trailer))
             (%line-isearch-find-pattern new-string point (mark-after trailer)
                                     direction))
            (t
             (%line-isearch-find-pattern new-string point trailer direction))))))


;;;      %LINE-ISEARCH-FIND-PATTERN takes a pattern for a string and direction
;;; and finds it, updating necessary pointers for the next call to %LINE-ISEARCH.
;;; If the search failed, tell the user and do not move any pointers.
;;;
(defun %line-isearch-find-pattern (string point trailer direction &optional backup)
  (let ((found-offset (line-isearch-find-pattern
		       direction
		       trailer
		       hemlock::*last-search-pattern*
		       string)))
    (cond (found-offset
            (cond ((eq direction :forward)
                   (character-offset (move-mark point trailer) found-offset))
                  (t
                   (move-mark point trailer)
                   (character-offset trailer found-offset)))
            (%line-isearch string point trailer direction nil))
          (t
           (when backup
             (move-mark point backup)
             (move-mark trailer backup))
           #+nil (update-prompt-for-line-isearch string direction "Failing")
           #+nil(if (interactive)
               (beep)
               (editor-error "I-Search failed."))
           (%line-isearch string point trailer direction "Failing")))))

(defun line-isearch-find-pattern
    (direction
     &optional (trailer (current-point))
     (pattern (get-search-pattern hemlock::*last-search-string* direction))
     (string hemlock::*last-search-string*))
  (iter
    (let ((found-offset (find-pattern trailer pattern)))
      (when found-offset
	(return found-offset)))
    (let (#+nil (backup (copy-mark point)))
      (ecase direction
	(:forward
	 (unless (history-forward-for-isearch string)
	   (return nil))
	 (goto-buffer-start))
	(:backward
	 (unless (history-backward-for-isearch string)
	   (return nil))
	 (goto-buffer-end)))
      (move-mark trailer (current-point)))))
