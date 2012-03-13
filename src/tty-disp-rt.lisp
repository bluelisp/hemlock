;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles.
;;;

(in-package :hemlock-internals)

(pushnew :tty hi::*available-backends*)


;;;; Get terminal attributes:

(defvar *terminal-baud-rate* nil)
(declaim (type (or (unsigned-byte 24) null) *terminal-baud-rate*))

;;; GET-TERMINAL-ATTRIBUTES  --  Interface
;;;
;;;    Get terminal attributes from Unix.  Return as values, the lines,
;;; columns and speed.  If any value is inaccessible, return NIL for that
;;; value.  We also sleazily cache the speed in *terminal-baud-rate*, since I
;;; don't want to figure out how to get my hands on the TTY-DEVICE at the place
;;; where I need it.  Currently, there really can only be one TTY anyway, since
;;; the buffer is in a global.
;;;
(defun get-terminal-attributes (&optional (fd 1))
  (let ((baud-rate #+(or CMU scl)
                   (alien:with-alien ((termios (alien:struct unix:termios)))
                     (declare (optimize (ext:inhibit-warnings 3)))
                     (when (unix:unix-tcgetattr fd termios)
                       (let ((baud (logand unix:tty-cbaud
                                           (alien:slot termios 'unix:c-cflag))))
                         (if (< baud unix::tty-cbaudex)
                             (aref #(0 50 75 110 134 150 200 300 600 1200
                                     1800 2400 4800 9600 19200 38400)
                                   baud)
                             (aref #(57600 115200 230400 460800 500000 576000
                                     921600 1000000 1152000 1500000 2000000
                                     2500000 3000000 3500000 4000000)
                                   (logxor baud unix::tty-cbaudex))))))
                   #-(or CMU scl) 4800))
    (setf *terminal-baud-rate* baud-rate)
    (cffi:with-foreign-object (ws 'osicat-posix::winsize)
      (osicat-posix:ioctl fd osicat-posix:tiocgwinsz ws)
      (cffi:with-foreign-slots ((osicat-posix::row osicat-posix::col)
                                ws osicat-posix::winsize)
        (values osicat-posix::row osicat-posix::col baud-rate)))))


;;;; Output routines and buffering.

(defconstant redisplay-output-buffer-length 256)

(defvar *redisplay-output-buffer*
  (make-string redisplay-output-buffer-length))
(declaim (simple-string *redisplay-output-buffer*))

(defvar *redisplay-output-buffer-index* 0)
(declaim (fixnum *redisplay-output-buffer-index*))

;;; WRITE-AND-MAYBE-WAIT  --  Internal
;;;
;;;    Write the first Count characters in the redisplay output buffer.  If
;;; *terminal-baud-rate* is set, then sleep for long enough to allow the
;;; written text to be displayed.  We multiply by 10 to get the baud-per-byte
;;; conversion, which assumes 7 character bits + 1 start bit + 2 stop bits, no
;;; parity.
;;;
(defun write-and-maybe-wait (count)
  (declare (fixnum count))
  (connection-write (subseq *redisplay-output-buffer* 0 count)
                    *tty-connection*)
  (dispatch-events-no-hang))


;;; TTY-WRITE-STRING blasts the string into the redisplay output buffer.
;;; If the string overflows the buffer, then segments of the string are
;;; blasted into the buffer, dumping the buffer, until the last piece of
;;; the string is stored in the buffer.  The buffer is always dumped if
;;; it is full, even if the last piece of the string just fills the buffer.
;;;
(defun tty-write-string (string &optional (start 0) length)
  (declare (fixnum start)
           (type (or fixnum null) length))
  (let ((buffer-space (- redisplay-output-buffer-length
                         *redisplay-output-buffer-index*))
        (length (or length (length string))))
    (declare (fixnum buffer-space))
    (cond ((<= length buffer-space)
           (let ((dst-index (+ *redisplay-output-buffer-index* length)))
             #+(or)
             (%primitive byte-blt
                         string             ;src
                         start              ;src-start
                         *redisplay-output-buffer* ;dst
                         *redisplay-output-buffer-index* ;dst-start
                         dst-index                       ;dst-end
                         )
             (replace *redisplay-output-buffer*
                      string
                      :start1 *redisplay-output-buffer-index*
                      :end1 dst-index
                      :start2 start)
             (cond ((= length buffer-space)
                    (write-and-maybe-wait redisplay-output-buffer-length)
                    (setf *redisplay-output-buffer-index* 0))
                   (t
                    (setf *redisplay-output-buffer-index* dst-index)))))
          (t
           (let ((remaining (- length buffer-space)))
             (declare (fixnum remaining))
             (loop
              #+(or)
                (%primitive byte-blt
                            string                        ;src
                            start                         ;src-start
                            *redisplay-output-buffer*     ;dst
                            *redisplay-output-buffer-index* ;dst-start
                            redisplay-output-buffer-length  ;dst-end
                            )
                (replace *redisplay-output-buffer*
                         string
                         :start1 *redisplay-output-buffer-index*
                         :end1 redisplay-output-buffer-length
                         :start2 start)
              (write-and-maybe-wait redisplay-output-buffer-length)
              (when (< remaining redisplay-output-buffer-length)
                #+(or)
                (%primitive byte-blt
                            string                    ;src
                            (+ start buffer-space)    ;src-start
                            *redisplay-output-buffer* ;dst
                            0                         ;dst-start
                            remaining                 ;dst-end
                            )
                (replace *redisplay-output-buffer*
                         string
                         :start1 0
                         :end1 remaining
                         :start2 (+ start buffer-space))
                (setf *redisplay-output-buffer-index* remaining)
                (return t))
              (incf start buffer-space)
              (setf *redisplay-output-buffer-index* 0)
              (setf buffer-space redisplay-output-buffer-length)
              (decf remaining redisplay-output-buffer-length)))))))


;;; TTY-WRITE-CHAR stores a character in the redisplay output buffer,
;;; dumping the buffer if it becomes full.
;;;
(defun tty-write-char (char)
  (setf (schar *redisplay-output-buffer* *redisplay-output-buffer-index*)
        char)
  (incf *redisplay-output-buffer-index*)
  (when (= *redisplay-output-buffer-index* redisplay-output-buffer-length)
    (write-and-maybe-wait redisplay-output-buffer-length)
    (setf *redisplay-output-buffer-index* 0)))

;;; Write a terminfo string returned by 'tputs which is a list of
;;; strings and delays.
(defun tty-write-cmd (cmd)
  (declare (type (or string list) cmd))
  (etypecase cmd
    (string
     (tty-write-string cmd))
    (list
     (dolist (string-or-delay cmd)
       (cond ((stringp string-or-delay)
              (tty-write-string string-or-delay))
             ((numberp string-or-delay)
              ;; Not yet supported, but could pass the delay to the
              ;; event handler.
              ))))))

;;; Return the total number of characters in the command list returned
;;; by 'tputs.
(defun tty-cmd-length (cmd)
  (declare (type (or string list) cmd))
  (etypecase cmd
    (string
     (length cmd))
    (list
     (let ((len 0))
       (dolist (string-or-delay cmd)
         (when (stringp string-or-delay)
           (incf len (length string-or-delay))))
       len))))


;;; TTY-FORCE-OUTPUT dumps the redisplay output buffer.  This is called
;;; out of terminal device structures in multiple places -- the device
;;; exit method, random typeout methods, out of tty-hunk-stream methods,
;;; after calls to REDISPLAY or REDISPLAY-ALL.
;;;
(defmethod device-force-output ((device tty-device))
  (unless (zerop *redisplay-output-buffer-index*)
    (write-and-maybe-wait *redisplay-output-buffer-index*)
    (setf *redisplay-output-buffer-index* 0)))


;;; TTY-FINISH-OUTPUT simply dumps output.
;;;
(defmethod device-finish-output ((device tty-device) window)
  (declare (ignore window))
  (device-force-output device))



;;;; Terminal init and exit methods.

(defmethod device-init ((device tty-device))
  (setup-input)
  (tty-write-cmd (tty-device-init-string device))
  (redisplay-all))

(defmethod device-exit ((device tty-device))
  (cursor-motion device 0 (1- (tty-device-lines device)))
  ;; Can't call the clear-to-eol method since we don't have a hunk to
  ;; call it on, and you can't count on the bottom hunk being the echo area.
  ;;
  (if (tty-device-clear-to-eol-string device)
      (tty-write-cmd (tty-device-clear-to-eol-string device))
      (dotimes (i (tty-device-columns device)
                  (cursor-motion device 0 (1- (tty-device-lines device))))
        (tty-write-char #\space)))
  (tty-write-cmd (tty-device-cm-end-string device))
  (device-force-output device)
  (reset-input))


;;;; Screen image line hacks.

(defun replace-si-line (dst-string src-string src-start dst-start dst-end)
;;;   `(%primitive byte-blt ,src-string ,src-start ,dst-string ,dst-start ,dst-end)
  (replace dst-string
           src-string
           :start1 dst-start
           :end1 dst-end
           :start2 src-start))

(defvar *old-c-iflag*)
(defvar *old-c-oflag*)
(defvar *old-c-cflag*)
(defvar *old-c-lflag*)
(defvar *old-c-cc*)

(defun setup-input ()
  (let ((fd 1 #+nil *editor-file-descriptor*))
    (when (plusp (osicat-posix::isatty fd))
      (cffi:with-foreign-object (tios 'osicat-posix::termios)
        (osicat-posix::tcgetattr fd tios)
        (cffi:with-foreign-slots ((osicat-posix::iflag
                                   osicat-posix::oflag
                                   osicat-posix::cflag
                                   osicat-posix::lflag
                                   osicat-posix::cc)
                                  tios osicat-posix::termios)
          (setf *old-c-iflag* osicat-posix::iflag)
          (setf *old-c-oflag* osicat-posix::oflag)
          (setf *old-c-cflag* osicat-posix::cflag)
          (setf *old-c-lflag* osicat-posix::lflag)
          (macrolet ((ccref (slot)
                       `(cffi:mem-ref osicat-posix::cc :uint8 ,slot)))
            (setf *old-c-cc*
                  (vector (ccref osicat-posix::cflag-vsusp)
                          (ccref osicat-posix::cflag-veof)
                          (ccref osicat-posix::cflag-verase)
                          (ccref osicat-posix::cflag-vintr)
                          (ccref osicat-posix::cflag-vquit)
                          (ccref osicat-posix::cflag-vstart)
                          (ccref osicat-posix::cflag-vstop)
                          (ccref osicat-posix::cflag-vsusp)
                          (when (boundp 'osicat-posix::cflag-vdsusp)
                            (ccref osicat-posix::cflag-vdsusp))
                          (ccref osicat-posix::cflag-vmin)
                          (ccref osicat-posix::cflag-vtime)))
            (setf osicat-posix::lflag
                  (logandc2 osicat-posix::lflag
                            (logior osicat-posix::tty-echo
                                    osicat-posix::tty-icanon)))
            (setf osicat-posix::iflag
                  (logandc2 osicat-posix::iflag
                            (logior osicat-posix::tty-icrnl
                                    osicat-posix::tty-ixon)))
            (setf osicat-posix::oflag
                  (logandc2 osicat-posix::oflag
                            #-bsd osicat-posix::tty-ocrnl
                            #+bsd osicat-posix::tty-onlcr))
            (setf (ccref osicat-posix::cflag-vsusp) #xff)
            (setf (ccref osicat-posix::cflag-veof) #xff)
            (setf (ccref osicat-posix::cflag-verase) 127) ;DEL
            (setf (ccref osicat-posix::cflag-vintr)
                  (if *editor-windowed-input* #xff 28))
            (setf (ccref osicat-posix::cflag-vquit) #xff)
            (setf (ccref osicat-posix::cflag-vstart) #xff)
            (setf (ccref osicat-posix::cflag-vstop) #xff)
            (setf (ccref osicat-posix::cflag-vsusp) #xff)
            (when (boundp 'osicat-posix::cflag-vdsusp)
              ;; Default VDSUSP is C-y; it causes SIGTSTP on BSD-heritage
              ;; systems -- but may be undefined elsewhere.
              (setf (ccref osicat-posix::cflag-vdsusp) #xff))
            (setf (ccref osicat-posix::cflag-vmin) 1)
            (setf (ccref osicat-posix::cflag-vtime) 0))
          (osicat-posix::tcsetattr fd osicat-posix::tcsaflush tios))))))

;;; #+nil ;; #-(or hpux irix bsd glibc2)
;;;       (alien:with-alien ((sg (alien:struct unix:sgttyb)))
;;;     (multiple-value-bind
;;;         (val err)
;;;         (unix:unix-ioctl fd unix:TIOCGETP (alien:alien-sap sg))
;;;       (unless val
;;;         (error "Could not get tty information, unix error ~S."
;;;                (unix:get-unix-error-msg err))))
;;;     (let ((flags (alien:slot sg 'unix:sg-flags)))
;;;       (setq old-flags flags)
;;;       (setf (alien:slot sg 'unix:sg-flags)
;;;             (logand #-(or hpux irix bsd glibc2) (logior flags unix:tty-cbreak)
;;;                     (lognot unix:tty-echo)
;;;                     (lognot unix:tty-crmod)))
;;;       (multiple-value-bind
;;;           (val err)
;;;           (unix:unix-ioctl fd unix:TIOCSETP (alien:alien-sap sg))
;;;         (if (null val)
;;;             (error "Could not set tty information, unix error ~S."
;;;                    (unix:get-unix-error-msg err))))))
;;;       #+nil ;; #-(or hpux irix bsd glibc2)
;;;       (alien:with-alien ((tc (alien:struct unix:tchars)))
;;;     (multiple-value-bind
;;;         (val err)
;;;         (unix:unix-ioctl fd unix:TIOCGETC (alien:alien-sap tc))
;;;       (unless val
;;;         (error "Could not get tty tchars information, unix error ~S."
;;;                (unix:get-unix-error-msg err))))
;;;     (setq old-tchars
;;;           (vector (alien:slot tc 'unix:t-intrc)
;;;                   (alien:slot tc 'unix:t-quitc)
;;;                   (alien:slot tc 'unix:t-startc)
;;;                   (alien:slot tc 'unix:t-stopc)
;;;                   (alien:slot tc 'unix:t-eofc)
;;;                   (alien:slot tc 'unix:t-brkc)))
;;;     (setf (alien:slot tc 'unix:t-intrc)
;;;           (if *editor-windowed-input* -1 28))
;;;     (setf (alien:slot tc 'unix:t-quitc) -1)
;;;     (setf (alien:slot tc 'unix:t-startc) -1)
;;;     (setf (alien:slot tc 'unix:t-stopc) -1)
;;;     (setf (alien:slot tc 'unix:t-eofc) -1)
;;;     (setf (alien:slot tc 'unix:t-brkc) -1)
;;;     (multiple-value-bind
;;;         (val err)
;;;         (unix:unix-ioctl fd unix:TIOCSETC (alien:alien-sap tc))
;;;       (unless val
;;;         (error "Failed to set tchars, unix error ~S."
;;;                (unix:get-unix-error-msg err)))))

;;;       ;; Needed even under HpUx to suppress dsuspc.
;;;       #+nil
;;;       ;; #-(or glibc2 irix)
;;;       (alien:with-alien ((tc (alien:struct unix:ltchars)))
;;;     (multiple-value-bind
;;;         (val err)
;;;         (unix:unix-ioctl fd unix:TIOCGLTC (alien:alien-sap tc))
;;;       (unless val
;;;         (error "Could not get tty ltchars information, unix error ~S."
;;;                (unix:get-unix-error-msg err))))
;;;     (setq old-ltchars
;;;           (vector (alien:slot tc 'unix:t-suspc)
;;;                   (alien:slot tc 'unix:t-dsuspc)
;;;                   (alien:slot tc 'unix:t-rprntc)
;;;                   (alien:slot tc 'unix:t-flushc)
;;;                   (alien:slot tc 'unix:t-werasc)
;;;                   (alien:slot tc 'unix:t-lnextc)))
;;;     (setf (alien:slot tc 'unix:t-suspc) -1)
;;;     (setf (alien:slot tc 'unix:t-dsuspc) -1)
;;;     (setf (alien:slot tc 'unix:t-rprntc) -1)
;;;     (setf (alien:slot tc 'unix:t-flushc) -1)
;;;     (setf (alien:slot tc 'unix:t-werasc) -1)
;;;     (setf (alien:slot tc 'unix:t-lnextc) -1)
;;;     (multiple-value-bind
;;;         (val err)
;;;         (unix:unix-ioctl fd unix:TIOCSLTC (alien:alien-sap tc))
;;;       (unless val
;;;         (error "Failed to set ltchars, unix error ~S."
;;;                (unix:get-unix-error-msg err)))))

(defun reset-input ()
  (let ((fd 1 #+nil *editor-file-descriptor*))
    (when (plusp (osicat-posix::isatty fd))
      (cffi:with-foreign-object (tios 'osicat-posix::termios)
        (osicat-posix::tcgetattr fd tios)
        (cffi:with-foreign-slots ((osicat-posix::iflag
                                   osicat-posix::oflag
                                   osicat-posix::cflag
                                   osicat-posix::lflag
                                   osicat-posix::cc)
                                  tios osicat-posix::termios)
          (setf osicat-posix::iflag *old-c-iflag*)
          (setf osicat-posix::oflag *old-c-oflag*)
          (setf osicat-posix::cflag *old-c-cflag*)
          (setf osicat-posix::lflag *old-c-lflag*)
          (macrolet ((ccref (slot)
                       `(cffi:mem-ref osicat-posix::cc :uint8 ,slot)))
            (assert (= (length *old-c-cc*) 11))
            (setf (ccref osicat-posix::cflag-vsusp) (elt *old-c-cc* 0)
                  (ccref osicat-posix::cflag-veof) (elt *old-c-cc* 1)
                  (ccref osicat-posix::cflag-verase) (elt *old-c-cc* 2)
                  (ccref osicat-posix::cflag-vintr) (elt *old-c-cc* 3)
                  (ccref osicat-posix::cflag-vquit) (elt *old-c-cc* 4)
                  (ccref osicat-posix::cflag-vstart) (elt *old-c-cc* 5)
                  (ccref osicat-posix::cflag-vstop) (elt *old-c-cc* 6)
                  (ccref osicat-posix::cflag-vsusp) (elt *old-c-cc* 7))
            (when (boundp 'osicat-posix::cflag-vdsusp)
              (setf (ccref osicat-posix::cflag-vdsusp) (elt *old-c-cc* 8)))
            (setf (ccref osicat-posix::cflag-vmin) (elt *old-c-cc* 9)
                  (ccref osicat-posix::cflag-vtime) (elt *old-c-cc* 10)))
          (osicat-posix::tcsetattr fd osicat-posix::tcsaflush tios))))))

#+(or)
(defun pause-hemlock ()
  "Pause hemlock and pop out to the Unix Shell."
  (without-hemlock
   (unix:unix-kill (unix:unix-getpid) :sigstop))
  t)
