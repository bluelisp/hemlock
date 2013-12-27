;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************

(in-package :hemlock-internals)

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
          :accessor tty-device-speed)
   (terminfo :initarg :terminfo :initform nil
             :accessor tty-device-terminfo)))

(defun %make-tty-device (&rest initargs)
  (apply #'make-instance 'tty-device initargs))
