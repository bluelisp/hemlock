;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+CMU (ext:file-comment
  "$Header: /home/david/phemlock/cvsroot/phemlock/src/bitmap/keysym-defs.lisp,v 1.1 2004-07-09 13:38:04 gbaumann Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file defines all the definitions of keysyms (see key-event.lisp).
;;; These keysyms match those for X11.
;;;
;;; Written by Bill Chiles
;;; Modified by Blaine Burks.
;;;

(in-package :hemlock-internals)


;;; The IBM RT keyboard has X11 keysyms defined for the following modifier
;;; keys, but we leave them mapped to nil indicating that they are non-events
;;; to be ignored:
;;;    ctrl             65507
;;;    meta (left)      65513
;;;    meta (right)     65514
;;;    shift (left)     65505
;;;    shift (right)    65506
;;;    lock             65509
;;;


;;; Function keys for the RT.
;;;
(hemlock-ext:define-keysym 65470 "F1")
(hemlock-ext:define-keysym 65471 "F2")
(hemlock-ext:define-keysym 65472 "F3")
(hemlock-ext:define-keysym 65473 "F4")
(hemlock-ext:define-keysym 65474 "F5")
(hemlock-ext:define-keysym 65475 "F6")
(hemlock-ext:define-keysym 65476 "F7")
(hemlock-ext:define-keysym 65477 "F8")
(hemlock-ext:define-keysym 65478 "F9")
(hemlock-ext:define-keysym 65479 "F10")
(hemlock-ext:define-keysym 65480 "F11" "L1")
(hemlock-ext:define-keysym 65481 "F12" "L2")

;;; Function keys for the Sun (and other keyboards) -- L1-L10 and R1-R15.
;;;
(hemlock-ext:define-keysym 65482 "F13" "L3")
(hemlock-ext:define-keysym 65483 "F14" "L4")
(hemlock-ext:define-keysym 65484 "F15" "L5")
(hemlock-ext:define-keysym 65485 "F16" "L6")
(hemlock-ext:define-keysym 65486 "F17" "L7")
(hemlock-ext:define-keysym 65487 "F18" "L8")
(hemlock-ext:define-keysym 65488 "F19" "L9")
(hemlock-ext:define-keysym 65489 "F20" "L10")
(hemlock-ext:define-keysym 65490 "F21" "R1")
(hemlock-ext:define-keysym 65491 "F22" "R2")
(hemlock-ext:define-keysym 65492 "F23" "R3")
(hemlock-ext:define-keysym 65493 "F24" "R4")
(hemlock-ext:define-keysym 65494 "F25" "R5")
(hemlock-ext:define-keysym 65495 "F26" "R6")
(hemlock-ext:define-keysym 65496 "F27" "R7")
(hemlock-ext:define-keysym 65497 "F28" "R8")
(hemlock-ext:define-keysym 65498 "F29" "R9")
(hemlock-ext:define-keysym 65499 "F30" "R10")
(hemlock-ext:define-keysym 65500 "F31" "R11")
(hemlock-ext:define-keysym 65501 "F32" "R12")
(hemlock-ext:define-keysym 65502 "F33" "R13")
(hemlock-ext:define-keysym 65503 "F34" "R14")
(hemlock-ext:define-keysym 65504 "F35" "R15")

;;; Upper right key bank.
;;;
(hemlock-ext:define-keysym 65377 "Printscreen")
;; Couldn't type scroll lock.
(hemlock-ext:define-keysym 65299 "Pause")

;;; Middle right key bank.
;;;
(hemlock-ext:define-keysym 65379 "Insert")
(hemlock-ext:define-keysym 65535 "Delete" "Rubout" (string (code-char 127)))
(hemlock-ext:define-keysym 65360 "Home")
(hemlock-ext:define-keysym 65365 "Pageup")
(hemlock-ext:define-keysym 65367 "End")
(hemlock-ext:define-keysym 65366 "Pagedown")

;;; Arrows.
;;;
(hemlock-ext:define-keysym 65361 "Leftarrow")
(hemlock-ext:define-keysym 65362 "Uparrow")
(hemlock-ext:define-keysym 65364 "Downarrow")
(hemlock-ext:define-keysym 65363 "Rightarrow")

;;; Number pad.
;;;
(hemlock-ext:define-keysym 65407 "Numlock")
(hemlock-ext:define-keysym 65421 "Numpad\-Return" "Numpad\-Enter")      ;num-pad-enter
(hemlock-ext:define-keysym 65455 "Numpad/")                             ;num-pad-/
(hemlock-ext:define-keysym 65450 "Numpad*")                             ;num-pad-*
(hemlock-ext:define-keysym 65453 "Numpad-")                             ;num-pad--
(hemlock-ext:define-keysym 65451 "Numpad+")                             ;num-pad-+
(hemlock-ext:define-keysym 65456 "Numpad0")                             ;num-pad-0
(hemlock-ext:define-keysym 65457 "Numpad1")                             ;num-pad-1
(hemlock-ext:define-keysym 65458 "Numpad2")                             ;num-pad-2
(hemlock-ext:define-keysym 65459 "Numpad3")                             ;num-pad-3
(hemlock-ext:define-keysym 65460 "Numpad4")                             ;num-pad-4
(hemlock-ext:define-keysym 65461 "Numpad5")                             ;num-pad-5
(hemlock-ext:define-keysym 65462 "Numpad6")                             ;num-pad-6
(hemlock-ext:define-keysym 65463 "Numpad7")                             ;num-pad-7
(hemlock-ext:define-keysym 65464 "Numpad8")                             ;num-pad-8
(hemlock-ext:define-keysym 65465 "Numpad9")                             ;num-pad-9
(hemlock-ext:define-keysym 65454 "Numpad.")                             ;num-pad-.

;;; "Named" keys.
;;;
(hemlock-ext:define-keysym 65289 "Tab")
(hemlock-ext:define-keysym 65307 "Escape" "Altmode" "Alt")              ;escape
(hemlock-ext:define-keysym 65288 "Backspace")                           ;backspace
(hemlock-ext:define-keysym 65293 "Return" "Enter")                      ;enter
(hemlock-ext:define-keysym 65512 "Linefeed" "Action" "Newline")         ;action
(hemlock-ext:define-keysym 32 "Space" " ")

;;; Letters.
;;;
(hemlock-ext:define-keysym 97 "a") (hemlock-ext:define-keysym 65 "A")
(hemlock-ext:define-keysym 98 "b") (hemlock-ext:define-keysym 66 "B")
(hemlock-ext:define-keysym 99 "c") (hemlock-ext:define-keysym 67 "C")
(hemlock-ext:define-keysym 100 "d") (hemlock-ext:define-keysym 68 "D")
(hemlock-ext:define-keysym 101 "e") (hemlock-ext:define-keysym 69 "E")
(hemlock-ext:define-keysym 102 "f") (hemlock-ext:define-keysym 70 "F")
(hemlock-ext:define-keysym 103 "g") (hemlock-ext:define-keysym 71 "G")
(hemlock-ext:define-keysym 104 "h") (hemlock-ext:define-keysym 72 "H")
(hemlock-ext:define-keysym 105 "i") (hemlock-ext:define-keysym 73 "I")
(hemlock-ext:define-keysym 106 "j") (hemlock-ext:define-keysym 74 "J")
(hemlock-ext:define-keysym 107 "k") (hemlock-ext:define-keysym 75 "K")
(hemlock-ext:define-keysym 108 "l") (hemlock-ext:define-keysym 76 "L")
(hemlock-ext:define-keysym 109 "m") (hemlock-ext:define-keysym 77 "M")
(hemlock-ext:define-keysym 110 "n") (hemlock-ext:define-keysym 78 "N")
(hemlock-ext:define-keysym 111 "o") (hemlock-ext:define-keysym 79 "O")
(hemlock-ext:define-keysym 112 "p") (hemlock-ext:define-keysym 80 "P")
(hemlock-ext:define-keysym 113 "q") (hemlock-ext:define-keysym 81 "Q")
(hemlock-ext:define-keysym 114 "r") (hemlock-ext:define-keysym 82 "R")
(hemlock-ext:define-keysym 115 "s") (hemlock-ext:define-keysym 83 "S")
(hemlock-ext:define-keysym 116 "t") (hemlock-ext:define-keysym 84 "T")
(hemlock-ext:define-keysym 117 "u") (hemlock-ext:define-keysym 85 "U")
(hemlock-ext:define-keysym 118 "v") (hemlock-ext:define-keysym 86 "V")
(hemlock-ext:define-keysym 119 "w") (hemlock-ext:define-keysym 87 "W")
(hemlock-ext:define-keysym 120 "x") (hemlock-ext:define-keysym 88 "X")
(hemlock-ext:define-keysym 121 "y") (hemlock-ext:define-keysym 89 "Y")
(hemlock-ext:define-keysym 122 "z") (hemlock-ext:define-keysym 90 "Z")

;;; Standard number keys.
;;;
(hemlock-ext:define-keysym 49 "1") (hemlock-ext:define-keysym 33 "!")
(hemlock-ext:define-keysym 50 "2") (hemlock-ext:define-keysym 64 "@")
(hemlock-ext:define-keysym 51 "3") (hemlock-ext:define-keysym 35 "#")
(hemlock-ext:define-keysym 52 "4") (hemlock-ext:define-keysym 36 "$")
(hemlock-ext:define-keysym 53 "5") (hemlock-ext:define-keysym 37 "%")
(hemlock-ext:define-keysym 54 "6") (hemlock-ext:define-keysym 94 "^")
(hemlock-ext:define-keysym 55 "7") (hemlock-ext:define-keysym 38 "&")
(hemlock-ext:define-keysym 56 "8") (hemlock-ext:define-keysym 42 "*")
(hemlock-ext:define-keysym 57 "9") (hemlock-ext:define-keysym 40 "(")
(hemlock-ext:define-keysym 48 "0") (hemlock-ext:define-keysym 41 ")")

;;; "Standard" symbol keys.
;;;
(hemlock-ext:define-keysym 96 "`") (hemlock-ext:define-keysym 126 "~")
(hemlock-ext:define-keysym 45 "-") (hemlock-ext:define-keysym 95 "_")
(hemlock-ext:define-keysym 61 "=") (hemlock-ext:define-keysym 43 "+")
(hemlock-ext:define-keysym 91 "[") (hemlock-ext:define-keysym 123 "{")
(hemlock-ext:define-keysym 93 "]") (hemlock-ext:define-keysym 125 "}")
(hemlock-ext:define-keysym 92 "\\") (hemlock-ext:define-keysym 124 "|")
(hemlock-ext:define-keysym 59 ";") (hemlock-ext:define-keysym 58 ":")
(hemlock-ext:define-keysym 39 "'") (hemlock-ext:define-keysym 34 "\"")
(hemlock-ext:define-keysym 44 ",") (hemlock-ext:define-keysym 60 "<")
(hemlock-ext:define-keysym 46 ".") (hemlock-ext:define-keysym 62 ">")
(hemlock-ext:define-keysym 47 "/") (hemlock-ext:define-keysym 63 "?")

;;; Standard Mouse keysyms.
;;;
(hemlock-ext::define-mouse-keysym 1 25601 "Leftdown" "Super" :button-press)
(hemlock-ext::define-mouse-keysym 1 25602 "Leftup" "Super" :button-release)

(hemlock-ext::define-mouse-keysym 2 25603 "Middledown" "Super" :button-press)
(hemlock-ext::define-mouse-keysym 2 25604 "Middleup" "Super" :button-release)

(hemlock-ext::define-mouse-keysym 3 25605 "Rightdown" "Super" :button-press)
(hemlock-ext::define-mouse-keysym 3 25606 "Rightup" "Super" :button-release)

;;; Sun keyboard.
;;;
(hemlock-ext:define-keysym 65387 "break")                       ;alternate (Sun).
;(hemlock-ext:define-keysym 65290 "linefeed")



;;;; SETFs of KEY-EVANT-CHAR and CHAR-KEY-EVENT.

;;; Converting ASCII control characters to Common Lisp control characters:
;;; ASCII control character codes are separated from the codes of the
;;; "non-controlified" characters by the code of atsign.  The ASCII control
;;; character codes range from ^@ (0) through ^_ (one less than the code of
;;; space).  We iterate over this range adding the ASCII code of atsign to
;;; get the "non-controlified" character code.  With each of these, we turn
;;; the code into a Common Lisp character and set its :control bit.  Certain
;;; ASCII control characters have to be translated to special Common Lisp
;;; characters outside of the loop.
;;;    With the advent of Hemlock running under X, and all the key bindings
;;; changing, we also downcase each Common Lisp character (where normally
;;; control characters come in upcased) in an effort to obtain normal command
;;; bindings.  Commands bound to uppercase modified characters will not be
;;; accessible to terminal interaction.
;;;
(let ((@-code (char-code #\@)))
  (dotimes (i (char-code #\space))
    (setf (hemlock-ext:char-key-event (code-char i))
          (hemlock-ext::make-key-event (string (char-downcase (code-char (+ i @-code))))
                               (hemlock-ext:key-event-modifier-mask "control")))))
(setf (hemlock-ext:char-key-event (code-char 9)) (hemlock-ext::make-key-event #k"Tab"))
(setf (hemlock-ext:char-key-event (code-char 10)) (hemlock-ext::make-key-event #k"Linefeed"))
(setf (hemlock-ext:char-key-event (code-char 13)) (hemlock-ext::make-key-event #k"Return"))
(setf (hemlock-ext:char-key-event (code-char 27)) (hemlock-ext::make-key-event #k"Alt"))
(setf (hemlock-ext:char-key-event (code-char 8)) (hemlock-ext::make-key-event #k"Backspace"))
;;;
;;; Other ASCII codes are exactly the same as the Common Lisp codes.
;;;
(do ((i (char-code #\space) (1+ i)))
    ((= i 128))
  (setf (hemlock-ext:char-key-event (code-char i))
        (hemlock-ext::make-key-event (string (code-char i)))))

;;; This makes KEY-EVENT-CHAR the inverse of CHAR-KEY-EVENT from the start.
;;; It need not be this way, but it is.
;;;
(dotimes (i 128)
  (let ((character (code-char i)))
    (setf (hemlock-ext::key-event-char (hemlock-ext:char-key-event character)) character)))

;;; Since we treated these characters specially above when setting
;;; HEMLOCK-EXT:CHAR-KEY-EVENT above, we must set these HEMLOCK-EXT:KEY-EVENT-CHAR's specially
;;; to make quoting characters into Hemlock buffers more obvious for users.
;;;
(setf (hemlock-ext:key-event-char #k"C-h") #\backspace)
(setf (hemlock-ext:key-event-char #k"C-i") #\tab)
(setf (hemlock-ext:key-event-char #k"C-j") #\linefeed)
(setf (hemlock-ext:key-event-char #k"C-m") #\return)
