;;;; -*- Mode: Lisp; indent-with-tabs: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles
;;;
;;; Terminal Capability

(in-package :hemlock-internals)

;;; This stuff used to parse a Termcap file.  Now it's just a
;;; compatibility layer over the Terminfo code.  At some point this
;;; should be deleted entirely.

(defvar *termcaps* ())

(defun termcap (name)
  (funcall (cdr (assoc name *termcaps* :test #'eq))))

(defmacro deftermcap (name type cl-name terminfo-name)
  (declare (ignore name type))
  `(progn (push (cons ',cl-name (lambda () ,(intern (symbol-name terminfo-name) :hemlock.terminfo))) *termcaps*)))

(deftermcap "is" :string :init-string init-2string)
(deftermcap "if" :string :init-file init-file)
(deftermcap "ti" :string :init-cursor-motion enter-ca-mode)
(deftermcap "te" :string :end-cursor-motion exit-ca-mode)
(deftermcap "al" :string :open-line insert-line)
(deftermcap "am" :boolean :auto-margins-p auto-right-margin)
(deftermcap "ce" :string :clear-to-eol clr-eol)
(deftermcap "cl" :string :clear-display clear-screen)
#+nil (deftermcap "cm" :string :cursor-motion cursor-address)
(deftermcap "co" :number :columns columns)
(deftermcap "dc" :string :delete-char delete-character)
(deftermcap "dm" :string :init-delete-mode enter-delete-mode)
(deftermcap "ed" :string :end-delete-mode clr-eos)
(deftermcap "dl" :string :delete-line delete-line)
(deftermcap "im" :string :init-insert-mode enter-insert-mode)
(deftermcap "ic" :string :init-insert-char insert-character)
(deftermcap "ip" :string :end-insert-char insert-padding)
(deftermcap "ei" :string :end-insert-mode exit-insert-mode)
(deftermcap "li" :number :lines lines)
(deftermcap "so" :string :init-standout-mode enter-standout-mode)
(deftermcap "se" :string :end-standout-mode exit-standout-mode)
#+nil(deftermcap "tc" :string :similar-terminal)
(deftermcap "os" :boolean :overstrikes over-strike)
(deftermcap "ul" :boolean :underlines transparent-underline)

;;; font related stuff, added by William
(deftermcap "ae" :string :end-alternate-char-set exit-alt-charset-mode)
(deftermcap "as" :string :start-alternate-char-set enter-alt-charset-mode)
(deftermcap "mb" :string :start-blinking-attribute enter-blink-mode)
(deftermcap "md" :string :start-bold-attribute enter-bold-mode)
(deftermcap "me" :string :end-all-attributes exit-attribute-mode)
(deftermcap "mh" :string :start-half-bright-attribute enter-dim-mode)
(deftermcap "mk" :string :start-blank-attribute enter-secure-mode)
(deftermcap "mp" :string :start-protected-attribute enter-protected-mode)
(deftermcap "mr" :string :start-reverse-video-attribute enter-reverse-mode)
(deftermcap "ue" :string :end-underscore-mode exit-underline-mode)
(deftermcap "us" :string :start-underscore-mode enter-underline-mode)
