;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; Terminal hunks.
;;;
(defstruct (tty-hunk #|(:print-function %print-device-hunk)|#
                     (:include device-hunk))
  text-position         ; Bottom Y position of text in hunk.
  text-height)          ; Number of lines of text.


