;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Hemlock File manipulation functions.
;;; Written by Skef Wholey, Horribly Hacked by Rob MacLachlan.
;;; Unhacked by Gilbert Baumann.
;;;

(in-package :hemlock-internals)


;;;; Utility functions.

;; FIND-CHAR-FROM-SAP was here, deleted --GB


;;; Read-File:

(defun read-file (pathname mark)
  "Inserts the contents of the file named by Pathname at the Mark."
  (with-mark ((mark mark :left-inserting))
    (let* ((first-line (mark-line mark))
           (buffer (line-%buffer first-line)))
      (modifying-buffer buffer)
      (with-open-file (input pathname :direction :input :element-type 'character)
        (do ((line (read-line input nil :eof) (read-line input nil :eof)))
            ((eql line :eof))
          (insert-string mark line)
          (insert-character mark #\newline))))))

;;; Hackish stuff for disgusting speed:

(defun read-buffered-line (line)
  ;; This function is not used at all by now --GB
  (declare (ignore line))
  (error "Oops, I am not supposed to be used.")
  #+NIL
  (let* ((len (line-buffered-p line))
         (chars (make-string len)))
    (%primitive byte-blt (line-%chars line) 0 chars 0 len)
    (setf (line-buffered-p line) nil)
    (setf (line-chars line) chars)))


;;; Write-File:

(defun write-file (region pathname &key append
                          (keep-backup (value hemlock::keep-backup-files))
                          access)
  "Writes the characters in region to the file named by pathname.  This writes
   region using a stream opened with :if-exists :rename-and-delete, unless
   either append or keep-backup is supplied.  If append is supplied, this
   writes the file opened with :if-exists :append.  If keep-backup is supplied,
   this writes the file opened with :if-exists :rename.  This signals an error
   if both append and keep-backup are supplied.  Access is an implementation
   dependent value that is suitable for setting pathname's access or protection
   bits."
  (let ((if-exists-action (cond ((and keep-backup append)
                                 (error "Cannot supply non-nil values for ~
                                         both keep-backup and append."))
                                (keep-backup :rename)
                                (append :append)
                                (t :rename-and-delete))))
    (with-open-file (file pathname :direction :output
                          :element-type 'character
                          :external-format :utf-8 ;fixme?
                          :if-exists if-exists-action)
      (close-line)
      (fast-write-file region file))
    (hemlock-ext:set-file-permissions pathname access)))

(defun fast-write-file (region file)
  (let* ((start (region-start region))
         (start-line (mark-line start))
         (start-charpos (mark-charpos start))
         (end (region-end region))
         (end-line (mark-line end))
         (end-charpos (mark-charpos end)))
    (if (eq start-line end-line)
        ;; just one line (fragment)
        (write-string (line-chars start-line) file
                      :start start-charpos :end end-charpos)
        ;; multiple lines
        (let* ((first-length (- (line-length start-line) start-charpos))
               (length (+ first-length end-charpos 1)))
          ;; count number of octets to be written
          (do ((line (line-next start-line) (line-next line)))
              ((eq line end-line))
            (incf length (1+ (line-length line))))
          ;;
          (macrolet ((chars (line)
                       `(if (line-buffered-p ,line)
                         (line-%chars ,line)
                         (line-chars ,line))))
            (write-sequence (chars start-line) file :start start-charpos :end (+ start-charpos first-length))
            (write-char #\newline file)
            (let ((offset (1+ first-length)))
              (do ((line (line-next start-line)
                         (line-next line)))
                  ((eq line end-line))
                (let ((end (+ offset (line-length line))))
                  (write-sequence (chars line) file :start 0 :end (- end offset))
                  (write-char #\newline file)
                  (setf offset (1+ end))))
              (unless (zerop end-charpos)
                (write-sequence (chars end-line) file :start 0 :end end-charpos))))))))
