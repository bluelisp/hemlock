;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; More Hemlock Text-Manipulation functions.
;;; Written by Skef Wholey and Rob MacLachlan.
;;; Modified by Bill Chiles.
;;;
;;; The code in this file implements the delete and copy functions in the
;;; "Doing Stuff and Going Places" chapter of the Hemlock Design document.
;;;

(in-package :hemlock-internals)


;;;; DELETE-CHARACTERS.

(defvar *internal-temp-region* (make-empty-region))
(defvar *internal-temp-mark* (internal-make-mark nil nil :temporary))

(defmethod delete-characters (mark &optional (n 1))
  "Deletes N characters after the mark (or -N before if N is negative)."
  (let* ((line (mark-line mark))
         (charpos (mark-charpos mark))
         (length (line-length* line)))
    (cond
     ((zerop n) t)
     ;; Deleting chars on one line, just bump the pointers.
     ((<= 0 (+ charpos n) length)
      (modifying-buffer (line-%buffer line)
        (modifying-line line mark)
        (cond
         ((minusp n)
          (setq left-open-pos (+ left-open-pos n))
          (move-some-marks (pos line)
            (if (> pos left-open-pos)
                (if (<= pos charpos) left-open-pos (+ pos n))
                pos)))

         (t
          (setq right-open-pos (+ right-open-pos n))
          (let ((bound (+ charpos n)))
            (move-some-marks (pos line)
              (if (> pos charpos)
                  (if (<= pos bound) left-open-pos (- pos n))
                  pos))))) t))

     ;; Deleting some newlines, punt out to delete-region.
     (t
      (setf (mark-line *internal-temp-mark*) line
            (mark-charpos *internal-temp-mark*) charpos)
      (let ((other-mark (character-offset *internal-temp-mark* n)))
        (cond
         (other-mark
          (if (< n 0)
              (setf (region-start *internal-temp-region*) other-mark
                    (region-end *internal-temp-region*) mark)
              (setf (region-start *internal-temp-region*) mark
                    (region-end *internal-temp-region*) other-mark))
          (delete-region *internal-temp-region*)
          t)
         (t nil)))))))



;;;; DELETE-REGION.

(defmethod delete-region (region)
  "Deletes the Region."
  (let* ((start (region-start region))
         (end (region-end region))
         (first-line (mark-line start))
         (last-line (mark-line end))
         (first-charpos (mark-charpos start))
         (last-charpos (mark-charpos end))
         (buffer (line-%buffer first-line)))
    (unless (and (eq first-line last-line)
                 (= first-charpos last-charpos))
      (modifying-buffer buffer
        (cond ((eq first-line last-line)
               ;; Simple case -- just skip over the characters:
               (modifying-line first-line start)
               (let ((num (- last-charpos first-charpos)))
                 (setq right-open-pos (+ right-open-pos num))
                 ;; and fix up any marks in there:
                 (move-some-marks (charpos first-line)
                   (if (> charpos first-charpos)
                       (if (<= charpos last-charpos)
                           first-charpos
                           (- charpos num))
                       charpos))))
              (t
               ;; hairy case -- squish lines together:
               (close-line)
               (let* ((first-chars (line-chars first-line))
                      (last-chars (line-chars last-line))
                      (last-length (length last-chars)))
                 (declare (simple-string last-chars first-chars))
                 ;; Cons new chars for the first line.
                 (let* ((length (+ first-charpos (- last-length last-charpos)))
                        (new-chars (make-string length)))
                   (%sp-byte-blt first-chars 0 new-chars 0 first-charpos)
                   (%sp-byte-blt last-chars last-charpos new-chars first-charpos
                                 length)
                   (setf (line-chars first-line) new-chars))
                 ;; fix up the first line's marks:
                 (move-some-marks (charpos first-line)
                   (if (> charpos first-charpos)
                       first-charpos
                       charpos))
                 ;; fix up the marks of the lines in the middle and mash
                 ;;line-%buffer:
                 (do* ((line (line-next first-line) (line-next line))
                       (count (incf *disembodied-buffer-counter*)))
                      ((eq line last-line)
                       (setf (line-%buffer last-line) count))
                   (setf (line-%buffer line) count)
                   (move-some-marks (ignore line first-line)
                     (declare (ignore ignore))
                     first-charpos))
                 ;; and fix up the last line's marks:
                 (move-some-marks (charpos last-line first-line)
                   (if (<= charpos last-charpos)
                       first-charpos
                       (+ (- charpos last-charpos)
                          first-charpos)))
                 ;; And splice the losers out:
                 (let ((next (line-next last-line)))
                   (setf (line-next first-line) next)
                   (when next (setf (line-previous next) first-line))))))))))



;;;; DELETE-AND-SAVE-REGION.

(defmethod delete-and-save-region (region)
  "Deletes Region and returns a region containing the deleted characters."
  (let* ((start (region-start region))
         (end (region-end region))
         (first-line (mark-line start))
         (last-line (mark-line end))
         (first-charpos (mark-charpos start))
         (last-charpos (mark-charpos end))
         (buffer (line-%buffer first-line)))
    (cond
     ((and (eq first-line last-line)
           (= first-charpos last-charpos))
      (make-empty-region))
     (t
      (modifying-buffer buffer
        (cond ((eq first-line last-line)
               ;; simple case -- just skip over the characters:
               (modifying-line first-line start)
               (let* ((num (- last-charpos first-charpos))
                      (new-right (+ right-open-pos num))
                      (new-chars (make-string num))
                      (new-line (make-line
                                 :chars new-chars  :number 0
                                 :%buffer (incf *disembodied-buffer-counter*))))
                 (declare (simple-string new-chars))
                 (%sp-byte-blt open-chars right-open-pos new-chars 0 num)
                 (setq right-open-pos new-right)
                 ;; and fix up any marks in there:
                 (move-some-marks (charpos first-line)
                   (if (> charpos first-charpos)
                       (if (<= charpos last-charpos)
                           first-charpos
                           (- charpos num))
                       charpos))
                 ;; And return the region with the nuked characters:
                 (internal-make-region (mark new-line 0 :right-inserting)
                                       (mark new-line num :left-inserting))))
              (t
               ;; hairy case -- squish lines together:
               (close-line)
               (let* ((first-chars (line-chars first-line))
                      (last-chars (line-chars last-line))
                      (first-length (length first-chars))
                      (last-length (length last-chars))
                      (saved-first-length (- first-length first-charpos))
                      (saved-first-chars (make-string saved-first-length))
                      (saved-last-chars (make-string last-charpos))
                      (count (incf *disembodied-buffer-counter*))
                      (saved-line (make-line :chars saved-first-chars
                                             :%buffer count)))
                 (declare (simple-string first-chars last-chars
                                         saved-first-chars saved-last-chars))
                 ;; Cons new chars for victim line.
                 (let* ((length (+ first-charpos (- last-length last-charpos)))
                        (new-chars (make-string length)))
                   (%sp-byte-blt first-chars 0 new-chars 0 first-charpos)
                   (%sp-byte-blt last-chars last-charpos new-chars first-charpos
                                 length)
                   (setf (line-chars first-line) new-chars))
                 ;; Make a region with all the lost stuff:
                 (%sp-byte-blt first-chars first-charpos saved-first-chars 0
                               saved-first-length)
                 (%sp-byte-blt last-chars 0 saved-last-chars 0 last-charpos)
                 ;; Mash the chars and buff of the last line.
                 (setf (line-chars last-line) saved-last-chars
                       (line-%buffer last-line) count)
                 ;; fix up the marks of the lines in the middle and mash
                 ;;line-%buffer:
                 (do ((line (line-next first-line) (line-next line)))
                     ((eq line last-line)
                      (setf (line-%buffer last-line) count))
                   (setf (line-%buffer line) count)
                   (move-some-marks (ignore line first-line)
                     (declare (ignore ignore))
                     first-charpos))
                 ;; And splice the losers out:
                 (let ((next (line-next first-line))
                       (after (line-next last-line)))
                   (setf (line-next saved-line) next
                         (line-previous next) saved-line
                         (line-next first-line) after)
                   (when after
                     (setf (line-previous after) first-line
                           (line-next last-line) nil)))

                 ;; fix up the first line's marks:
                 (move-some-marks (charpos first-line)
                   (if (> charpos first-charpos)
                       first-charpos
                       charpos))
                 ;; and fix up the last line's marks:
                 (move-some-marks (charpos last-line first-line)
                   (if (<= charpos last-charpos)
                       first-charpos
                       (+ (- charpos last-charpos)
                          first-charpos)))
                 ;; And return the region with the nuked characters:
                 (renumber-region
                  (internal-make-region
                   (mark saved-line 0 :right-inserting)
                   (mark last-line last-charpos :left-inserting)))))))))))



;;;; COPY-REGION.

(defun copy-region (region)
  "Returns a region containing a copy of the text within Region."
  (let* ((start (region-start region))
         (end (region-end region))
         (first-line (mark-line start))
         (last-line (mark-line end))
         (first-charpos (mark-charpos start))
         (last-charpos (mark-charpos end))
         (count (incf *disembodied-buffer-counter*)))
    (cond
     ((eq first-line last-line)
      (when (eq first-line open-line) (close-line))
      (let* ((length (- last-charpos first-charpos))
             (chars (make-string length))
             (line (make-line :chars chars  :%buffer count  :number 0)))
        (%sp-byte-blt (line-chars first-line) first-charpos chars 0 length)
        (internal-make-region (mark line 0 :right-inserting)
                              (mark line length :left-inserting))))
     (t
      (close-line)
      (let* ((first-chars (line-chars first-line))
             (length (- (length first-chars) first-charpos))
             (chars (make-string length))
             (first-copied-line (make-line :chars chars  :%buffer count
                                           :number 0)))
        (declare (simple-string first-chars))
        (%sp-byte-blt first-chars first-charpos chars 0 length)
        (do ((line (line-next first-line) (line-next line))
             (previous first-copied-line)
             (number line-increment (+ number line-increment)))
            ((eq line last-line)
             (let* ((chars (make-string last-charpos))
                    (last-copied-line (make-line :chars chars
                                                 :number number
                                                 :%buffer count
                                                 :previous previous)))
               (%sp-byte-blt (line-chars last-line) 0 chars 0 last-charpos)
               (setf (line-next previous) last-copied-line)
               (internal-make-region
                (mark first-copied-line 0 :right-inserting)
                (mark last-copied-line last-charpos :left-inserting))))
          (let* ((new-line (%copy-line line :%buffer count
                                       :number number
                                       :previous previous)))
            (setf (line-next previous) new-line)
            (setq previous new-line))))))))



;;;; FILTER-REGION.

(eval-when (:compile-toplevel :execute)
(defmacro fcs (fun str)
  `(let ((rs (funcall ,fun ,str)))
     (if (simple-string-p rs) rs
         (coerce rs 'simple-string))))
); eval-when

;;; FILTER-REGION  --  Public
;;;
;;;    After we deal with the nasty boundry conditions of the first and
;;; last lines, we just scan through lines in the region replacing their
;;; chars with the result of applying the function to the chars.
;;;
(defun filter-region (function region)
  "This function filters the text in a region though a Lisp function.  The
   argument function must map from a string to a string.  It is passed each
   line string from region in order, and each resulting string replaces the
   original.  The function must neither destructively modify its argument nor
   modify the result string after it is returned.  The argument will always be
   a simple-string.  It is an error for any string returned to contain
   newlines."
  (let* ((start (region-start region))
         (start-line (mark-line start))
         (first (mark-charpos start))
         (end (region-end region))
         (end-line (mark-line end))
         (last (mark-charpos end))
         (marks ()))
    (modifying-buffer (line-%buffer start-line)
      (modifying-line end-line end)
      (cond ((eq start-line end-line)
             (let* ((res (fcs function (subseq open-chars first last)))
                    (rlen (length res))
                    (new-left (+ first rlen))
                    (delta (- new-left left-open-pos)))
               (declare (simple-string res))
               (when (> new-left right-open-pos)
                 (grow-open-chars (+ new-left line-cache-length)))
               (%sp-byte-blt res 0 open-chars first left-open-pos)
               ;;
               ;; Move marks to start or end of region, depending on kind.
               (dolist (m (line-marks start-line))
                 (let ((charpos (mark-charpos m)))
                   (when (>= charpos first)
                     (setf (mark-charpos m)
                           (if (<= charpos last)
                               (if (eq (mark-%kind m) :left-inserting)
                                   new-left first)
                               (+ charpos delta))))))
               (setq left-open-pos new-left)))
            (t
             ;;
             ;; Do the chars for the first line.
             (let* ((first-chars (line-chars start-line))
                    (first-len (length first-chars))
                    (res (fcs function (subseq first-chars first first-len)))
                    (rlen (length res))
                    (nlen (+ first rlen))
                    (new (make-string nlen)))
               (declare (simple-string res first-chars new))
               (%sp-byte-blt first-chars 0 new 0 first)
               (%sp-byte-blt res 0 new first nlen)
               (setf (line-chars start-line) new))
             ;;
             ;; Fix up marks on the first line, saving any within the region
             ;; to be dealt with later.
             (let ((outside ()))
               (dolist (m (line-marks start-line))
                 (if (<= (mark-charpos m) first)
                     (push m outside) (push m marks)))
               (setf (line-marks start-line) outside))
             ;;
             ;; Do chars of intermediate lines in the region, saving their
             ;; marks.
             (do ((line (line-next start-line) (line-next line)))
                 ((eq line end-line))
               (when (line-marks line)
                 (setq marks (nconc (line-marks line) marks))
                 (setf (line-marks line) nil))
               (setf (line-chars line) (fcs function (line-chars line))))
             ;;
             ;; Do the last line, which is cached.
             (let* ((res (fcs function (subseq (the simple-string open-chars)
                                               0 last)))
                    (rlen (length res))
                    (delta (- rlen last)))
               (declare (simple-string res))
               (when (> rlen right-open-pos)
                 (grow-open-chars (+ rlen line-cache-length)))
               (%sp-byte-blt res 0 open-chars 0 rlen)
               (setq left-open-pos rlen)
               ;;
               ;; Adjust marks after the end of the region and save ones in it.
               (let ((outside ()))
                 (dolist (m (line-marks end-line))
                   (let ((charpos (mark-charpos m)))
                     (cond ((> charpos last)
                            (setf (mark-charpos m) (+ charpos delta))
                            (push m outside))
                           (t
                            (push m marks)))))
                 (setf (line-marks end-line) outside))
               ;;
               ;; Scan over saved marks, moving them to the correct end of the
               ;; region.
               (dolist (m marks)
                 (cond ((eq (mark-%kind m) :left-inserting)
                        (setf (mark-charpos m) rlen)
                        (setf (mark-line m) end-line)
                        (push m (line-marks end-line)))
                       (t
                        (setf (mark-charpos m) first)
                        (setf (mark-line m) start-line)
                        (push m (line-marks start-line)))))))))
    region))
