;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :clim-internals)

(in-package :clim-clx)

#+NIL
(defmethod medium-draw-text* ((medium clx-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                              x y)
    (with-clx-graphics (medium)
      (when (characterp string)
        (setq string (make-string 1 :initial-element string)))
      (when (null end) (setq end (length string)))
      (multiple-value-bind (text-width text-height x-cursor y-cursor baseline)
          (text-size medium string :start start :end end)
        (declare (ignore x-cursor y-cursor))
        (unless (and (eq align-x :left) (eq align-y :baseline))
          (setq x (- x (ecase align-x
                         (:left 0)
                         (:center (round text-width 2))
                         (:right text-width))))
          (setq y (ecase align-y
                    (:top (+ y (xlib:font-ascent (xlib:gcontext-font gc))))
                    (:center (+ y baseline (- (floor text-height 2))))
                    (:baseline y)
                    (:bottom (+ y baseline (- text-height)))))))
      (let ((x (round-coordinate x))
            (y (round-coordinate y)))
        (when (and (<= #x-8000 x #x7FFF)
                   (<= #x-8000 y #x7FFF))
          (multiple-value-bind (halt width)
              (xlib:draw-glyphs mirror gc x y string
                                :start start :end end
                                :translate #'translate)))))))

#+NIL
(defmethod medium-draw-text* ((medium clx-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                              x y)
    (with-clx-graphics (medium)
      (when (characterp string)
        (setq string (make-string 1 :initial-element string)))
      (when (null end) (setq end (length string)))
      (multiple-value-bind (text-width text-height x-cursor y-cursor baseline)
          (unless (and (eq align-x :left)
                       (member align-y '(:baseline :top)))
            (text-size medium string :start start :end end))
        (declare (ignore x-cursor y-cursor))
        (unless (and (eq align-x :left) (eq align-y :baseline))
          (setq x (- x (ecase align-x
                         (:left 0)
                         (:center (round text-width 2))
                         (:right text-width))))
          (setq y (ecase align-y
                    (:top (+ y (xlib:font-ascent (xlib:gcontext-font gc))))
                    (:center (+ y baseline (- (floor text-height 2))))
                    (:baseline y)
                    (:bottom (+ y baseline (- text-height)))))))
      (let ((x (round-coordinate x))
            (y (round-coordinate y)))
        (when (and (<= #x-8000 x #x7FFF)
                   (<= #x-8000 y #x7FFF))
          (multiple-value-bind (halt width)
              (xlib:draw-glyphs mirror gc x y string
                                :start start :end end
                                :translate #'translate)))))))