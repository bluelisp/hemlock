(in-package :hemlock-vim)



(defun add-at-current (point delta)
  ;; TODO: octal, hex, keep number of characters
  (with-mark ((mark point))
    (if (word-offset mark -1)
      (filter-region
        (lambda (stg)
          (format nil "~d"
                  (+ delta
                   (parse-integer stg
                                  :junk-allowed T))))
        (region mark point))
      (editor-error "Not enough words."))))


(defcommand "Decrement at Cursor" (p)
  "Increments the number at the cursor"
  "Increments the number at the cursor"
  (add-at-current (current-point) -1))
(defcommand "Increment at Cursor" (p)
  "Increments the number at the cursor"
  "Increments the number at the cursor"
  (add-at-current (current-point) +1))


(in-mode ("vim-normal-count")
  (bind-key "Into Normal Mode" #k"escape")
  (bind-key "Argument Digit" #k"1")
  (bind-key "Argument Digit" #k"2")
  (bind-key "Argument Digit" #k"3")
  (bind-key "Argument Digit" #k"4")
  (bind-key "Argument Digit" #k"5")
  (bind-key "Argument Digit" #k"6")
  (bind-key "Argument Digit" #k"7")
  (bind-key "Argument Digit" #k"8")
  (bind-key "Argument Digit" #k"9"))

(in-mode ("vim-normal-count0")
  (bind-key "Into Normal Mode" #k"escape")
  (bind-key "Argument Digit" #k"0"))


(in-mode ("vim-normal")
  (bind-key "Into Normal Mode" #k"escape")
  (bind-key "Into Cmdline Mode" #k":")
  (bind-key "Into Insert Mode" #k"i")

  (bind-key "Beginning of Line" #k"0")
  (bind-key "Beginning of Line" #k"^") ; TODO: first non-space char
  (bind-key "End of Line" #k"$")

  (bind-key "Into Normal/0 Mode" #k"1")
  (bind-key "Into Normal/0 Mode" #k"2")
  (bind-key "Into Normal/0 Mode" #k"3")
  (bind-key "Into Normal/0 Mode" #k"4")
  (bind-key "Into Normal/0 Mode" #k"5")
  (bind-key "Into Normal/0 Mode" #k"6")
  (bind-key "Into Normal/0 Mode" #k"7")
  (bind-key "Into Normal/0 Mode" #k"8")
  (bind-key "Into Normal/0 Mode" #k"9")

  (bind-key "Top of Window" #k"H")
  (bind-key "Line to Center of Window" #k"z z")
  (bind-key "Bottom of Window" #k"L")

  (bind-key "Forward Search" #k"/")
  (bind-key "Reverse Search" #k"?")

  (bind-key "Goto Absolute Line" #k"G")

  (bind-key "Increment at Cursor" #k"control-a")
  (bind-key "Decrement at Cursor" #k"control-x")

  (bind-key "Next Line" #k"j")
  (bind-key "Next Line" #k"downarrow")

  (bind-key "Previous Line" #k"k")
  (bind-key "Previous Line" #k"uparrow")

  (bind-key "Forward Character" #k"l")
  (bind-key "Forward Character" #k"rightarrow")

  (bind-key "Backward Character" #k"h")
  (bind-key "Backward Character" #k"leftarrow")

  (bind-key "Forward Word" #k"w")
  (bind-key "Backward Word" #k"b")
  (bind-key "Kill Next Word" #k"d w")

  (bind-key "Split Window" #k"control-w control-s")
  (bind-key "Next Window" #k"control-w control-w")
  (bind-key "Delete Window" #k"control-w control-c")

  (bind-key "Here to Top of Window" #k"leftdown")
  (bind-key "Point to Here" #k"middledown")

  (bind-key "Scroll Window Down" #k"control-f")
  (bind-key "Scroll Window Up" #k"control-b")
  )

(in-mode ("vim-normal-count")
  (bind-key "Argument Digit" #k"0")
  (bind-key "Argument Digit" #k"1")
  (bind-key "Argument Digit" #k"2")
  (bind-key "Argument Digit" #k"3")
  (bind-key "Argument Digit" #k"4")
  (bind-key "Argument Digit" #k"5")
  (bind-key "Argument Digit" #k"6")
  (bind-key "Argument Digit" #k"7")
  (bind-key "Argument Digit" #k"8")
  (bind-key "Argument Digit" #k"9")
)
