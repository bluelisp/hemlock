(defpackage :hemlock-vim
  (:use :cl :hemlock :hemlock-interface))
(in-package :hemlock-vim)


(defmode 
  "vim-normal"
  :major-p T
  :transparent-p NIL
  :documentation "Normal mode.")

(defmode 
  "vim-insert"
  :major-p T
  :transparent-p NIL
  :documentation "Insert mode")

(defmode 
  "vim-cmdline"
  :major-p T
  :transparent-p NIL
  :documentation "Cmdline mode")



(defmacro in-mode ((which) &body body)
  `(progn
     ,@(mapcar
       (lambda (form)
         (if (eq (first form) 'hemlock-interface:bind-key)
           (append form `(:mode ,which))
           form))
       body)))
    

(defcommand "Into Normal Mode" (p)
            "puts the buffer into Normal mode"
            "puts the buffer into Normal mode"
            (setf (buffer-major-mode (current-buffer))
                  "vim-normal"))
(defcommand "Into Insert Mode" (p)
            "puts the buffer into Insert mode"
            "puts the buffer into Insert mode"
            (setf (buffer-major-mode (current-buffer))
                  "vim-insert"))
(defcommand "Into Cmdline Mode" (p)
            "puts the buffer into Cmdline mode"
            "puts the buffer into Cmdline mode"
            (setf (buffer-major-mode (current-buffer))
                  "vim-cmdline"))

(in-mode ("vim-normal")
  (bind-key "Into Normal Mode" #k"escape")
  (bind-key "Into Cmdline Mode" #k":")
  (bind-key "Into Insert Mode" #k"i")
  (bind-key "Beginning of Line" #k"0")
  (bind-key "Beginning of Line" #k"^") ; TODO: first non-space char
  (bind-key "End of Line" #k"$")
  (bind-key "Forward Search" #k"/")
  (bind-key "Reverse Search" #k"?")
  (bind-key "Next Line" #k"j")
  (bind-key "Previous Line" #k"k")
  (bind-key "Forward Word" #k"w")
  (bind-key "Backward Word" #k"b")
  (bind-key "Here to Top of Window" #k"leftdown")
  (bind-key "Point to Here" #k"middledown")
  (bind-key "Scroll Window Down" #k"control-f")
(bind-key "Scroll Window Up" #k"control-b")
  )

(in-mode ("vim-cmdline")
  (bind-key "Into Normal Mode" #k"escape")
  )

(in-mode ("vim-insert")
  (bind-key "Into Normal Mode" #k"escape")
  )

