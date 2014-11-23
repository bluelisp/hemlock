(defpackage :hemlock-vim
  (:use :cl :hemlock :hemlock-interface))
(in-package :hemlock-vim)


(defmode
  "vim-normal"
  :major-p T
  :transparent-p NIL
  :documentation "Normal mode.")

(defmode
  "vim-normal-count"
  :major-p nil
  :transparent-p T
  :precedence 10
  :documentation "Normal mode, repetition count.")
(defmode
  "vim-normal-count0"
  :major-p nil
  :transparent-p NIL
  :precedence 11
  :documentation "Normal mode, repetition count, including 0.")



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
        "vim-normal"
        (key-translation #k"control-^") 
        nil
        (key-translation #k"escape")
        nil
        (buffer-minor-mode
          (current-buffer)
          "vim-normal-count") T))

(bind-key "Into Normal Mode" #k"control-v")


(defcommand "Into Normal/0 Mode" (p)
  "puts the buffer into Normal/0 mode"
  "puts the buffer into Normal/0 mode"
  #+nil(hemlock-internals::wind-bindings
         '("vim-normal-count0")))


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


;; delete all "self-insert" keys in the normal mode
;; doesn't work, seems to be default?
#+nil
(map-bindings
  (lambda (key command)
    ;(print (list key (command-name command)))
    (if (equal (command-name command)
               "Self Insert")
      (bind-key "Do Nothing" key)))
  :mode "vim-normal")


;;do-alpha-key-events
