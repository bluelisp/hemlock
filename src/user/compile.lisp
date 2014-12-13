;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock)

(defmode "Compile" :major-p t
  :documentation
  "Compile results")

(defvar *compilation-buffer* nil)

(defhvar "Compilation Command"
  "Holds the last compilation command executed.")

(defhvar "Compilation Directory"
  "Holds the directory of the last compilation.")

(defcommand "Shell Compile"
	    (p &optional (command (hi::prompt-for-string
                           :prompt "Compile (like this): "
                           :default (or (value compilation-command)
					"make -k ")))
                 (directory (or (value compilation-directory)
				(default-directory))))
  "Execute the \"Compilation Command\" as a shell command
   in a buffer named *compilation*.  The command will prompt
   for the compilation command, and will execute it in the
   current directory one is unless otherwise provided.  "
  "Execute the \"Compilation Command\" as a shell command
   in a buffer named *compilation*.  The command will prompt
   for the compilation command, and will execute it in the
   current directory one is unless otherwise provided.  "

  #+scl (setf directory (ext:unix-namestring directory nil))
  (let ((new-compile-buffer (shell-command-command p command directory)))
    (setf *compilation-buffer* new-compile-buffer
	  (buffer-major-mode new-compile-buffer) "Compile"
	  (value compilation-command) command
	  (value compilation-directory) directory
	  (buffer-name new-compile-buffer) "*compilation*")))

(defcommand "Shell Recompile" (p)
  "Execute the \"Compilation Command\" as a shell command
   in a buffer named *compilation*.  The command will not
   prompt for the compilation command, but will use the one
   provided previously.  Recompile evaluates the compilation
   command in the original directory provided."
  "Execute the \"Compilation Command\" as a shell command
   in a buffer named *compilation*.  The command will not
   prompt for the compilation command, but will use the one
   provided previously.  Recompile evaluates the compilation
   command in the original directory provided."

  (let ((new-compile-buffer (shell-command-command p
						   (value compilation-command)
						   (value compilation-directory))))
    (setf *compilation-buffer* new-compile-buffer
	  (buffer-major-mode new-compile-buffer) "Compile"
	  (buffer-name new-compile-buffer) "*compilation*")))

(defcommand "Compile Quit" (p)
  "Quit the *compilation* buffer."
  ""
  (when *compilation-buffer*
    (delete-buffer-if-possible *compilation-buffer*)))  

(bind-key "Compile Quit" #k"q" :mode "Compile")
