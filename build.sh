#!/bin/sh
unset backends

if test $# -eq 0; then
	cat <<eof
Building backends tty and clx.
(Specify backend types at the command line to override this default.)
eof
else
	while test $# -gt 0; do
		case $1 in
			tty|clx|qt)
				backends="$backends :hemlock.$1"
				echo backend $1 enabled
				shift
				;;
			*)
				echo invalid backend type $1
				exit 1
				;;
		esac
	done
fi


clbuild lisp <<EOF

;; NOTE: the order in which clx and tty are given matters.
;; The last backend loaded is the default, and only if no $DISPLAY is
;; specified, main.lisp has special-cased the tty backend as a fallback,
;; so CLX must come last to have a chance of overriding it.
;;
(dolist (system (or '($backends) '(:hemlock.tty :hemlock.clx)))
  (asdf:operate 'asdf:load-op system))

(defun hemlock-toplevel ()
  #+ccl (when (find-package :qt) (funcall (find-symbol "REBIRTH" :qt)))
  (let ((argv0 (car sb-ext:*posix-argv*))) 
    (setf hi::*installation-directory*
	  (concatenate 
	   'string
	   (iolib.pathnames:file-path-directory argv0 :namestring t)
	   "/"))
    (setf hemlock::*slave-command* (list argv0 "--slave"))
    (hemlock:main))
  (quit))

(sb-ext:save-lisp-and-die "hemlock"
			  :toplevel 'hemlock-toplevel
			  :executable t)
EOF
