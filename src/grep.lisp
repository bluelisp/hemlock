;;;; -*- Mode: Lisp; indent-with-tabs: nil -*-

(in-package :hemlock)

(defmode "Grep" :major-p t
  :documentation
  "Grep results")

(defcommand "Grep"
    (p &optional (command (hi::prompt-for-string
                           :prompt "Run grep (like this): "
                           :default "grep -nH -e "))
                 (directory (default-directory)))
  "" ""
  (setf (buffer-major-mode (shell-command-command p command directory))
        "Grep"))

(defcommand "Grep Goto" (p)
  "" ""
  (declare (ignore p))
  (cl-ppcre:register-groups-bind (file line)
      ("^\([^:]+\):\([0-9]+\):" (line-string (mark-line (current-point))))
    (when file
      (change-to-buffer (find-file-buffer (merge-pathnames file (default-directory))))
      (let ((point (current-point)))
        (push-buffer-mark (copy-mark point))
        (buffer-start point)
        (when line
          (line-offset point (1- (parse-integer line))))))))

(bind-key "Grep Goto" #k"return" :mode "Grep")
