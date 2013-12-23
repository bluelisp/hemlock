(in-package :hi)


(defmethod backend-init-raw-io ((backend (eql :tty)) display)
  (declare (ignore display))
  ;; The editor's file descriptor is Unix standard input (0).
  ;; We don't need to affect system:*file-input-handlers* here
  ;; because the init and exit methods for tty redisplay devices
  ;; take care of this.
  ;;
  (setf *editor-file-descriptor* 0)
  (setf *editor-input* (make-tty-editor-input :fd 0))
  (setf *real-editor-input* *editor-input*))

