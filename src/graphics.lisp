;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :qt-hemlock)

(named-readtables:in-readtable :qt-hemlock)

(defvar *dependency-graph-buffer* nil)

(defun make-graphics-buffer (name projects)
  (unless (qt-hemlock::find-buffer name)
    (let* ((widget (#_new QGraphicsView))
           (scene (#_new QGraphicsScene widget)))
      (#_setScene widget scene)
      (let ((graph (sugiyama::clbuild-dependency-graph projects)))
        (sugiyama::graph-to-scene graph scene)
        (#_setRenderHint widget (#_QPainter::Antialiasing) t)
        (let ((buffer
               (make-virtual-buffer
                name
                widget
                :modes '("Graphics")
                :delete-hook (list #'graphics-buffer-deleted))))
          (defhvar "current-graph" "" :buffer buffer :value graph)
          (setf *dependency-graph-buffer* buffer)
          buffer)))))

(defun graphics-buffer-deleted (buffer)
  (when (eq buffer *dependency-graph-buffer*)
    (setf *dependency-graph-buffer* nil)))

(defcommand "Show Project Graph" (p &optional projects)
  "" ""
  (declare (ignore p))
  (let ((name "*Dependency Graph*"))
    (let ((buf (qt-hemlock::find-buffer name)))
      (when buf
        (when (eq buf (current-buffer))
          (change-to-buffer (previous-buffer)))
        (delete-buffer buf)))
    (change-to-buffer (or (make-graphics-buffer name projects)
                          (progn
                            (message "Buffer already exists: ~A" name)
                            (qt-hemlock::find-buffer name))))))

(defcommand "Add Project To Graph"
    (p &optional (project (hi::prompt-for-string :prompt "Project: ")))
  "" ""
  (declare (ignore p))
  (let ((graph (variable-value 'hemlock::current-graph))
        (scene (#_scene (hi::buffer-widget (current-buffer)))))
    (sugiyama::update-graph-and-scene
     (lambda ()
       (iter (for node in-graph graph)
             (setf (sugiyama::node-color node) #xc8ddff))
       (sugiyama::merge-graph-into
        (sugiyama::clbuild-dependency-graph (list project))
        graph))
     graph
     scene)))

(defcommand "Layout Graph"
    (p)
  "" ""
  (declare (ignore p))
  (let ((graph (variable-value 'hemlock::current-graph))
        (scene (#_scene (hi::buffer-widget (current-buffer)))))
    (sugiyama::update-graph-and-scene
     (lambda ())
     graph
     scene)))

(defmode "Graphics" :major-p t)

(bind-key "test" #k"g" :mode "Graphics")
(bind-key "Project Graph" #k"a" :mode "Graphics")

(defun graph-scroll-y-page (f area)
  (let ((scroller (#_verticalScrollBar area)))
    (#_setValue scroller (+ (#_value scroller)
                            (* f (#_height area))))))

(defcommand "Graphics Page Down" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-y-page 1 (hi::buffer-widget (current-buffer))))

(defcommand "Graphics Page Up" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-y-page -1 (hi::buffer-widget (current-buffer))))

(defun graph-scroll-x-page (f area)
  (let ((scroller (#_horizontalScrollBar area)))
    (#_setValue scroller (+ (#_value scroller)
                            (* f (#_height area))))))

(defcommand "Graphics Page Left" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-x-page 1 (hi::buffer-widget (current-buffer))))

(defcommand "Graphics Page Right" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-x-page -1 (hi::buffer-widget (current-buffer))))

(defun graph-scroll-y-fixed (c area)
  (let ((scroller (#_verticalScrollBar area)))
    (#_setValue scroller (+ c (#_value scroller)))))

(defcommand "Graphics Scroll Down" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-y-fixed 20 (hi::buffer-widget (current-buffer))))

(defcommand "Graphics Scroll Up" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-y-fixed -20 (hi::buffer-widget (current-buffer))))

(defun graph-scroll-x-fixed (c area)
  (let ((scroller (#_horizontalScrollBar area)))
    (#_setValue scroller (+ c (#_value scroller)))))

(defcommand "Graphics Scroll Left" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-x-fixed 20 (hi::buffer-widget (current-buffer))))

(defcommand "Graphics Scroll Right" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-x-fixed -20 (hi::buffer-widget (current-buffer))))

(defun graph-scroll-y-to (f area)
  (let ((scroller (#_verticalScrollBar area)))
    (#_setValue scroller
                (* f
                   (max 0
                        (+ (#_maximum scroller)
                           (#_height area)))))))

(defcommand "Graphics Scroll Top" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-y-to 0 (hi::buffer-widget (current-buffer))))

(defun graph-scroll-x-to (f area)
  (let ((scroller (#_horizontalScrollBar area)))
    (#_setValue scroller
                (* f
                   (max 0
                        (+ (#_maximum scroller)
                           (#_height area)))))))

(defcommand "Graphics Scroll Bottom" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-y-to 1 (hi::buffer-widget (current-buffer))))

(defcommand "Graphics Scroll Leftmost" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-x-to 0 (hi::buffer-widget (current-buffer))))

(defcommand "Graphics Scroll Rightmost" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-x-to 1 (hi::buffer-widget (current-buffer))))

(defun assert-in-graphics-buffer ()
  (unless #+(or)
          (find "Graphics"
                (buffer-modes (current-buffer))
                :test #'string=)
          (eq (current-buffer) *dependency-graph-buffer*)
    (error "not in a graphics buffer")))

(defcommand "Graphics Quit" (p)
  "" ""
  (declare (ignore p))
  (assert-in-graphics-buffer)
  (delete-buffer-if-possible (current-buffer)))

(defcommand "Back To Clbuild Buffer" (p)
  "" ""
  (declare (ignore p))
  (assert-in-graphics-buffer)
  (hemlock::clbuild-command nil))

(bind-key "Graphics Page Down" #k"space" :mode "Graphics")
(bind-key "Graphics Page Down" #k"control-v" :mode "Graphics")
(bind-key "Graphics Page Up" #k"meta-v" :mode "Graphics")
(bind-key "Graphics Page Up" #k"backspace" :mode "Graphics")

(bind-key "Graphics Scroll Down" #k"control-n" :mode "Graphics")
(bind-key "Graphics Scroll Up" #k"control-p" :mode "Graphics")

(bind-key "Graphics Scroll Top" #k"meta-\<" :mode "Graphics")
(bind-key "Graphics Scroll Bottom" #k"meta-\>" :mode "Graphics")

(bind-key "Graphics Page Left" #k"control-x \>" :mode "Graphics")
(bind-key "Graphics Page Right" #k"control-x \<" :mode "Graphics")

(bind-key "Graphics Scroll Left" #k"\>" :mode "Graphics")
(bind-key "Graphics Scroll Right" #k"\<" :mode "Graphics")

(bind-key "Graphics Scroll Leftmost" #k"control-a" :mode "Graphics")
(bind-key "Graphics Scroll Rightmost" #k"control-e" :mode "Graphics")

(bind-key "Graphics Quit" #k"q" :mode "Graphics")
(bind-key "Back To Clbuild Buffer" #k"l" :mode "Graphics")

#+nil (bind-key "Enter Foreign Widget" #k"enter" :mode "Graphics")
