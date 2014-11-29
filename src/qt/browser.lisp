;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock.qt)

(named-readtables:in-readtable :hemlock.qt)

(defun note-webkit-title-changed (buffer title)
  (hi::rename-buffer-uniquely buffer
                              (format nil "*Webkit* [~A]" title)))

(defun make-browser-buffer (name url)
  (unless (hi::find-buffer name)
    (let ((widget (#_new QWebView)))
      (#_setUrl widget (#_new QUrl url))
      (let ((buffer
             (make-virtual-buffer name widget :modes '("QWebView"))))
        #+nil
        (connect/string widget
                        (QSIGNAL "titleChanged(const QString&)")
                        (lambda (title)
                          (note-webkit-title-changed buffer title)))
        #+nil
        (connect widget
                 (QSIGNAL "loadStarted()")
                 (lambda ()
                   (message "Loading page...")))
        #+nil
        (connect/boolean widget
                         (QSIGNAL "loadFinished(bool)")
                         (lambda (ok)
                           (message (if ok
                                        "Page loaded."
                                        "Failed to load page."))))
        #+nil
        (connect/int widget
                     (QSIGNAL "loadProgress(int)")
                     (lambda (p)
                       (message "Loading page... ~D%" p)))
        buffer))))

(defun ensure-browser-buffer (name url &aux *)
  (cond
    ((setf * (hi::find-buffer name))
     (#_setUrl (hi::buffer-widget *) (#_new QUrl url))
     *)
    (t
     (make-browser-buffer name url))))

(defvar *qt-documentation-root*
  "file:///home/david/src/qt4-x11-4.4.3/doc/html/")

(defcommand "Browse Qt Documentation" (p)
  "" ""
  (declare (ignore p))
  (let ((buffer
         (ensure-browser-buffer "*Webkit*" *qt-documentation-root*)))
    (when buffer
      (change-to-buffer buffer))))

(defcommand "Browse Qt Class"
    (p &optional (class-name (hi::prompt-for-string :prompt "Class: ")))
  "" ""
  (declare (ignore p))
  (let* ((url (format nil "~A/~A.html"
                      *qt-documentation-root*
                      (string-downcase class-name)))
         (buffer (ensure-browser-buffer "*Webkit*" url)))
    (when buffer
      (change-to-buffer buffer))))

(defcommand "Browse" (p &optional url)
  "" ""
  (declare (ignore p))
  (let ((buffer
         (ensure-browser-buffer
          "*Webkit*"
          (or url (hi::prompt-for-url :prompt "URL: ")))))
    (when buffer
      (change-to-buffer buffer))))

(defcommand "Google" (p &optional search-term)
  "" ""
  (declare (ignore p))
  (let ((buffer
         (ensure-browser-buffer
          "*Webkit*"
          (concatenate 'string
                       "http://google.com/search?q="
                       (or search-term (hi::prompt-for-string))))))
    (when buffer
      (change-to-buffer buffer))))

(defcommand "clhs" (p &optional search-term)
  "" ""
  (declare (ignore p))
  (let ((buffer
         (ensure-browser-buffer
          "*Webkit*"
          (concatenate 'string
                       "http://l1sp.org/cl/"
                       (or search-term (hi::prompt-for-string
                                        :prompt "Symbol: "))))))
    (when buffer
      (change-to-buffer buffer))))

(defcommand "Browser Toggle Full Screen" (p)
  "" ""
  (declare (ignore p))
  (#_showFullScreen (#_window *main-stack*)))

(defmode "QWebView" :major-p t)

(defun qwebview-scroll-page (f view)
  (let ((frame (#_currentFrame (#_page view))))
    (#_setScrollBarValue frame
                         (#_Qt::Vertical)
                         (+ (#_scrollBarValue frame (#_Qt::Vertical))
                            (* f (#_height view))))))

(defcommand "QWebView Page Down" (p)
  "" ""
  (declare (ignore p))
  (qwebview-scroll-page 1 (hi::buffer-widget (current-buffer))))

(defcommand "QWebView Page Up" (p)
  "" ""
  (declare (ignore p))
  (qwebview-scroll-page -1 (hi::buffer-widget (current-buffer))))

(defun qwebview-scroll-fixed (c view)
  (let ((frame (#_currentFrame (#_page view))))
    (#_setScrollBarValue frame
                         (#_Qt::Vertical)
                         (+ c (#_scrollBarValue frame (#_Qt::Vertical))))))

(defcommand "QWebView Scroll Down" (p)
  "" ""
  (declare (ignore p))
  (qwebview-scroll-fixed 20 (hi::buffer-widget (current-buffer))))

(defcommand "QWebView Scroll Up" (p)
  "" ""
  (declare (ignore p))
  (qwebview-scroll-fixed -20 (hi::buffer-widget (current-buffer))))

(defun qwebview-scroll-to (f view)
  (let ((frame (#_currentFrame (#_page view))))
    (#_setScrollBarValue frame
                         (#_Qt::Vertical)
                         (* f
                            (max 0
                                 (- (#_height (#_contentsSize frame))
                                    (#_height view)))))))

(defcommand "QWebView Scroll Top" (p)
  "" ""
  (declare (ignore p))
  (qwebview-scroll-to 0 (hi::buffer-widget (current-buffer))))

(defcommand "QWebView Scroll Bottom" (p)
  "" ""
  (declare (ignore p))
  (qwebview-scroll-to 1 (hi::buffer-widget (current-buffer))))

(defcommand "QWebView Back" (p)
  "" ""
  (declare (ignore p))
  (#_back (hi::buffer-widget (current-buffer))))

(defcommand "QWebView Forward" (p)
  "" ""
  (declare (ignore p))
  (#_forward (hi::buffer-widget (current-buffer))))

(defhvar "Previous Search Term" "QWebView Search Term" :mode "QWebView")

(defcommand "QWebView Find Text"
    (p &optional (search-term
                  (hi::prompt-for-string
                   :default (value hemlock::previous-search-term))))
  "" ""
  (declare (ignore p))
  (setf (value hemlock::previous-search-term) search-term)
  (#_findText (hi::buffer-widget (current-buffer))
              search-term
              (#_QWebPage::FindWrapsAroundDocument)))

(defcommand "QWebView Change URL" (p)
  "" ""
  (declare (ignore p))
  (let ((widget (hi::buffer-widget (current-buffer))))
    (#_load widget
            (#_new QUrl (hi::prompt-for-url
                         :default (#_toString (#_url widget)))))))

(bind-key "QWebView Page Down" #k"space" :mode "QWebView")
(bind-key "QWebView Page Down" #k"control-v" :mode "QWebView")
(bind-key "QWebView Page Up" #k"meta-v" :mode "QWebView")
(bind-key "QWebView Page Up" #k"backspace" :mode "QWebView")

(bind-key "QWebView Scroll Down" #k"control-n" :mode "QWebView")
(bind-key "QWebView Scroll Up" #k"control-p" :mode "QWebView")

(bind-key "QWebView Scroll Top" #k"meta-\<" :mode "QWebView")
(bind-key "QWebView Scroll Bottom" #k"meta-\>" :mode "QWebView")

(bind-key "QWebView Back" #k"meta-p" :mode "QWebView")
(bind-key "QWebView Forward" #k"meta-n" :mode "QWebView")

(bind-key "QWebView Find Text" #k"control-s" :mode "QWebView")
(bind-key "QWebView Change URL" #k"control-l" :mode "QWebView")

(bind-key "Enter Foreign Widget" #k"enter" :mode "QWebView")
