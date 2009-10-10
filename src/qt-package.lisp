;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl-user)

(defpackage :qt-hemlock
  (:use :common-lisp :hemlock-interface :qt :iterate)
  (:shadow #:enable-syntax)
  (:export #:enable-syntax))
