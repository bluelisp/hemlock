;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl-user)

(defpackage :hemlock.qt
  (:use :common-lisp :hemlock-interface :qt :iterate)
  (:shadow #:enable-syntax)
  (:export #:enable-syntax))
