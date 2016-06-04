;;;; package.lisp

(defpackage #:debug-locals
  (:shadow #:let #:let* #:multiple-value-bind #:destructuring-bind)
  (:use #:cl)
  (:export #:defun*
           #:with-debug
           #:let
           #:let*
           #:multiple-value-bind
           #:destructuring-bind)
  )
