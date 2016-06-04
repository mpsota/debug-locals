(in-package #:debug-locals)

;;;; Examples

;; Function without defun* debug facilities
(defun function-foo ()
  (let ((x 5))
    (destructuring-bind (y) '(10)
      (break)
      (format t "~d + ~d = ~d" x y (+ x y)))))

;; Function with defun* debug facilities
(defun* function-foo* ()
  (let ((x 5))
    (destructuring-bind (y) '(10)
      (break)
      (format t "~d + ~d = ~d" x y (+ x y)))))

;; Function foo written using (with-debug …) macro.
(defun foo ()
  (declare (optimize (debug 3)))
  (with-debug
    (let ((x 5))
      (break)
      (print x))))
