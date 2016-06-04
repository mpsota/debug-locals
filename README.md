# debug-locals
Small common lisp library to debug local variables. Solves [No Locals] issue while debugging local variables under SBCL/CCL.

For now it supports local variables defined using:
 - `let`
 - `let*`
 - `multiple-value-bind`
 - `destructuring-bind`

## Sample usage:
```lisp
(ql:quickload :debug-locals)
```
In your defpackage form add:
```lisp
(:import-from #:debug-locals
              #:let
              #:let*
              #:multiple-value-bind
              #:destructuring-bind
              #:with-debug
              #:defun*)
```
Write function using `defun*`:
```
(defun* foo* ()
  (let ((x 5))
    (destructuring-bind (y) '(10)
      (break)
      (format t "~d + ~d = ~d" x y (+ x y)))))
```
Or using `with-debug` (remember about `(debug 3)` declaration):
```lisp
(defun foo ()
  (declare (optimize (debug 3)))
  (with-debug
    (let ((x 5))
      (break)
      (print x))))
```

## Examples

You can load examples defined at `debug-locals/examples` package:
```lisp
(ql:quickload :debug-locals/examples)
(in-package :debug-locals)
```
And run:
```lisp
(function-foo)
```
and
```
(function-foo*)
```
To see the difference.


Slime backtrace without debug-locals:

```slime
Backtrace:
  0: (FOO)
      [No Locals]
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (FOO) #<NULL-LEXENV>)
```

Slime backtrace with debug-locals:

```slime
Backtrace:
  0: (FOO)
      Locals:
        X = 5
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (FOO) #<NULL-LEXENV>)
```
