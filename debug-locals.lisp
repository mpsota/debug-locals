;;;; debug-locals.lisp

(in-package #:debug-locals)

;;; "debug-locals" goes here. Hacks and glory await!

;;; Code inspired by `(setf ,n ,n) from discussion on comp.lang.lisp "getting list-locals to tell me about all variables"
;;; https://groups.google.com/forum/#!topic/comp.lang.lisp/tWD_xSMBVaM

(defmacro portable-disable-package-locks (&rest symbols)
  "SBCL requires special declaration, to disable package lock"
  #+sbcl `'(sb-ext:disable-package-locks ,@symbols)
  #+ccl nil
  #-(or sbcl ccl) (error "implementation not supported"))

;; define cl symbols, so {let,let*…} in macrolet could shadow them.
(defmacro let (&rest args)
  `(cl:let ,@args))

(defmacro let* (&rest args)
  `(cl:let* ,@args))

(defmacro destructuring-bind (&rest args)
  `(cl:destructuring-bind ,@args))

(defmacro multiple-value-bind (&rest args)
  `(cl:multiple-value-bind ,@args))

(defmacro re-setf-variables (&rest vars)
  "To solve [no locals] issue, we need to re-setf variable"
  `(progn
     ,@(mapcar (lambda (x)
                 `(setf ,x ,x))
               vars)))

(defmacro with-binding-vars ((vars bindings) &body body)
  "Extract variable name from the `bindings', store list of vars as `vars'"
  `(cl:let ((,vars (mapcar (lambda (binding)
                            (typecase binding
                              (list (car binding))
                              (t binding)))
                            ,bindings)))
     ,@body))

;; Debug functions, with visible local variables.
;; Each variable from the bingins is re-setf.

(defmacro let-debug (bindings &body body)
  "Like `let' but with local-variables visible"
  (with-binding-vars (vars bindings)
    `(cl:let ,bindings
       (re-setf-variables ,@vars)
       ,@body)))

(defmacro let*-debug (bindings &body body)
  "Like `let*' but with local-variables visible"
  (with-binding-vars (vars bindings)
    `(cl:let* ,bindings
       (re-setf-variables ,@vars)
       ,@body)))


(defmacro destructuring-bind-debug (lambda-list expression &body body)
  "Like `destructuring-bind' but with local-variables visible"
  (cl:let* ((env-parameters '(&optional &rest &body &key &allow-other-keys &aux &environment &whole))
            (cleaned-lambda-list (remove-if (lambda (x) (member x env-parameters)) `,lambda-list)))
    (with-binding-vars (vars cleaned-lambda-list)
      `(cl:destructuring-bind ,lambda-list ,expression
         (re-setf-variables ,@vars)
         ,@body))))

(defmacro multiple-value-bind-debug (vars value-form &body body)
  "Like `multiple-value-bind' but with local-variables visible"
  `(cl:multiple-value-bind ,vars ,value-form
     (re-setf-variable ,vars)
     ,@body))

;; Exported macros

(defmacro with-debug (&body body)
  "Evaluate body inside environment where `let', `let*', `multiple-value-bind', `destructuring-bind'
 point to theirs debug equivalents.
Call this macro only from functions with (debug 3), otherwise there might be no effect."
  `(macrolet ((let (&rest args)
                `(let-debug ,@args))
              (let* (&rest args)
                `(let*-debug ,@args))
              (multiple-value-bind (&rest args)
                  `(multiple-value-bind-debug ,@args))
              (destructuring-bind (&rest args)
                  `(destructuring-bind-debug ,@args)))
     ,@body))

(defmacro defun* (name lambda-list &body body)
  "Like defun, but `with-debug' used around the body and with maximum debug settings."
  `(defun ,name ,lambda-list
     (declare (optimize (speed 0) (safety 3) (debug 3))
              ,(portable-disable-package-locks cl:let cl:let*))
     (with-debug
       ,@body)))
