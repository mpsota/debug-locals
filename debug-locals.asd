;;;; debug-locals.asd

(asdf:defsystem #:debug-locals
  :description "debug-locals - solves [No Locals] issue while debugging local variables under SBCL/CCL"
  :author "Michał Psota <mpsota@lisp.pl>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "debug-locals")))

(asdf:defsystem #:debug-locals/examples
  :description "debug-locals examples"
  :author "Michał Psota <mpsota@lisp.pl>"
  :license "MIT"
  :serial t
  :depends-on (:debug-locals)
  :components ((:file "examples")))
