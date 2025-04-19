(defsystem "concrete-syntax-tree"
  :description "Library for parsing Common Lisp code into a concrete syntax tree."
  :license "BSD" ; See LICENSE file for details
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :version (:read-file-form "version-string.sexp")
  :depends-on ("concrete-syntax-tree-base"
               "concrete-syntax-tree-lambda-list")
  :in-order-to ((test-op (test-op "concrete-syntax-tree/test"))))

(defsystem "concrete-syntax-tree/test"
  :depends-on ("concrete-syntax-tree")
  :pathname "Test"
  :components ((:file "packages")
               (:file "random-expression")
               (:file "cst-from-expression")
               (:file "quasiquotation")
               (:file "random-sources")
               (:file "reconstruct"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:concrete-syntax-tree-test '#:run-tests)))
