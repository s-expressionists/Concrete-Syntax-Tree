(cl:in-package #:asdf-user)

(defsystem concrete-syntax-tree-test
  :depends-on (:concrete-syntax-tree)
  :components
  ((:file "packages")
   (:file "random-expression")
   (:file "cst-from-expression")
   (:file "random-sources")))
