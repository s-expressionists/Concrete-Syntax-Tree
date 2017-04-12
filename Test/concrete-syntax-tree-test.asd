(cl:in-package #:asdf-user)

(defsystem concrete-syntax-tree-test
  :depends-on (:concrete-syntax-tree)
  :components
  ((:file "packages")))
