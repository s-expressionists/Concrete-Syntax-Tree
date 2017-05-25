(cl:in-package #:asdf-user)

(defsystem #:concrete-syntax-tree-lambda-list-test
  :depends-on (#:concrete-syntax-tree-lambda-list)
  :components
  ((:file "packages")))
