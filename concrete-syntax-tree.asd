(cl:in-package #:asdf-user)

(defsystem :concrete-syntax-tree
  :depends-on (:concrete-syntax-tree-base
               :concrete-syntax-tree-lambda-list))
