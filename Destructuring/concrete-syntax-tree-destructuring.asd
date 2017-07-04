(cl:in-package #:asdf-user)

(defsystem :concrete-syntax-tree-destructuring
  :depends-on (:concrete-syntax-tree-lambda-list)
  :serial t
  :components
  ((:file "parse-macro")))
