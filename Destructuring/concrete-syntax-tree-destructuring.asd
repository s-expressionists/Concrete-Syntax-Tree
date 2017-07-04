(cl:in-package #:asdf-user)

(defsystem :concrete-syntax-tree-destructuring
  :depends-on (:concrete-syntax-tree-lambda-list)
  :serial t
  :components
  ((:file "generic-functions")
   (:file "required-parameters")
   (:file "optional-parameters")
   (:file "rest-parameters")
   (:file "key-parameters")
   (:file "aux-parameters")
   (:file "lambda-list")
   (:file "parse-macro")))
