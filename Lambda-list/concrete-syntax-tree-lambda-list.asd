(cl:in-package #:asdf-user)

(defsystem #:concrete-syntax-tree-lambda-list
  :serial t
  :components
  ((:file "client")
   (:file "lambda-list-keywords")
   (:file "lambda-list-types")
   (:file "grammar")
   (:file "standard-grammars")))

