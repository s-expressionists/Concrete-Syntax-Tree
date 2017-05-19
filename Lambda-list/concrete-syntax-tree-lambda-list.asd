(cl:in-package #:asdf-user)

(defsystem #:concrete-syntax-tree-lambda-list
  :serial t
  :components
  ((:file "client")
   (:file "lambda-list-types")
   (:file "lambda-list-keywords")
   (:file "grammar-symbols")
   (:file "grammar")
   (:file "standard-grammars")
   (:file "earley-item")
   (:file "earley")))
