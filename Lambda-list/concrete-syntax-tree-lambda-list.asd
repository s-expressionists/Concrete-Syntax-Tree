(cl:in-package #:asdf-user)

(defsystem #:concrete-syntax-tree-lambda-list
  :depends-on (:concrete-syntax-tree-base)
  :serial t
  :components
  ((:file "client")
   (:file "ensure-proper")
   (:file "grammar-symbols")
   (:file "lambda-list-keywords")
   (:file "grammar")
   (:file "standard-grammars")
   (:file "earley-item")
   (:file "earley-state")
   (:file "parser")
   (:file "scanner-action")
   (:file "earley")
   (:file "parse-top-levels")
   (:file "unparse")))
