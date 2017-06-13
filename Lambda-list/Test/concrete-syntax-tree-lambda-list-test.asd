(cl:in-package #:asdf-user)

(defsystem #:concrete-syntax-tree-lambda-list-test
  :depends-on (#:concrete-syntax-tree-lambda-list)
  :components
  ((:file "packages")
   (:file "random-lambda-list")
   (:file "compare-parse-trees")
   (:file "parsers")
   (:file "test")))
