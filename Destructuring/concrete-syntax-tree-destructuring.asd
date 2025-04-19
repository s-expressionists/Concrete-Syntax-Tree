(defsystem "concrete-syntax-tree-destructuring"
  :depends-on ("concrete-syntax-tree-lambda-list")
  :serial t
  :components ((:file "variables")
               (:file "generic-functions")
               (:file "conditions")
               (:file "whole-parameters")
               (:file "condition-generation")
               (:file "required-parameters")
               (:file "optional-parameters")
               (:file "rest-parameters")
               (:file "key-parameters")
               (:file "aux-parameters")
               (:file "lambda-list")
               (:file "parse-macro")
               (:file "db-defmacro")

               (:file "condition-reporters-english")))
