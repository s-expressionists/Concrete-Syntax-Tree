(defsystem "concrete-syntax-tree-lambda-list"
  :depends-on  ("concrete-syntax-tree")
  :serial      t
  :components  ((:file "client")
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
                (:file "unparse"))
  :in-order-to ((test-op (test-op "concrete-syntax-tree-lambda-list/test"))))

(defsystem "concrete-syntax-tree-lambda-list/test"
  :depends-on ("fiveam"
               "concrete-syntax-tree-lambda-list")
  :components ((:module     "Test"
                :serial     t
                :components ((:file "packages")
                             (:file "random-lambda-list")
                             (:file "compare-parse-trees")
                             (:file "parsers")
                             (:file "unparse")
                             (:file "test"))))
  :perform    (test-op (operation component)
                (uiop:symbol-call '#:concrete-syntax-tree-lambda-list-test '#:run-tests)))
