(defsystem "concrete-syntax-tree"
  :description "Library for parsing Common Lisp code into a concrete syntax tree."
  :license     "BSD" ; See LICENSE file for details
  :author      "Robert Strandh <robert.strandh@gmail.com>"
  :maintainer  "Jan Moringen <jan.moringen@posteo.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("acclimation")

  :components  ((:module     "base"
                 :pathname   "."
                 :serial     t
                 :components ((:file "packages")
                              (:file "generic-functions")
                              (:file "conditions")
                              (:file "utilities")
                              (:file "cst")
                              (:file "cons-cst")
                              (:file "listify")
                              (:file "cstify")
                              (:file "cst-from-expression")
                              (:file "quasiquotation")
                              (:file "reconstruct")
                              (:file "declarations")
                              (:file "body")
                              (:file "list-structure")
                              (:file "bindings")

                              (:file "condition-reporters-english"))))

  :in-order-to ((test-op (test-op "concrete-syntax-tree/test"))))

(defsystem "concrete-syntax-tree/test"
  :depends-on ("fiveam"
               "concrete-syntax-tree")
  :pathname   "Test"
  :serial     t
  :components ((:file "packages")
               (:file "random-expression")
               (:file "cst-from-expression")
               (:file "quasiquotation")
               (:file "random-sources")
               (:file "reconstruct"))
  :perform    (test-op (operation component)
                (when (and (not (uiop:symbol-call '#:concrete-syntax-tree-test
                                                  '#:run-tests))
                           (boundp 'cl-user::*result*))
                  (setf (symbol-value 'cl-user::*result*) nil))))
