(cl:defpackage #:concrete-syntax-tree-lambda-list-test
  (:use
   #:common-lisp)

  (:import-from #:fiveam
   #:def-suite
   #:in-suite
   #:test
   #:is
   #:is-true)

  (:export
   #:run-tests))

(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(def-suite :concrete-syntax-tree-lambda-list)

(defun run-tests ()
  (fiveam:run! :concrete-syntax-tree-lambda-list))
