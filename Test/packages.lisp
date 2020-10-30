(cl:defpackage #:concrete-syntax-tree-test
  (:use #:common-lisp)
  (:export
   #:run-tests))

(cl:in-package #:concrete-syntax-tree-test)

(defun run-tests ()
  (test-cst-from-expression)
  (test-reconstruct)
  (test-reconstruct-1))
