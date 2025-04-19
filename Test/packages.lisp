(cl:defpackage #:concrete-syntax-tree-test
  (:use
   #:common-lisp)

  (:import-from #:fiveam
   #:def-suite
   #:def-suite*
   #:test
   #:is
   #:is-true
   #:fail)

  (:export
   #:run-tests))

(cl:in-package #:concrete-syntax-tree-test)

(def-suite :concrete-syntax-tree)

(defun run-tests ()
  (fiveam:run! :concrete-syntax-tree))
