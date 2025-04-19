(cl:in-package #:concrete-syntax-tree-test)

(def-suite* :concrete-syntax-tree.cst-from-expression
  :in :concrete-syntax-tree)

(defun assert-equality (cst expression)
  (if (cst:null cst)
      (is-true (null expression))
      (progn
        (is (equalp expression (cst:raw cst)))
        (when (cst:consp cst)
          (is-true (consp expression))
          (assert-equality (cst:first cst) (first expression))
          (assert-equality (cst:rest cst) (rest expression))))))

(test cst-from-expression.random
  (let ((fiveam:*test-dribble* nil)) ; too much output otherwise
    (loop repeat 1000000
          for expression = (random-expression)
          for cst = (cst:cst-from-expression expression)
          do (assert-equality cst expression))))
