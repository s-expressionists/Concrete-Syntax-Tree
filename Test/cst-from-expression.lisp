(cl:in-package #:concrete-syntax-tree-test)

(defun assert-equality (cst expression)
  (if (cst:null cst)
      (assert (null expression))
      (progn (assert (equalp (cst:raw cst) expression))
             (when (cst:consp cst)
               (assert (consp expression))
               (assert-equality (cst:first cst) (first expression))
               (assert-equality (cst:rest cst) (rest expression))))))

(defun test-cst-from-expression ()
  (loop repeat 1000000
        for expression = (random-expression)
        for cst = (cst:cst-from-expression expression)
        do (assert-equality cst expression)))

