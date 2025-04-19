(cl:in-package #:concrete-syntax-tree-test)

(def-suite* :concrete-syntax-tree.cst-from-expression
  :in :concrete-syntax-tree)

(defun assert-equality (cst-root expression-root)
  (loop with tail = (list (cons cst-root expression-root))
        for worklist = tail then (rest worklist)
        for (cst . expression) = (first worklist)
        while worklist
        if (cst:null cst)
          do (unless (null expression)
               (fail "~@<In CST ~A, expression ~A, sub-CST ~A is null but ~
                      sub-expression ~A is not null.~@:>"
                     cst-root expression-root cst expression))
        else
          do (unless (eql expression (cst:raw cst))
               (fail "~@<In CST ~A, expression ~A, sub-CST ~A has raw ~
                      ~S which is not equal to sub-expression ~S.~@:>"
                     cst-root expression-root cst (cst:raw cst) expression))
             (when (cst:consp cst)
               (unless (consp expression)
                 (fail "~@<In CST ~A, expression ~A, sub-CST ~A is a cons ~
                        but sub-expression ~A is not a cons.~@:>"
                       cst-root expression-root cst expression))
               (flet ((enqueue (item)
                        (let ((cell (list item)))
                          (setf (rest tail) cell
                                tail cell))))
                 (enqueue (cons (cst:first cst) (car expression)))
                 (enqueue (cons (cst:rest cst) (cdr expression)))))))

(test cst-from-expression.random
  (let ((fiveam:*test-dribble* nil)) ; too much output otherwise
    (loop repeat 1000000
          for expression = (random-expression)
          for cst = (cst:cst-from-expression expression)
          do (assert-equality cst expression))))

(test cst-from-expression.circular
  (let* ((expression '#1=(1 #1#))
         (cst (cst:cst-from-expression expression)))
    (is (eq expression (cst:raw cst)))
    (is (eq (first expression) (cst:raw (cst:first cst))))
    (is (eq cst (cst:second cst)))))
