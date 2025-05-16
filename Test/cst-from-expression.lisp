(cl:in-package #:concrete-syntax-tree-test)

(def-suite* :concrete-syntax-tree.cst-from-expression
  :in :concrete-syntax-tree)

(test cst-from-expression.atoms-without-identity
  "Test that `cst-from-expression' always creates fresh CSTs for atoms
that behave like values without identity."
  (flet ((test-atoms (atom)
           (let* ((expression `(,atom ,atom))
                  (cst        (cst:cst-from-expression expression)))
             (is (not (eq (cst:first cst) (cst:second cst)))))))
    (test-atoms 1)
    (test-atoms #\c)
    ;; Symbols are different from numbers and characters, of course,
    ;; but distinct occurrences of a given symbol either in code or in
    ;; an s-expression are not typically treated as the same syntactic
    ;; object.
    (test-atoms :fo)))

(defstruct atom-test-struct)

(defclass atom-test-class () ())

(test cst-from-expression.atoms-with-identity
  "Test that `cst-from-expression' creates fresh CSTs or uses shared CSTs
for atoms that have meaningful identities."
  (flet ((test-atoms (atom1 atom2)
           (let* ((expression `(,atom1 ,atom2 ,atom1))
                  (cst        (cst:cst-from-expression expression)))
             (is (not (eq (cst:first cst) (cst:second cst))))
             (is      (eq (cst:first cst) (cst:third cst))))))
    (test-atoms #P"foo" #P"bar")
    (test-atoms (make-atom-test-struct) (make-atom-test-struct))
    (test-atoms (make-instance 'atom-test-class)
                (make-instance 'atom-test-class))))

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

(test cst-from-expression.long-list
  (let* ((expression (make-long-list))
         (cst (cst:cst-from-expression expression)))
    (assert-equality cst expression)))
