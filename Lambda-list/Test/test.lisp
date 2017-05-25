(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defun test1 ()
  (let ((p (make-instance 'cst::parser
             :rules cst::*ordinary-lambda-list-grammar*
             :input '(a b)
             :lambda-list (make-instance 'cst::lambda-list-type-ordinary)
             :client nil)))
    (cst::parse-step p)
    (cst::parse-step p)
    (cst::parse-step p)
    (let ((initial-state (car (cst::all-states p)))
          (final-state (car (cl:last (cst::all-states p)))))
      (list initial-state final-state))))
