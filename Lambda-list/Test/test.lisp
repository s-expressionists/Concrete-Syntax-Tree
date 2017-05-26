(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defun assert-success (parser)
  (let ((initial-state (car (cst::all-states parser)))
        (final-state (car (cl:last (cst::all-states parser)))))
    (assert (find-if (lambda (item)
                       (and (eq (cst::left-hand-side (cst::rule item))
                                'cst::target)
                            (eq (cst::origin item) initial-state)))
                     (cst::items final-state)))))

(defun test1 ()
  (let ((p (make-instance 'cst::parser
             :rules cst::*ordinary-lambda-list-grammar*
             :input '(a b)
             :lambda-list (make-instance 'cst::lambda-list-type-ordinary)
             :client nil)))
    (cst::parse-step p)
    (cst::parse-step p)
    (cst::parse-step p)
    (assert-success p)))
