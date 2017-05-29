(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defun assert-success (parser)
  (let* ((initial-state (car (cst::all-states parser)))
         (final-state (car (cl:last (cst::all-states parser))))
         (item (find-if (lambda (item)
                          (and (eq (cst::left-hand-side (cst::rule item))
                                   'cst::target)
                               (eq (cst::origin item) initial-state)))
                        (cst::items final-state))))
    (assert (not (null item)))
    (car (cst::parse-trees item))))

(defun test1 ()
  (let* ((lambda-list '(a b))
         (p (make-instance 'cst::parser
              :rules cst::*ordinary-lambda-list-grammar*
              :input lambda-list
              :lambda-list (make-instance 'cst::lambda-list-type-ordinary)
              :client nil)))
    (cst::parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees result (parse-ordinary-lambda-list lambda-list)))))

(defun test2 ()
  (let* ((lambda-list '(a))
         (p (make-instance 'cst::parser
              :rules cst::*ordinary-lambda-list-grammar*
              :input lambda-list
              :lambda-list (make-instance 'cst::lambda-list-type-ordinary)
              :client nil)))
    (cst::parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees result (parse-ordinary-lambda-list lambda-list)))))

(defun test ()
  (assert (test1))
  (assert (test2)))
