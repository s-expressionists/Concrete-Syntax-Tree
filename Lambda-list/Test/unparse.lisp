(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defgeneric unparse (tree))

(defmethod unparse ((tree cst::ordinary-lambda-list))
  (reduce #'append (mapcar #'unparse (cst::children tree))))
