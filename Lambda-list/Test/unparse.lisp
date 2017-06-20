(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defgeneric unparse (tree))

(defmethod unparse ((tree cst::lambda-list-type))
  (reduce #'append (mapcar #'unparse (cst::children tree))))

(defmethod unparse ((tree cst::parameter-group))
  (mapcar #'unparse (cst::parameters tree)))

(defmethod unparse ((tree cst::lambda-list-keyword))
  (cst::name tree))
