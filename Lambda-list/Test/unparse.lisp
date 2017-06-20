(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defgeneric unparse (tree))

(defmethod unparse ((tree cst::lambda-list-type))
  (reduce #'append (mapcar #'unparse (cst::children tree))))

(defmethod unparse ((tree cst::parameter-group))
  (mapcar #'unparse (cst::parameters tree)))

(defmethod unparse ((tree cst::lambda-list-keyword))
  (cst::name tree))

(defmethod unparse ((tree cst::simple-variable))
  (cst::name tree))

(defmethod unparse ((tree cst::ordinary-optional-parameter))
  (list (cst::name tree)
        (cst::form tree)
        (cst::supplied-p tree)))

(defmethod unparse ((tree cst::ordinary-key-parameter))
  (list (list (cst::keyword tree) (cst::name tree))
        (cst::form tree)
        (cst::supplied-p tree)))

(defmethod unparse ((tree cst::aux-parameter))
  (list (cst::name tree)
        (cst::form tree)))

(defmethod unparse ((tree cst::generic-function-optional-parameter))
  (cst::name tree))

(defmethod unparse ((tree cst::generic-function-key-parameter))
  (list (list (cst::keyword tree) (cst::name tree))))

(defmethod unparse ((tree cst::specialized-required-parameter))
  (list (cst::name tree) (cst::specializer tree)))
