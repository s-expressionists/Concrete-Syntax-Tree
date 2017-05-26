(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defgeneric compare-parse-trees (tree1 tree2))

(defmethod compare-parse-trees (tree1 tree2)
  (declare (ignore tree1 tree2))
  nil)

(defun compare-children (tree1 tree2)
  (and (eq (class-of tree1) (class-of tree2))
       (every #'compare-parse-trees
              (cst::children tree1)
              (cst::children tree2))))
   
(defmethod compare-parse-trees
    ((tree1 cst::lambda-list-type) (tree2 cst::lambda-list-type))
  (compare-children tree1 tree2))

(defmethod compare-parse-trees
    ((tree1 cst::parameter-group) (tree2 cst::parameter-group))
  (compare-children tree1 tree2))

(defmethod compare-parse-trees
    ((tree1 cst::ordinary-required-parameter)
     (tree2 cst::ordinary-required-parameter))
  (eq (cst::name tree1) (cst::name tree2)))

(defmethod compare-parse-trees
    ((tree1 cst::ordinary-optional-parameter)
     (tree2 cst::ordinary-optional-parameter))
  (and (eq (cst::name tree1) (cst::name tree2))
       (equal (cst::form tree1) (cst::form tree2))
       (or (and (null (symbol-package (cst::supplied-p tree1)))
                (null (symbol-package (cst::supplied-p tree2))))
           (eq (cst::supplied-p tree1) (cst::supplied-p tree2)))))



