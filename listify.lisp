(cl:in-package #:concrete-syntax-tree)

(defmethod listify ((cst atom-cst))
  (raw cst))

(defmethod listify ((cst cons-cst))
  (cl:cons (first cst) (listify (rest cst))))
