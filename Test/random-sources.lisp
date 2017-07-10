(cl:in-package #:concrete-syntax-tree-test)

;;; Assign random unique symbols as source locations to every node in
;;; a concrete syntax tree.

(defgeneric random-sources (cst))

(defmethod random-sources ((cst cst:null-cst))
  cst)

(defmethod random-sources ((cst cst:atom-cst))
  (reinitialize-instance cst :source (gensym))
  cst)

(defmethod random-sources ((cst cst:cons-cst))
  (reinitialize-instance cst :source (gensym))
  (random-sources (cst:first cst))
  (random-sources (cst:rest cst))
  cst)
