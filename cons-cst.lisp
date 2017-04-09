(cl:in-package #:concrete-syntax-tree)

(defgeneric first (cst))

(defgeneric rest (cst))

(defclass cons-cst (expression-cst)
  (;; This slot contains a CST that represents the CAR of the
   ;; corresponding expression.
   (%first :initform nil :initarg :first :reader first)
   ;; This slot contains a CST that represents the CDR of the
   ;; corresponding expression.
   (%rest :initform nil :initarg :rest :reader rest)))

(defmethod consp ((cst cons-cst))
  (declare (ignorable cst))
  t)
