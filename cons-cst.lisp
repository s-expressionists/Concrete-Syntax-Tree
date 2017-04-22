(cl:in-package #:concrete-syntax-tree)

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

(defgeneric cons (first rest))

(defun raw-or-nil (cst)
  (if (typep cst 'null-cst)
      nil
      (raw cst)))

(defmethod cons (first rest)
  (make-instance 'cons-cst
    :raw (cl:cons (raw-or-nil first) (raw-or-nil rest))
    :first first
    :rest rest))

(defun list (&rest csts)
  (loop for result = (make-instance 'null-cst) then (cons cst result)
        for cst in (reverse csts)
        finally (return result)))
