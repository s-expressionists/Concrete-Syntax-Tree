(cl:in-package #:concrete-syntax-tree)

(defgeneric parent (cst))

(defgeneric first (cst))

(defgeneric rest (cst))

(defgeneric null (cst))

;;; Return true if and only if CST represents an atomic expression.
;;; Notice that (ATOM CST) is NOT equivalent to (NOT (CONSP CST))
;;; because there are CSTs that represent neither atoms nor conses.
(defgeneric atom (cst))

;;; Return true if and only if CST represents a CONS expression.
;;; Notice that (CONSP CST) is NOT equivalent to (NOT (ATOM CST))
;;; because there are CSTs that represent neither atoms nor conses.
(defgeneric consp (cst))

(defclass cst ()
  (;; This slot contains either another CST, namely the parent of this
   ;; one, or, if this is a top-level CST, an indication of the source
   ;; document (a file or an editor buffer or something similar).
   (%parent :initarg :parent :accessor parent)))

(defmethod null ((cst cst))
  (declare (ignorable cst))
  nil)

(defmethod atom ((cst cst))
  (declare (ignorable cst))
  nil)

(defmethod consp ((cst cst))
  (declare (ignorable cst))
  nil)

;;; This class is used as a terminator of a chain of CSTs.  It does
;;; NOT represent NIL in other situations.
(defclass null-cst (cst) ())

(defmethod null ((cst null-cst))
  (declare (ignorable cst))
  t)

(defclass expression-cst (cst)
  ())

;;; This class is used to represent expressions that are atoms.  It is
;;; not used to represent the end of a chain of CSTs.
(defclass atom-cst (expression-cst)
  ())

(defmethod atom ((cst atom-cst))
  (declare (ignorable cst))
  t)

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
