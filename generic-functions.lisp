(cl:in-package #:concrete-syntax-tree)

;;; Return the parent of CST.
(defgeneric parent (cst))

;;; Return true if and only if CST is an instance of NULL-CST.  Notice
;;; that this is not the same as a CST representing the atom NIL.
(defgeneric null (cst))

;;; Return true if and only if CST represents an atomic expression.
;;; Notice that (ATOM CST) is NOT equivalent to (NOT (CONSP CST))
;;; because there are CSTs that represent neither atoms nor conses.
(defgeneric atom (cst))

;;; Return true if and only if CST represents a CONS expression.
;;; Notice that (CONSP CST) is NOT equivalent to (NOT (ATOM CST))
;;; because there are CSTs that represent neither atoms nor conses.
(defgeneric consp (cst))
