(cl:in-package #:concrete-syntax-tree)

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

;;; An instance of this class is used to represent a Common Lisp
;;; expression.
(defclass expression-cst (cst)
  ((%raw :initarg :raw :reader raw)))

;;; This class is used to represent expressions that are atoms.  It is
;;; not used to represent the end of a chain of CSTs.
(defclass atom-cst (expression-cst)
  ())

(defmethod atom ((cst atom-cst))
  (declare (ignorable cst))
  t)
