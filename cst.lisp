(cl:in-package #:concrete-syntax-tree)

(defclass cst ()
  (;; This slot contains either another CST, namely the parent of this
   ;; one, or NIL if this is a top-level CST.
   (%parent :initarg :parent :accessor parent)
   ;; This slot contains client-supplied information about the origin
   ;; of this CST.
   (%source :initform nil :initarg :source :accessor source)))

(defmethod null ((cst cst))
  (declare (ignorable cst))
  nil)

(defmethod atom ((cst cst))
  (declare (ignorable cst))
  nil)

(defmethod consp ((cst cst))
  (declare (ignorable cst))
  nil)

(defmethod raw (cst)
  (error 'expression-cst-required
         :cst cst))

(defmethod first (cst)
  (error 'cons-cst-required
         :cst cst))

(defmethod second (cst)
  (error 'cons-cst-required
         :cst cst))

;;; This class represents the atom NIL.
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
