(cl:in-package #:concrete-syntax-tree)

(defclass cst ()
  (;; This slot contains either another CST, namely the parent of this
   ;; one, or NIL if this is a top-level CST.
   (%parent :initarg :parent :accessor parent)
   ;; This slot contains client-supplied information about the origin
   ;; of this CST.
   (%source :initform nil :initarg :source :accessor source)
   ;; This slot contains the raw expression that this CST represents.
   (%raw :initarg :raw :reader raw)))

(defmethod null ((cst cst))
  (declare (ignorable cst))
  nil)

(defmethod atom ((cst cst))
  (declare (ignorable cst))
  nil)

(defmethod consp ((cst cst))
  (declare (ignorable cst))
  nil)

(defmethod first (cst)
  (error 'cons-cst-required
         :cst cst))

(defmethod second (cst)
  (error 'cons-cst-required
         :cst cst))

;;; This class is used to represent expressions that are atoms.  It is
;;; not used to represent the end of a chain of CSTs.
(defclass atom-cst (cst)
  ())

(defmethod atom ((cst atom-cst))
  (declare (ignorable cst))
  t)

(defmethod null ((cst atom-cst))
  (cl:null (raw cst)))
