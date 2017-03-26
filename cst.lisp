(cl:in-package #:concrete-syntax-tree)

(defgeneric parent (cst))

(defgeneric first (cst))

(defgeneric rest (cst))

(defclass cst ()
  (;; This slot contains either another CST, namely the parent of this
   ;; one, or, if this is a top-level CST, an indication of the source
   ;; document (a file or an editor buffer or something similar).
   (%parent :initarg :parent :accessor parent)))

(defclass expression-cst (cst)
  (;; If this CST represents an atom, then this slot contains NIL.
   ;; Otherwise, this slot contains a CST that represents the CAR of
   ;; the corresponding expression.
   (%first :initform nil :initarg :first :reader first)
   ;; If this CST represents an atom, then this slot contains NIL.
   ;; Otherwise, this slot contains a CST that represents the CDR of
   ;; the corresponding expression.
   (%rest :initform nil :initarg :rest :reader rest)))
