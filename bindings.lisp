(cl:in-package #:concrete-syntax-tree)

;;;; This code provides useful operations on bindings as used by LET
;;;; and LET*.  The bindings are in the form of a CST.  Recall that
;;;; such a binding can have three different shapes, namely VAR,
;;;; (VAR), or (VAR INIT-FORM).  The last of these three shapes is
;;;; considered canonical.

;;; Check whether a binding is valid, i.e., has one of the three valid
;;; shapes.
(defun valid-binding-p (binding-cst)
  (let ((raw (raw binding-cst)))
    (or (symbolp raw)
        (and (cl:consp raw)
             (or (cl:null (cdr raw))
                 (and (cl:consp (cdr raw))
                      (cl:null (cddr raw))))))))
