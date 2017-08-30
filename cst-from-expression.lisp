(cl:in-package #:concrete-syntax-tree)

(defun cst-from-element (expression)
  (cond ((cl:atom expression)
         (make-instance 'atom-cst
           :raw expression))
        (t
         (cst-from-list-expression expression))))

(defun cst-from-list-expression (expression)
  (cond ((cl:atom expression)
         (make-instance 'atom-cst
           :raw expression))
        (t
         (make-instance 'cons-cst
           :raw expression
           :first (cst-from-element (car expression))
           :rest (cst-from-list-expression (cdr expression))))))

(defun cst-from-expression (expression)
  (cst-from-element expression))
