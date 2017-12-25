(cl:in-package #:concrete-syntax-tree)

(defvar *table*)

(defun cst-from-element (expression)
  (cond ((cl:atom expression)
         (make-instance 'atom-cst
           :raw expression))
        (t
         (cst-from-list-expression expression))))

(defun cst-from-list-expression (expression)
  (cond ((nth-value 1 (gethash expression *table*))
         (gethash expression *table*))
        ((cl:atom expression)
         (make-instance 'atom-cst
           :raw expression))
        (t
         (let ((result (make-instance 'cons-cst :raw expression)))
           (setf (gethash expression *table*) result)
           (reinitialize-instance result
              :first (cst-from-element (car expression))
              :rest (cst-from-list-expression (cdr expression)))))))

(defun cst-from-expression (expression)
  (let ((*table* (make-hash-table :test #'eq)))
    (cst-from-element expression)))
