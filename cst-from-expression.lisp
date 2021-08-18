(cl:in-package #:concrete-syntax-tree)

(defvar *table*)

(defun cst-from-element (expression source)
  (cond ((cl:atom expression)
         (make-instance 'atom-cst
           :raw expression
           :source source))
        (t
         (cst-from-list-expression expression source))))

(defun cst-from-list-expression (expression source)
  (cond ((nth-value 1 (gethash expression *table*))
         (gethash expression *table*))
        ((cl:atom expression)
         (make-instance 'atom-cst
           :raw expression
           :source source))
        (t
         (let ((result (make-instance 'cons-cst
                         :raw expression :source source)))
           (setf (gethash expression *table*) result)
           (reinitialize-instance result
              :first (cst-from-element (car expression) source)
              :rest (cst-from-list-expression (cdr expression) source))))))

(defun cst-from-expression (expression &key source)
  (let ((*table* (make-hash-table :test #'eq)))
    (cst-from-element expression source)))
