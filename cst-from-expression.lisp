(cl:in-package #:concrete-syntax-tree)

(defvar *table*)

(defun cst-from-element (expression)
  (let ((existing (gethash expression *table*)))
    (cond ((not (cl:null existing))
           existing)
          ((cl:atom expression)
           (setf (gethash expression *table*)
                 (make-instance 'atom-cst
                   :raw expression)))
          (t
           (cst-from-list-expression expression)))))

(defun cst-from-list-expression (expression)
  (let ((existing (gethash expression *table*)))
    (cond ((not (cl:null existing))
           existing)
          ((cl:null expression)
           (make-instance 'null-cst))
          ((cl:atom expression)
           (setf (gethash expression *table*)
                 (make-instance 'atom-cst
                   :raw expression)))
          (t
           (let ((new (make-instance 'cons-cst
                        :raw expression)))
             (setf (gethash expression *table*) new)
             (reinitialize-instance
              new
              :raw expression
              :first (cst-from-element (car expression))
              :rest (cst-from-list-expression (cdr expression)))
             new)))))

(defun cst-from-expression (expression)
  (let ((*table* (make-hash-table :test #'eq)))
    (cst-from-element expression)))
