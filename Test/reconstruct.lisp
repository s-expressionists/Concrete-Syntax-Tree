(cl:in-package #:concrete-syntax-tree-test)

(defun test-reconstruct ()
  (let ((circular (cst:cons (make-instance 'cst:atom-cst :raw nil)
                            (make-instance 'cst:atom-cst :raw nil))))
    (setf (slot-value circular 'cst::%first) circular)
    (cst:reconstruct '#1=(#1#) circular nil)))
