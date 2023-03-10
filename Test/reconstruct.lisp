(cl:in-package #:concrete-syntax-tree-test)

(defun test-reconstruct ()
  (let ((circular (cst:cons (make-instance 'cst:atom-cst :raw nil)
                            (make-instance 'cst:atom-cst :raw nil))))
    (setf (slot-value circular 'cst::%first) circular)
    (cst:reconstruct nil '#1=(#1#) circular)))

(defun test-reconstruct-1 ()
  (let* ((cons (cons 'a 'c))
         (thing (cst:cst-from-expression cons)))
    (assert (eq (cst:first (cst:rest (cst:reconstruct nil (list 'b cons) thing)))
                thing))))
