(cl:in-package #:concrete-syntax-tree-test)

(def-suite* :concrete-syntax-tree.reconstruct
  :in :concrete-syntax-tree)

(test reconstruct.smoke
  (let* ((expression (cons 'a 'c))
         (cst (cst:cst-from-expression expression))
         (result (cst:reconstruct nil (list 'b expression) cst)))
    (is (eq cst (cst:first (cst:rest result))))))

(test reconstruct.circular
  (let* ((circular (cst:cons (make-instance 'cst:atom-cst :raw nil)
                             (make-instance 'cst:atom-cst :raw nil)))
         (result (progn
                   (setf (slot-value circular 'cst::%first) circular)
                   (cst:reconstruct nil '#1=(#1#) circular))))
    (is-true (typep result 'cst:cst))
    (is (eq result (cst:first result)))))
