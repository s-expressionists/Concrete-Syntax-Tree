(cl:in-package #:concrete-syntax-tree-test)

(def-suite* :concrete-syntax-tree.reconstruct
  :in :concrete-syntax-tree)

(test reconstruct.smoke
  (let* ((expression-1 (cons 'a 'c))
         (expression-2 (list 'b expression-1))
         (cst (cst:cst-from-expression expression-1))
         (reconstructed (cst:reconstruct nil expression-2 cst)))
    (is (eq cst (cst:first (cst:rest reconstructed))))))

(test reconstruct.circular
  (let* ((circular (cst:cons (make-instance 'cst:atom-cst :raw nil)
                             (make-instance 'cst:atom-cst :raw nil)))
         (result (progn
                   (setf (slot-value circular 'cst::%first) circular)
                   (cst:reconstruct nil '#1=(#1#) circular))))
    (is-true (typep result 'cst:cst))
    (is (eq result (cst:first result)))))
