(cl:in-package #:concrete-syntax-tree)

(defgeneric ensure-proper (lambda-list))

(defmethod ensure-proper ((lambda-list null-cst))
  lambda-list)

(defmethod ensure-proper ((lambda-list atom-cst))
  (if (null (source lambda-list))
      (make-instance 'null-cst)
      (list (make-instance 'atom-cst :raw '&rest)
            lambda-list)))

(defmethod ensure-proper ((lambda-list cons-cst))
  (let ((rest (ensure-proper (rest lambda-list))))
    (if (eq rest (rest lambda-list))
        lambda-list
        (make-instance 'cons-cst
          :source (source lambda-list)
          :raw (cl:cons (raw (first lambda-list)) (raw rest))
          :first (first lambda-list)
          :rest rest))))
