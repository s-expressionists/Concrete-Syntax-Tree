(cl:in-package #:concrete-syntax-tree)

(defmethod cstify ((list cl:null))
  (make-instance 'atom-cst :raw nil))

(defmethod cstify ((list cl:cons))
  (let ((rest (cstify (cdr list))))
    (make-instance 'cons-cst
      :first (car list)
      :rest (cstify (cdr list))
      :raw (cl:cons (raw (car list)) (raw rest)))))
