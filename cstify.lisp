(cl:in-package #:concrete-syntax-tree)

(defmethod cstify ((list cl:null) &key source)
  (make-instance 'atom-cst :raw nil :source source))

(defmethod cstify ((list cl:cons) &key source)
  (let ((rest (cstify (cdr list))))
    (make-instance 'cons-cst
      :first (car list)
      :rest rest
      :source source
      :raw (cl:cons (raw (car list)) (raw rest)))))
