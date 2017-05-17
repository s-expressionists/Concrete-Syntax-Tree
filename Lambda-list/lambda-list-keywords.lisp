(cl:in-package #:concrete-syntax-tree)

(defgeneric all-lambda-list-keywords (client)
  (:method-combination append))

(defmethod all-lambda-list-keywords append (client)
  '(&optional
    &key
    &allow-other-keys
    &rest
    &body
    &aux
    &environment
    &whole))

#+sbcl
(defmethod all-lambda-list-keywords append ((client sbcl))
  '(sb-int:&more))
