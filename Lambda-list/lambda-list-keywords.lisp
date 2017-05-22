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

(defgeneric allowed-lambda-list-keywords (client lambda-list)
  (:method-combination append))

(defmethod allowed-lambda-list-keywords append
    (client (lambda-list lambda-list-type-ordinary))
  '(&optional
    &key
    &allow-other-keys
    &rest
    &body
    &aux))
