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
    (client (lambda-list ordinary-lambda-list))
  '(&optional
    &key
    &allow-other-keys
    &rest
    &aux))

(defmethod allowed-lambda-list-keywords append
    (client (lambda-list generic-function-lambda-list))
  '(&optional
    &key
    &allow-other-keys
    &rest))

(defmethod allowed-lambda-list-keywords append
    (client (lambda-list specialized-lambda-list))
  '(&optional
    &key
    &allow-other-keys
    &rest
    &aux))

(defmethod allowed-lambda-list-keywords append
    (client (lambda-list defsetf-lambda-list))
  '(&optional
    &key
    &allow-other-keys
    &rest
    &environment))

(defmethod allowed-lambda-list-keywords append
    (client (lambda-list define-modify-macro-lambda-list))
  '(&optional
    &rest))

(defmethod allowed-lambda-list-keywords append
    (client (lambda-list define-method-combination-lambda-list))
  '(&optional
    &rest))
