(cl:in-package #:concrete-syntax-tree)

(defmethod rest-parameter-bindings
    (client (parameter simple-variable) argument-variable &optional source)
  (values (cl:list (list (name parameter) argument-variable)) nil))

(defmethod rest-parameter-bindings
    (client (parameter destructuring-lambda-list)
     argument-variable &optional source)
  (destructuring-lambda-list-bindings client parameter
                                      argument-variable source))

(defmethod parameter-group-bindings
    (client (parameter-group destructuring-rest-parameter-group)
     argument-variable &optional source)
  (rest-parameter-bindings client (parameter parameter-group)
                           argument-variable source))
