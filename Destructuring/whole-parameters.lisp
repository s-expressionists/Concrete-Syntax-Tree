(cl:in-package #:concrete-syntax-tree)

(defmethod whole-parameter-bindings
    (client (parameter simple-variable) argument-variable)
  (declare (ignore client))
  `((,(raw (name parameter)) ,argument-variable)))

(defmethod whole-parameter-bindings
    (client (parameter destructuring-lambda-list) argument-variable)
  (destructuring-lambda-list-bindings client parameter argument-variable))

(defmethod parameter-group-bindings
    (client (parameter-group whole-parameter-group)
     argument-variable)
  (whole-parameter-bindings client (parameter parameter-group)
                            argument-variable))
