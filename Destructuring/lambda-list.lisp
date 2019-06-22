(cl:in-package #:concrete-syntax-tree)

(defmethod parameter-groups-bindings
    (client (parameter-groups cl:null) argument-variable)
  (declare (ignore client argument-variable))
  nil)

(defmethod parameter-groups-bindings
    (client (parameter-groups cl:cons) argument-variable)
  (loop for parameter-group in parameter-groups
        appending (parameter-group-bindings client parameter-group
                                            argument-variable)))

(defmethod destructuring-lambda-list-bindings
    (client (lambda-list macro-lambda-list) argument-variable)
  (parameter-groups-bindings client (children lambda-list)
                             argument-variable))

(defmethod destructuring-lambda-list-bindings
    (client (lambda-list destructuring-lambda-list) argument-variable)
  (parameter-groups-bindings client (children lambda-list)
                             argument-variable))
