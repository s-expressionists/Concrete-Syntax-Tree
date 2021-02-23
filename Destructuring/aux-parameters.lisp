(cl:in-package #:concrete-syntax-tree)

(defmethod aux-parameter-bindings
    (client (parameter aux-parameter) &optional source)
  (declare (ignore client))
  (values
   (cl:list (list (name parameter) (or (form parameter)
                                       (quasiquote source ()))))
   nil))

(defmethod aux-parameters-bindings
    (client (parameters cl:null) &optional source)
  (declare (ignore client source))
  (values nil nil))

(defmethod aux-parameters-bindings
    (client (parameters cl:cons) &optional source)
  (values
   (loop for parameter in parameters
         appending (aux-parameter-bindings client parameter source))))

(defmethod parameter-group-bindings
    (client (parameter-group aux-parameter-group)
     argument-variable &optional source)
  (declare (ignore argument-variable))
  (aux-parameters-bindings client (parameters parameter-group) source))
