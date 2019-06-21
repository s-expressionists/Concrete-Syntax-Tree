(cl:in-package #:concrete-syntax-tree)

(defmethod aux-parameter-bindings
    (client (parameter aux-parameter))
  (declare (ignore client))
  (let ((name (raw (name parameter)))
        (form (form parameter)))
    `((,name ,(if (cl:null form) cl:nil (raw form))))))

(defmethod aux-parameters-bindings
    (client (parameters cl:null))
  (declare (ignore client))
  nil)

(defmethod aux-parameters-bindings
    (client (parameters cl:cons))
  (loop for parameter in parameters
        appending (aux-parameter-bindings client parameter)))

(defmethod parameter-group-bindings
    (client (parameter-group aux-parameter-group)
     argument-variable)
  (declare (ignore argument-variable))
  (aux-parameters-bindings client (parameters parameter-group)))
