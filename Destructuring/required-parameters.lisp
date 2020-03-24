(cl:in-package #:concrete-syntax-tree)

(defmethod required-parameter-bindings
    (client (parameter simple-variable) argument-variable)
  (declare (ignore client))
  `((,(raw (name parameter))
     (if (cl:consp ,argument-variable)
         (car ,argument-variable)
         (error "too few arguments")))
    (,argument-variable (cl:cdr ,argument-variable))))

(defmethod required-parameter-bindings
    (client (parameter destructuring-lambda-list) argument-variable)
  (let ((new-argument-variable (gensym)))
    (multiple-value-bind (d-l-l-bindings d-l-l-ignorables)
        (destructuring-lambda-list-bindings
         client parameter new-argument-variable)
      (values
       `((,new-argument-variable
          (if (cl:consp ,argument-variable)
              (car ,argument-variable)
              (error "too few arguments")))
         ,@d-l-l-bindings
         (,argument-variable (cl:cdr ,argument-variable)))
       d-l-l-ignorables))))

(defmethod required-parameters-bindings
    (client (parameters cl:null) argument-variable)
  (declare (ignore client argument-variable))
  nil)

(defmethod required-parameters-bindings
    (client (parameters cl:cons) argument-variable)
  (loop with all-binds = nil with all-ignorables = nil
        for parameter in parameters
        do (multiple-value-bind (binds ignorables)
               (required-parameter-bindings client parameter argument-variable)
             (setf all-binds (append binds all-binds)
                   all-ignorables (append ignorables all-ignorables)))
        finally (return (values all-binds all-ignorables))))

(defmethod parameter-group-bindings
    (client (parameter-group destructuring-required-parameter-group)
     argument-variable)
  (required-parameters-bindings client (parameters parameter-group)
                                argument-variable))
