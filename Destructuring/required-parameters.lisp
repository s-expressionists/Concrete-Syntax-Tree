(cl:in-package #:concrete-syntax-tree)

(defmethod required-parameter-bindings
    (client (parameter simple-variable) argument-variable &optional source)
  (declare (ignore client))
  (values
   (cl:list
    (quasiquote
     source
     ((unquote (name parameter))
      (if (cl:consp (unquote argument-variable))
          (car (unquote argument-variable))
          (error "too few arguments"))))
    (list argument-variable
          (quasiquote source (cl:cdr (unquote argument-variable)))))
   nil))

(defmethod required-parameter-bindings
    (client (parameter destructuring-lambda-list)
     argument-variable &optional source)
  (let ((new-argument-variable
          (make-instance 'atom-cst :raw (gensym) :source source)))
    (multiple-value-bind (d-l-l-bindings d-l-l-ignorables)
        (destructuring-lambda-list-bindings
         client parameter new-argument-variable source)
      (values
       (append
        (cl:list
         (quasiquote source
                     ((unquote new-argument-variable)
                      (if (cl:consp (unquote argument-variable))
                          (car (unquote argument-variable))
                          (error "too few arguments")))))
        d-l-l-bindings
        (cl:list
         (list argument-variable
               (quasiquote source (cl:cdr (unquote argument-variable))))))
       d-l-l-ignorables))))

(defmethod required-parameters-bindings
    (client (parameters cl:null) argument-variable &optional source)
  (declare (ignore client argument-variable))
  (values nil nil))

(defmethod required-parameters-bindings
    (client (parameters cl:cons) argument-variable &optional source)
  (loop with all-binds = nil
        with all-ignorables = nil
        for parameter in parameters
        do (multiple-value-bind (binds ignorables)
               (required-parameter-bindings client parameter
                                            argument-variable source)
             (setf all-binds (append all-binds binds)
                   all-ignorables (append ignorables all-ignorables)))
        finally (return (values all-binds all-ignorables))))

(defmethod parameter-group-bindings
    (client (parameter-group destructuring-required-parameter-group)
     argument-variable &optional source)
  (required-parameters-bindings client (parameters parameter-group)
                                argument-variable source))
