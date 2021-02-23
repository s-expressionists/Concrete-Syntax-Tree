(cl:in-package #:concrete-syntax-tree)

(defmethod parameter-groups-bindings
    (client (parameter-groups cl:null) argument-variable &optional source)
  (declare (ignore client))
  (values (cl:list
           (quasiquote source
                       ((unquote argument-variable)
                        (if (cl:null (unquote argument-variable))
                            (unquote argument-variable)
                            (error "too many arguments")))))
          (cl:list argument-variable)))

(defmethod parameter-groups-bindings
    (client (parameter-groups cl:cons) argument-variable &optional source)
  (loop with all-binds = nil
        with all-ignorables = (cl:list argument-variable)
        with too-many-args-bindings
          = (if (some (lambda (pg)
                        (parameter-group-varargs-p client pg))
                      parameter-groups)
                nil
                (cl:list
                 (quasiquote
                  source
                  ((unquote argument-variable)
                   (if (cl:null (unquote argument-variable))
                       (unquote argument-variable)
                       (error "too many arguments"))))))
        for parameter-group in parameter-groups
        do (multiple-value-bind (binds ignorables)
               (parameter-group-bindings client parameter-group
                                         argument-variable source)
             (setf all-binds (append all-binds binds)
                   all-ignorables (append ignorables all-ignorables)))
        finally (return
                  (values (append all-binds too-many-args-bindings)
                          all-ignorables))))

(defmethod destructuring-lambda-list-bindings
    (client (lambda-list macro-lambda-list) argument-variable &optional source)
  (parameter-groups-bindings client (children lambda-list)
                             argument-variable source))

(defmethod destructuring-lambda-list-bindings
    (client (lambda-list destructuring-lambda-list) argument-variable
     &optional source)
  (parameter-groups-bindings client (children lambda-list)
                             argument-variable source))
