(cl:in-package #:concrete-syntax-tree)

(defmethod optional-parameter-bindings
    (client (parameter ordinary-optional-parameter) argument-variable)
  (declare (ignore client))
  (let* ((name (raw (name parameter)))
         (default-form-cst (form parameter))
         (default-form (if (cl:null default-form-cst)
                           nil
                           (raw default-form-cst)))
         (suppliedp-cst (supplied-p parameter))
         ;; the suppliedp is not bound for the default form, so we do this.
         (suppliedp-dummy (gensym "SUPPLIEDP")))
    `((,suppliedp-dummy (cl:consp ,argument-variable))
      (,name (if ,suppliedp-dummy (cl:car ,argument-variable) ,default-form))
      ,@(unless (cl:null suppliedp-cst)
          `((,(raw suppliedp-cst) ,suppliedp-dummy)))
      (,argument-variable (if ,suppliedp-dummy
                              (cl:cdr ,argument-variable)
                              ,argument-variable)))))

(defmethod optional-parameter-bindings
    (client (parameter destructuring-optional-parameter) argument-variable)
  (let* ((tree (name parameter))
         (new-argument-variable (gensym))
         (default-form-cst (form parameter))
         (default-form (if (cl:null default-form-cst)
                           nil
                           (raw default-form-cst)))
         (suppliedp-cst (supplied-p parameter))
         ;; the suppliedp is not bound for the default form, so we do this.
         (suppliedp-dummy (gensym "SUPPLIEDP")))
    (multiple-value-bind (d-l-l-bindings d-l-l-ignorables)
        (destructuring-lambda-list-bindings client tree new-argument-variable)
      (values
       `((,suppliedp-dummy (cl:consp ,argument-variable))
         (,new-argument-variable
          (if ,suppliedp-dummy (cl:car ,argument-variable) ,default-form))
         ,@d-l-l-bindings
         ,@(unless (cl:null suppliedp-cst)
             `((,(raw suppliedp-cst) ,suppliedp-dummy)))
         (,argument-variable (if ,suppliedp-dummy
                                 (cl:cdr ,argument-variable)
                                 ,argument-variable)))
       d-l-l-ignorables))))

(defmethod optional-parameters-bindings
    (client (parameters cl:null) argument-variable)
  (declare (ignore client argument-variable))
  nil)

(defmethod optional-parameters-bindings
    (client (parameters cl:cons) argument-variable)
  (loop with all-binds = nil with all-ignorables = nil
        for parameter in parameters
        do (multiple-value-bind (binds ignorables)
               (optional-parameter-bindings client parameter argument-variable)
             (setf all-binds (append binds all-binds)
                   all-ignorables (append ignorables all-ignorables)))
        finally (return (values all-binds all-ignorables))))

(defmethod parameter-group-bindings
    (client (parameter-group optional-parameter-group)
     argument-variable)
  (optional-parameters-bindings client (parameters parameter-group)
                                argument-variable))
