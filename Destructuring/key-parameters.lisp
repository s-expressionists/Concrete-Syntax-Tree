(cl:in-package #:concrete-syntax-tree)

(defmethod key-parameter-bindings
    (client (parameter ordinary-key-parameter) argument-variable)
  (declare (ignore client))
  (let* ((name (raw (name parameter)))
         (keyword (raw (keyword parameter)))
         (default-form-cst (form parameter))
         (default-form (if (cl:null default-form-cst)
                           nil
                           (raw default-form-cst)))
         (suppliedp-cst (supplied-p parameter))
         (suppliedp-dummy (gensym "SUPPLIEDP"))
         (default-for-getf '(cl:list nil))
         (default-var (gensym "DEFAULT"))
         (search-var (gensym "GETF")))
    `((,default-var ,default-for-getf)
      (,search-var (getf ,argument-variable ',keyword ,default-var))
      (,suppliedp-dummy (not (eq ,search-var ,default-var)))
      (,name (if ,suppliedp-dummy ,search-var ,default-form))
      ;; we bind suppliedp after so that it's not bound during the
      ;; execution of the default form.
      ,@(unless (cl:null suppliedp-cst)
          `((,(raw suppliedp-cst) ,suppliedp-dummy))))))

(defmethod key-parameter-bindings
    (client (parameter destructuring-key-parameter) argument-variable)
  (let* ((tree (name parameter))
         (new-argument-variable (gensym))
         (keyword (raw (keyword parameter)))
         (default-form-cst (form parameter))
         (default-form (if (cl:null default-form-cst)
                           nil
                           (raw default-form-cst)))
         (suppliedp-cst (supplied-p parameter))
         (suppliedp-dummy (gensym "SUPPLIEDP"))
         (default-for-getf '(cl:list nil))
         (default-var (gensym "DEFAULT"))
         (search-var (gensym "GETF")))
    (multiple-value-bind (d-l-l-bindings d-l-l-ignorables)
        (destructuring-lambda-list-bindings client tree new-argument-variable)
      (values
       `((,default-var ,default-for-getf)
         (,search-var (getf ,argument-variable ',keyword ,default-var))
         (,suppliedp-dummy (not (eq ,search-var ,default-var)))
         (,new-argument-variable
          (if ,suppliedp-dummy ,search-var ,default-form))
         ,@d-l-l-bindings
         ;; we bind suppliedp after so that it's not bound during the
         ;; execution of the default form or any nested defaults.
         ,@(unless (cl:null suppliedp-cst)
             `((,(raw suppliedp-cst) ,suppliedp-dummy))))
       d-l-l-ignorables))))

(defmethod key-parameters-bindings
    (client (parameters cl:null) argument-variable)
  (declare (ignore client argument-variable))
  nil)

(defmethod key-parameters-bindings
    (client (parameters cl:cons) argument-variable)
  (loop with all-binds = nil with all-ignorables = nil
        for parameter in parameters
        do (multiple-value-bind (binds ignorables)
               (key-parameter-bindings client parameter argument-variable)
             (setf all-binds (append binds all-binds)
                   all-ignorables (append ignorables all-ignorables)))
        finally (return (values all-binds all-ignorables))))

(defmethod parameter-group-bindings
    (client (parameter-group key-parameter-group)
     argument-variable)
  (key-parameters-bindings client (parameters parameter-group)
                           argument-variable))
