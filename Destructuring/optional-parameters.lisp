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

;;; FIXME: destructuring optional parameters go here

(defmethod optional-parameters-bindings
    (client (parameters cl:null) argument-variable)
  (declare (ignore client argument-variable))
  nil)

(defmethod optional-parameters-bindings
    (client (parameters cl:cons) argument-variable)
  (loop for parameter in parameters
        appending (optional-parameter-bindings client parameter
                                               argument-variable)))

(defmethod parameter-group-bindings
    (client (parameter-group ordinary-optional-parameter-group)
     argument-variable)
  (optional-parameters-bindings client (parameters parameter-group)
                                argument-variable))
