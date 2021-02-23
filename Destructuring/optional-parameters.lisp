(cl:in-package #:concrete-syntax-tree)

(defmethod optional-parameter-bindings
    (client (parameter ordinary-optional-parameter)
     argument-variable &optional source)
  (declare (ignore client))
  (let* ((name (name parameter))
         (default-form-cst (or (form parameter) (quasiquote source ())))
         (suppliedp-cst (supplied-p parameter))
         ;; the suppliedp is not bound for the default form, so we do this.
         (suppliedp-dummy (make-instance 'atom-cst
                            :raw (gensym "SUPPLIEDP") :source source)))
    (values
     (append
      (cl:list
       (quasiquote
        source
        ((unquote suppliedp-dummy) (cl:consp (unquote argument-variable))))
       (quasiquote
        source
        ((unquote name) (if (unquote suppliedp-dummy)
                            (cl:car (unquote argument-variable))
                            (unquote default-form-cst)))))
      (unless (cl:null suppliedp-cst)
        (cl:list (list suppliedp-cst suppliedp-dummy)))
      (cl:list
       (quasiquote
        source
        ((unquote argument-variable)
         (if (unquote suppliedp-dummy)
             (cl:cdr (unquote argument-variable))
             (unquote argument-variable))))))
     nil)))

(defmethod optional-parameter-bindings
    (client (parameter destructuring-optional-parameter)
     argument-variable &optional source)
  (let* ((tree (name parameter))
         (new-argument-variable
           (make-instance 'atom-cst :raw (gensym) :source source))
         (default-form-cst (or (form parameter) (quasiquote source ())))
         (suppliedp-cst (supplied-p parameter))
         ;; the suppliedp is not bound for the default form, so we do this.
         (suppliedp-dummy
           (make-instance 'atom-cst :raw (gensym "SUPPLIEDP") :source source)))
    (multiple-value-bind (d-l-l-bindings d-l-l-ignorables)
        (destructuring-lambda-list-bindings client tree
                                            new-argument-variable source)
      (values
       (append
        (cl:list
         (quasiquote
          source
          ((unquote suppliedp-dummy) (cl:consp (unquote argument-variable))))
         (quasiquote
          source
          ((unquote new-argument-variable)
           (if (unquote suppliedp-dummy)
               (cl:car (unquote argument-variable))
               (unquote default-form-cst)))))
        d-l-l-bindings
        (unless (cl:null suppliedp-cst)
          (cl:list (list suppliedp-cst suppliedp-dummy)))
        (cl:list
         (quasiquote
          source
          ((unquote argument-variable) (if (unquote suppliedp-dummy)
                                           (cl:cdr (unquote argument-variable))
                                           (unquote argument-variable))))))
       d-l-l-ignorables))))

(defmethod optional-parameters-bindings
    (client (parameters cl:null) argument-variable &optional source)
  (declare (ignore client argument-variable source))
  (values nil nil))

(defmethod optional-parameters-bindings
    (client (parameters cl:cons) argument-variable &optional source)
  (loop with all-binds = nil with all-ignorables = nil
        for parameter in parameters
        do (multiple-value-bind (binds ignorables)
               (optional-parameter-bindings client parameter argument-variable)
             (setf all-binds (append all-binds binds)
                   all-ignorables (append ignorables all-ignorables)))
        finally (return (values all-binds all-ignorables))))

(defmethod parameter-group-bindings
    (client (parameter-group optional-parameter-group)
     argument-variable &optional source)
  (optional-parameters-bindings client (parameters parameter-group)
                                argument-variable source))
