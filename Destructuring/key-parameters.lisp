(cl:in-package #:concrete-syntax-tree)

(defmethod key-parameter-bindings
    (client (parameter ordinary-key-parameter)
     argument-variable &optional source)
  (declare (ignore client))
  (let* ((name (name parameter))
         (keyword (keyword parameter))
         (default-form-cst (or (form parameter) (quasiquote source ())))
         (suppliedp-cst (supplied-p parameter))
         (suppliedp-dummy
           (make-instance 'atom-cst :raw (gensym "SUPPLIEDP") :source source))
         (default-var
           (make-instance 'atom-cst :raw (gensym "DEFAULT") :source source))
         (search-var
           (make-instance 'atom-cst :raw (gensym "GETF") :source source)))
    (values
     (cl:list*
      (quasiquote source ((unquote default-var) (cl:list nil)))
      (quasiquote source ((unquote search-var)
                          (getf (unquote argument-variable) '(unquote keyword)
                                (unquote default-var))))
      (quasiquote source ((unquote suppliedp-dummy)
                          (not (eq (unquote search-var)
                                   (unquote default-var)))))
      (quasiquote source ((unquote name) (if (unquote suppliedp-dummy)
                                             (unquote search-var)
                                             (unquote default-form-cst))))
      ;; we bind suppliedp after so that it's not bound during the
      ;; execution of the default form.
      (unless (cl:null suppliedp-cst)
        (cl:list (list suppliedp-cst suppliedp-dummy))))
     nil)))

(defmethod key-parameter-bindings
    (client (parameter destructuring-key-parameter) argument-variable
     &optional source)
  (let* ((tree (name parameter))
         (new-argument-variable
           (make-instance 'atom-cst :raw (gensym) :source source))
         (keyword (keyword parameter))
         (default-form-cst (or (form parameter) (quasiquote source ())))
         (suppliedp-cst (supplied-p parameter))
         (suppliedp-dummy
           (make-instance 'atom-cst :raw (gensym "SUPPLIEDP") :source source))
         (default-var
           (make-instance 'atom-cst :raw (gensym "DEFAULT") :source source))
         (search-var
           (make-instance 'atom-cst :raw (gensym "GETF") :source source)))
    (multiple-value-bind (d-l-l-bindings d-l-l-ignorables)
        (destructuring-lambda-list-bindings client tree new-argument-variable)
      (values
       (cl:append
        (cl:list
         (quasiquote source ((unquote default-var) (cl:list nil)))
         (quasiquote source ((unquote search-var)
                             (getf (unquote argument-variable)
                                   '(unquote keyword)
                                   (unquote default-var))))
         (quasiquote source ((unquote suppliedp-dummy)
                             (not (eq (unquote search-var)
                                      (unquote default-var)))))
         (quasiquote source ((unquote new-argument-variable)
                             (if (unquote suppliedp-dummy)
                                 (unquote search-var)
                                 (unquote default-form)))))
        d-l-l-bindings
        ;; we bind suppliedp after so that it's not bound during the
        ;; execution of the default form or any nested defaults.
        (unless (cl:null suppliedp-cst)
          (cl:list (list suppliedp-cst suppliedp-dummy))))
       d-l-l-ignorables))))

(defmethod key-parameters-bindings
    (client (parameters cl:null) argument-variable &optional source)
  (declare (ignore client argument-variable source))
  (values nil nil))

(defmethod key-parameters-bindings
    (client (parameters cl:cons) argument-variable &optional source)
  (loop with all-binds = nil with all-ignorables = nil
        for parameter in parameters
        do (multiple-value-bind (binds ignorables)
               (key-parameter-bindings client parameter argument-variable)
             (setf all-binds (append all-binds binds)
                   all-ignorables (append ignorables all-ignorables)))
        finally (return (values all-binds all-ignorables))))

(defmethod parameter-group-bindings
    (client (parameter-group key-parameter-group)
     argument-variable &optional source)
  (key-parameters-bindings client (parameters parameter-group)
                           argument-variable source))
