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
             (setf all-binds (append all-binds binds)
                   all-ignorables (append ignorables all-ignorables)))
        finally (return (values all-binds all-ignorables))))

(defmethod key-validation-bindings
    (client (parameter-group key-parameter-group) argument-variable)
  (let ((glength-check (gensym "LENGTH-CHECK-DUMMY"))
        (length-check-form
          `(unless (evenp (cl:length ,argument-variable))
             ,(odd-keywords-error client *current-lambda-list*
                                  argument-variable *current-macro-name*))))
    (if (allow-other-keys parameter-group)
        (values `((,glength-check ,length-check-form)) `(,glength-check))
        (let* ((unknowns (gensym "UNKNOWN-KEYWORDS"))
               (known-keywords
                 (loop for parameter in (parameters parameter-group)
                       collect (raw (keyword parameter))))
               (unknowns-form
                 `(loop with seen-allow-other-keys-p = nil
                        with allow-other-keys = nil
                        for (key value) on ,argument-variable by #'cl:cddr
                        ;; :allow-other-keys is always acceptable, so we have
                        ;; to be careful here to never include it in the
                        ;; unknowns list.
                        if (eq key :allow-other-keys)
                          do (unless seen-allow-other-keys-p
                               (setf allow-other-keys value
                                     seen-allow-other-keys-p t))
                        else unless (member key ',known-keywords :test #'eq)
                               collect key into unknowns
                        finally (unless allow-other-keys
                                  (return unknowns))))
               (unknown-check (gensym "UNKNOWN-KEYWORDS-DUMMY"))
               (unknown-check-form
                 `(unless (cl:null ,unknowns)
                    ,(unknown-keywords-error client *current-lambda-list*
                                             argument-variable unknowns
                                             *current-macro-name*))))
          (values `((,glength-check ,length-check-form)
                    (,unknowns ,unknowns-form)
                    (,unknown-check ,unknown-check-form))
                  `(,glength-check ,unknown-check))))))

(defmethod parameter-group-bindings
    (client (parameter-group key-parameter-group)
     argument-variable)
  (multiple-value-bind (validation-binds validation-ignorables)
      (key-validation-bindings client parameter-group argument-variable)
    (multiple-value-bind (main-binds main-ignorables)
        (key-parameters-bindings client (parameters parameter-group)
                                 argument-variable)
      (values (append validation-binds main-binds)
              (append validation-ignorables main-ignorables)))))
