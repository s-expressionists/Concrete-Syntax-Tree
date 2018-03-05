(cl:in-package #:concrete-syntax-tree)

(defmethod destructure-key-parameter
    (client (parameter ordinary-key-parameter) argument-variable body)
  (declare (ignore client))
  (let* ((name (raw (name parameter)))
         (keyword (raw (keyword parameter)))
         (default-form-cst (form parameter))
         (default-form (if (cl:null default-form-cst)
                           nil
                           (raw default-form-cst)))
         (supplied-p-cst (supplied-p parameter))
         (default-for-getf '(cl:list nil))
         (default-var (gensym))
         (form-variable (gensym)))
    `(let* ((,default-var ,default-for-getf)
            (,form-variable (getf ,argument-variable ,keyword ,default-var)))
       (let ((,name (if (eq ,form-variable ,default-var)
                        ,default-form
                        ,form-variable))
             ,@(if (cl:null supplied-p-cst)
                   '()
                   `((,(raw supplied-p-cst)
                      (not (eq ,form-variable ,default-var))))))
         ,body))))

(defmethod destructure-key-parameters
    (client (parameters cl:null) argument-variable body)
  (declare (ignore client))
  body)

(defmethod destructure-key-parameters
    (client (parameters cl:cons) argument-variable body)
  (destructure-key-parameter
   client
   (car parameters)
   argument-variable
   (destructure-key-parameters client (cdr parameters) argument-variable body)))

(defmethod destructure-parameter-group
    (client
     (parameter-group ordinary-key-parameter-group)
     argument-variable
     tail-variable
     body)
  (destructure-key-parameters
   client
   (parameters parameter-group)
   argument-variable
   `(let ((,tail-variable ,argument-variable))
      ,body)))
