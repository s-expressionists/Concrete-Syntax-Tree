(cl:in-package #:concrete-syntax-tree)

(defmethod destructure-optional-parameter
    (client (parameter ordinary-optional-parameter) argument-variable body)
  (declare (ignore client))
  (let* ((name (raw (name parameter)))
         (default-form-cst (form parameter))
         (default-form (if (cl:null default-form-cst)
                           nil
                           (raw default-form-cst)))
         (supplied-p-cst (supplied-p parameter)))
    `(let ((,name (if (cl:null ,argument-variable)
                      ,default-form
                      (car ,argument-variable)))
           ,@(if (cl:null supplied-p-cst)
                 '()
                 `((,(raw supplied-p-cst))
                   (not (cl:null ,argument-variable)))))
       ,body)))

(defmethod destructure-optional-parameters
    (client (parameters cl:null) argument-variable tail-variable body)
  (declare (ignore client))
  `(let ((,tail-variable ,argument-variable))
     ,body))

(defmethod destructure-optional-parameters
    (client (parameters cl:cons) argument-variable tail-variable body)
  (let ((rest-variable (gensym)))
    (destructure-optional-parameter
     client
     (car parameters)
     argument-variable
     `(let ((,rest-variable (if (cl:null ,argument-variable)
                                '()
                                (cdr ,argument-variable))))
        ,(destructure-optional-parameters client
                                          (cdr parameters)
                                          rest-variable
                                          tail-variable
                                          body)))))

(defmethod destructure-parameter-group
    (client
     (parameter-group ordinary-optional-parameter-group)
     argument-variable
     tail-variable
     body)
  (destructure-optional-parameters client
                                   (parameters parameter-group)
                                   argument-variable
                                   tail-variable
                                   body))
