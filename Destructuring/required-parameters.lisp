(cl:in-package #:concrete-syntax-tree)

(defmethod destructure-required-parameter
    (client (parameter simple-variable) argument-variable body)
  `(if (cl:null ,argument-variable)
       (error "too few arguments")
       (let ((,(raw (name parameter)) (car ,argument-variable)))
         ,body)))

(defmethod destructure-required-parameter
    (client (parameter destructuring-lambda-list) argument-variable body)
  (let ((new-argument-variable)
        (tail-variable (gensym))
        (temp (gensym)))
  `(if (cl:null ,argument-variable)
       (error "too few arguments")
       (let ((,new-argument-variable (car ,argument-variable)))
         ,(destructure-lambda-list
           client
           parameter
           new-argument-variable
           tail-variable
           `(let ((,temp ,tail-variable))
              (declare (ignore ,temp))
              ,body))))))

(defmethod destructure-required-parameters
    (client (parameters cl:null) argument-variable tail-variable body)
  `(let ((,tail-variable ,argument-variable))
     ,body))

(defmethod destructure-required-parameters
    (client (parameters cl:cons) argument-variable tail-variable body)
  (let ((rest-variable (gensym)))
    (destructure-required-parameter
     client
     (car parameters)
     argument-variable
     `(let ((,rest-variable (if (cl:null ,argument-variable)
                                '()
                                (cdr ,argument-variable))))
        ,(destructure-required-parameters
          client
          (cdr parameters)
          rest-variable
          tail-variable
          body)))))

(defmethod destructure-parameter-group
    (client
     (parameter-group destructuring-required-parameter-group) 
     argument-variable
     tail-variable
     body)
  (destructure-required-parameters
   client
   (parameters parameter-group)
   argument-variable
   tail-variable
   body))
