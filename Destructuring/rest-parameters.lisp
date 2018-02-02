(cl:in-package #:concrete-syntax-tree)

(defmethod destructure-rest-parameter
    (client (parameter simple-variable) argument-variable body)
  `(let ((,(raw (name parameter)) ,argument-variable))
     ,body))

(defmethod destructure-rest-parameter
    (client (parameter destructuring-lambda-list) argument-variable body)
  (let ((tail-variable (gensym))
        (temp (gensym)))
  (destructure-lambda-list
   client
   parameter
   argument-variable
   tail-variable
   `(let ((,temp ,tail-variable))
      (declare (ignore ,temp))
      ,body))))

(defmethod destructure-parameter-group
    (client
     (parameter-group destructuring-rest-parameter-group)
     argument-variable
     tail-variable
     body)
  (destructure-rest-parameter
   client
   (parameter parameter-group)
   argument-variable
   `(let ((,tail-variable ,argument-variable))
      ,body)))
