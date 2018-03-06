(cl:in-package #:concrete-syntax-tree)

(defmethod destructure-parameter-groups
    (client (parameter-groups cl:null) argument-variable tail-variable body)
  `(let ((,tail-variable ,argument-variable))
     (declare (ignorable ,tail-variable))
     ,body))

(defmethod destructure-parameter-groups
    (client (parameter-groups cl:cons) argument-variable tail-variable body)
  (let ((rest-variable (gensym)))
    (destructure-parameter-group
     client
     (car parameter-groups)
     argument-variable
     rest-variable
     (destructure-parameter-groups
      client
      (cdr parameter-groups)
      rest-variable
      tail-variable
      body))))

(defmethod destructure-lambda-list
    (client
     (lambda-list macro-lambda-list)
     argument-variable
     tail-variable
     body)
  (destructure-parameter-groups
   client
   (children lambda-list)
   argument-variable
   tail-variable
   body))

(defmethod destructure-lambda-list
    (client
     (lambda-list destructuring-lambda-list)
     argument-variable
     tail-variable
     body)
  (destructure-parameter-groups
   client
   (children lambda-list)
   argument-variable
   tail-variable
   body))
