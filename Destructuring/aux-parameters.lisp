(cl:in-package #:concrete-syntax-tree)

(defmethod destructure-aux-parameter
    (client (parameter aux-parameter) body)
  (declare (ignore client))
  (let ((name (name parameter))
        (form (form parameter)))
    `(let ((,(raw name) ,(if (cl:null form) cl:nil (raw form))))
       ,body)))

(defmethod destructure-aux-parameters
    (client (parameters cl:null) body)
  (declare (ignore client))
  body)

(defmethod destructure-aux-parameters
    (client (parameters cl:cons) body)
  (destructure-aux-parameter
   client
   (car parameters)
   (destructure-aux-parameters client (cdr parameters) body)))
                                   
(defmethod destructure-parameter-group
    (client
     (parameter-group aux-parameter-group)
     argument-variable
     tail-variable
     body)
  (destructure-aux-parameters
   client
   (parameters parameter-group)
   `(let ((,tail-variable ,argument-variable))
      ,body)))
