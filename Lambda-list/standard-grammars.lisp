(cl:in-package #:concrete-syntax-tree)

(defparameter ordinary-required-parameters
  '((required-parameters <-
     (* ordinary-required-parameter))))

(defparameter ordinay-optional-parameters
  '((optional-parameters <-
     keyword-optional
     (* ordinary-optional-parameter))))

(defparameter rest-parameter
  '((rest-parameter <-
     keyword-rest
     variable)))

(defparameter key-parameters
  '((key-parameters <-
     keyword-key
     (* key-parameter)
     (? key-allow-other-keys))))

(defparameter aux-parameters
  '((aux-parameters <-
     keyword-aux
     (* aux-parameter))))

(defparameter ordinary-lambda-list
  '((lambda-list <-
     (? required-parameters)
     (? optional-parameters)
     (? rest-parameter)
     (? key-parameters)
     (? aux-parameters))))

(defparameter generic-function-optional-parameters
  '((optional-parameters <-
     keyword-optional
     (* generic-function-optional-parameter))))

(defparameter generic-function-lambda-list
  '((lambda-list <-
     (? required-parameters)
     (? optional-parameters)
     (? rest-parameter)
     (? key-parameters))))
