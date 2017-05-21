(cl:in-package #:concrete-syntax-tree)

(defparameter ordinary-required-parameters
  '((ordinary-required-parameters <-
     (* ordinary-required-parameter))))

(defparameter ordinay-optional-parameters
  '((ordinary-optional-parameters <-
     keyword-optional
     (* ordinary-optional-parameter))))

(defparameter rest-parameter
  '((rest-parameter <-
     keyword-rest
     simple-variable)))

(defparameter ordinary-key-parameters
  '((ordinary-key-parameters <-
     keyword-key
     (* ordinary-key-parameter)
     (? key-allow-other-keys))))

(defparameter aux-parameters
  '((aux-parameters <-
     keyword-aux
     (* aux-parameter))))

(defparameter ordinary-lambda-list
  '((lambda-list <-
     (? ordinary-required-parameters)
     (? ordinary-optional-parameters)
     (? rest-parameter)
     (? ordinary-key-parameters)
     (? aux-parameters))))

(defparameter generic-function-optional-parameters
  '((generic-function-optional-parameters <-
     keyword-optional
     (* generic-function-optional-parameter))))

(defparameter generic-function-key-parameters
  '((generic-function-key-parameters <-
     keyword-key
     (* generic-function-key-parameter)
     (? key-allow-other-keys))))

(defparameter generic-function-lambda-list
  '((lambda-list <-
     (? ordinary-required-parameters)
     (? generic-function-optional-parameters)
     (? rest-parameter)
     (? generic-function-key-parameters))))

(defparameter specialized-required-parameters
  '((required-parameters <-
     (* specialized-required-parameter))))

(defparameter specialized-lambda-list
  '((lambda-list <-
     (? required-parameters)
     (? optional-parameters)
     (? rest-parameter)
     (? key-parameters)
     (? aux-parameters))))

(defparameter environment-parameter
  '((environment-parameter <-
     keyword-environment
     simple-variable)))

(defparameter whole-parameter
  '((whole-parameter <-
     keyword-whole
     simple-variable)))
