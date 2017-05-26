(cl:in-package #:concrete-syntax-tree)

(defparameter *ordinary-required-parameter-group*
  '((ordinary-required-parameter-group <-
     (* ordinary-required-parameter))))

(defparameter *ordinary-optional-parameter-group*
  '((ordinary-optional-parameter-group <-
     keyword-optional
     (* ordinary-optional-parameter))))

(defparameter *ordinary-rest-parameter-group*
  '((ordinary-rest-parameter-group <-
     keyword-rest
     simple-variable)))

(defparameter *ordinary-key-parameter-group*
  '((ordinary-key-parameter-group <-
     keyword-key
     (* ordinary-key-parameter)
     (? key-allow-other-keys))))

(defparameter *aux-parameter-group*
  '((aux-parameter-group <-
     keyword-aux
     (* aux-parameter))))

(defparameter *ordinary-lambda-list*
  '((ordinary-lambda-list <-
     ordinary-required-parameter-group
     (? ordinary-optional-parameter-group)
     (? ordinary-rest-parameter-group)
     (? ordinary-key-parameter-group)
     (? aux-parameter-group))))

(defparameter *generic-function-optional-parameter-group*
  '((generic-function-optional-parameter-group <-
     keyword-optional
     (* generic-function-optional-parameter))))

(defparameter *generic-function-key-parameter-group*
  '((generic-function-key-parameter-group <-
     keyword-key
     (* generic-function-key-parameter)
     (? key-allow-other-keys))))

(defparameter *generic-function-lambda-list*
  '((generic-function-lambda-list <-
     ordinary-required-parameter-group
     (? generic-function-optional-parameter-group)
     (? ordinary-rest-parameter-group)
     (? generic-function-key-parameter-group))))

(defparameter *specialized-required-parameter-group*
  '((specialized-required-parameter-group <-
     (* specialized-required-parameter))))

(defparameter *specialized-lambda-list*
  '((specialized-lambda-list <-
     specialized-required-parameter-group
     (? optional-parameters)
     (? ordinary-rest-parameter-group)
     (? key-parameters)
     (? aux-parameter-group))))

(defparameter *environment-parameter*
  '((environment-parameter <-
     keyword-environment
     simple-variable)))

(defparameter *whole-parameter*
  '((whole-parameter <-
     keyword-whole
     simple-variable)))

(defparameter *standard-grammar*
  (append *ordinary-required-parameter-group*
	  *ordinary-optional-parameter-group*
	  *ordinary-rest-parameter-group*
	  *ordinary-key-parameter-group*
	  *aux-parameter-group*
	  *ordinary-lambda-list*
	  *generic-function-optional-parameter-group*
	  *generic-function-key-parameter-group*
	  *generic-function-lambda-list*
	  *specialized-required-parameter-group*
	  *specialized-lambda-list*
	  *environment-parameter*
	  *whole-parameter*))

(defparameter *ordinary-lambda-list-grammar*
  (cl:cons '(target <- ordinary-lambda-list) *standard-grammar*))

(defparameter *generic-function-lambda-list-grammar*
  (cl:cons '(target <- generic-function-lambda-list) *standard-grammar*))

(defparameter *specialized-lambda-list-grammar*
  (cl:cons '(target <- specialized-lambda-list) *standard-grammar*))
