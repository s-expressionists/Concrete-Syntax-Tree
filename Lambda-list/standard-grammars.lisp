(cl:in-package #:concrete-syntax-tree)

(defparameter *ordinary-required-parameter-group*
  '((ordinary-required-parameter-group <-
     (* simple-variable))))

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
     (? keyword-allow-other-keys))))

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
     (? keyword-allow-other-keys))))

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
     (? ordinary-optional-parameter-group)
     (? ordinary-rest-parameter-group)
     (? ordinary-key-parameter-group)
     (? aux-parameter-group))))

(defparameter *environment-parameter-group*
  '((environment-parameter-group <-
     keyword-environment
     simple-variable)))

(defparameter *defsetf-lambda-list*
  '((defsetf-lambda-list <-
     ordinary-required-parameter-group
     (? ordinary-optional-parameter-group)
     (? ordinary-rest-parameter-group)
     (? ordinary-key-parameter-group)
     (? environment-parameter-group))))

(defparameter *define-modify-macro-lambda-list*
  '((define-modify-macro-lambda-list <-
     ordinary-required-parameter-group
     (? ordinary-optional-parameter-group)
     (? ordinary-rest-parameter-group))))

(defparameter *ordinary-whole-parameter-group*
  '((ordinary-whole-parameter-group <-
     keyword-whole
     simple-variable)))

(defparameter *define-method-combination-lambda-list*
  '((define-method-combination-lambda-list <-
     (? ordinary-whole-parameter-group)
     ordinary-required-parameter-group
     (? ordinary-optional-parameter-group)
     (? ordinary-rest-parameter-group)
     (? ordinary-key-parameter-group)
     (? aux-parameter-group))))

(defparameter *destructuring-whole-parameter-group*
  '((destructuring-whole-parameter-group <-
     keyword-whole
     destructuring-parameter)))

(defparameter *destructuring-required-parameter-group*
  '((destructuring-required-parameter-group <-
     (* destructuring-parameter))))

(defparameter *destructuring-optional-parameter-group*
  '((destructuring-optional-parameter-group <-
     keyword-optional
     (* destructuring-optional-parameter))))

(defparameter *destructuring-key-parameter-group*
  '((destructuring-key-parameter-group <-
     keyword-key
     (* destructuring-key-parameter)
     (? keyword-allow-other-keys))))

(defparameter *destructuring-rest-parameter-group*
  '((destructuring-rest-parameter-group <-
     keyword-rest
     destructuring-parameter)
    (destructuring-rest-parameter-group <-
     keyword-body
     destructuring-parameter)))

(defparameter *destructuring-lambda-list*
  `((destructuring-lambda-list <-
     (? destructuring-whole-parameter-group)
     destructuring-required-parameter-group
     (? ordinary-optional-parameter-group)
     (? destructuring-rest-parameter-group)
     (? ordinary-key-parameter-group)
     (? aux-parameter-group))))

(defparameter *macro-lambda-list*
  `((macro-lambda-list <-
     (? destructuring-whole-parameter-group)
     (? environment-parameter-group)
     destructuring-required-parameter-group
     (? environment-parameter-group)
     (? destructuring-optional-parameter-group)
     (? environment-parameter-group)
     (? destructuring-rest-parameter-group)
     (? environment-parameter-group)
     (? destructuring-key-parameter-group)
     (? environment-parameter-group)
     (? aux-parameter-group)
     (? environment-parameter-group))))

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
	  *environment-parameter-group*
          *defsetf-lambda-list*
          *define-modify-macro-lambda-list*
	  *ordinary-whole-parameter-group*
          *define-method-combination-lambda-list*
          *destructuring-whole-parameter-group*
          *destructuring-required-parameter-group*
          *destructuring-optional-parameter-group*
          *destructuring-key-parameter-group*
          *destructuring-rest-parameter-group*
          *destructuring-lambda-list*
          *macro-lambda-list*))

(defparameter *ordinary-lambda-list-grammar*
  (generate-grammar 'ordinary-lambda-list *standard-grammar*))

(defparameter *generic-function-lambda-list-grammar*
  (generate-grammar 'generic-function-lambda-list *standard-grammar*))

(defparameter *specialized-lambda-list-grammar*
  (generate-grammar 'specialized-lambda-list *standard-grammar*))

(defparameter *defsetf-lambda-list-grammar*
  (generate-grammar 'defsetf-lambda-list *standard-grammar*))

(defparameter *define-modify-macro-lambda-list-grammar*
  (generate-grammar 'define-modify-macro-lambda-list *standard-grammar*))

(defparameter *define-method-combination-lambda-list-grammar*
  (generate-grammar 'define-method-combination-lambda-list *standard-grammar*))

(defparameter *destructuring-lambda-list-grammar*
  (generate-grammar 'destructuring-lambda-list *standard-grammar*))

(defparameter *macro-lambda-list-grammar*
  (generate-grammar 'macro-lambda-list *standard-grammar*))
