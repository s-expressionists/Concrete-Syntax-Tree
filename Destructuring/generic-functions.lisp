(cl:in-package #:concrete-syntax-tree)

(defgeneric destructure-lambda-list
    (client lambda-list argument-variable tail-variable body))

(defgeneric destructure-aux-parameter (client aux-parameter body))

(defgeneric destructure-aux-parameters (client parameters body))

(defgeneric destructure-parameter-group
    (client parameter-group argument-variable tail-variable body))
  
(defgeneric destructure-key-parameter
    (client key-parameter argument-variable body))

(defgeneric destructure-key-parameters
    (client parameters argument-variable body))

(defgeneric destructure-rest-parameter
    (client parameter argument-variable body))

(defgeneric destructure-optional-parameter
    (client optional-parameter argument-variable body))

(defgeneric destructure-optional-parameters
    (client parameters argument-variable tail-variable body))

(defgeneric destructure-required-parameter
    (client parameter argument-variable body))

(defgeneric destructure-required-parameters
    (client parameters argument-variable tail-variable body))

(defgeneric destructure-parameter-groups
    (client parameter-groups argument-variable tail-variable body))
