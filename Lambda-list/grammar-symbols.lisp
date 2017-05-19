(cl:in-package #:concrete-syntax-tree)

(defclass grammar-symbol ()
  ((%parse-tree :initarg :parse-tree :reader parse-tree)))

(defclass parameter-group (grammar-symbol) ())

(defclass ordinary-required-parameters (parameter-group) ())

(defclass ordinary-optional-parameters (parameter-group) ())

(defclass rest-parameter (parameter-group) ())

(defclass key-parameters (parameter-group) ())

(defclass aux-parameters (parameter-group) ())

(defclass generic-function-optional-parameters (parameter-group) ())

(defclass specialized-required-parameters (parameter-group) ())

(defclass parameter (grammar-symbol) ())

(defclass ordinary-required-parameter (parameter) ())

(defclass ordinary-optional-parameter (parameter) ())

(defclass key-parameter (parameter) ())

(defclass aux-parameter (parameter) ())

(defclass generic-function-optional-parameter (parameter) ())

(defclass specialized-required-parameter (parameter) ())

(defclass environment-parameter (paremeter) ())

(defclass whole-parameter (parameter) ())

(defclass lambda-list-type (grammar-symbol) ())

(defclass ordinary-lambda-list (lambda-list-type) ())

(defclass generic-function-lambda-list (lambda-list-type) ())

(defclass specialized-lambda-list (lambda-list-type) ())

(defclass macro-lambda-list (lambda-list-type) ())

(defclass destructuring-lambda-list (lambda-list-type) ())

(defclass boa-lambda-list (lambda-list-type) ())

(defclass defsetf-lambda-list (lambda-list-type) ())

(defclass deftype-lambda-list (lambda-list-type) ())

(defclass define-modify-macro-lambda-list (lambda-list-type) ())

(defclass define-method-combination-lambda-list (lambda-list-type) ())
