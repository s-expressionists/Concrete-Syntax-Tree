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
