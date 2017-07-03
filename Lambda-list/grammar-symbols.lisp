(cl:in-package #:concrete-syntax-tree)

;;; This is the root class of all grammar symbols.
(defclass grammar-symbol ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parameter groups.
;;;
;;; A parameter group represents a list of parameters of the same kind
;;; and that appear together in a lambda list.

;;; The root of all classes that represent parameter groups.
(defclass parameter-group (grammar-symbol)
  ())

(defclass singleton-parameter-group-mixin ()
  ((%parameter :initarg :parameter :reader parameter)))

(defclass multi-parameter-group-mixin ()
  ((%parameters :initarg :parameters :reader parameters)))

;;; An instance of this class represents a parameter group that does
;;; not have any associated lambda-list keyword.  Every different kind
;;; of required parameter group is a subclass of this class.
(defclass implicit-parameter-group (parameter-group multi-parameter-group-mixin)
  ())

;;; When an instance of an implicit parameter group is created, we
;;; initialize the parameters from the CHILDREN keyword passed by the
;;; completer.
(defmethod initialize-instance :after
    ((parameter-group implicit-parameter-group) &key children)
  (reinitialize-instance parameter-group
                         :parameters children))

;;; An instance of this class represents a parameter group that has an
;;; associated lambda-list keyword.
(defclass explicit-parameter-group (parameter-group)
  ((%keyword :initarg :keyword :reader keyword)))

(defclass explicit-multi-parameter-group
    (explicit-parameter-group multi-parameter-group-mixin)
  ())

;;; When an instance of an explicit parameter group is created, we
;;; want to separate the keyword from the list of parameters.
(defmethod initialize-instance :after
    ((parameter-group explicit-multi-parameter-group) &key children)
  (reinitialize-instance parameter-group
                         :keyword (car children)
                         :parameters (cdr children)))

(defclass ordinary-required-parameter-group (implicit-parameter-group)
  ())

(defclass optional-parameter-group (explicit-multi-parameter-group)
  ())

(defclass ordinary-optional-parameter-group (optional-parameter-group)
  ())

(defclass key-parameter-group (explicit-multi-parameter-group)
  (;; This slot can be either &ALLOW-OTHER-KEYS, if that lambda-list
   ;; keyword is present, or NIL if it is absent.
   (%allow-other-keys :initarg :allow-other-keys :reader allow-other-keys)))

(defclass ordinary-key-parameter-group (key-parameter-group)
  ())

(defmethod initialize-instance :after
    ((parameter-group key-parameter-group) &key children)
  (when (typep (car (last children)) 'keyword-allow-other-keys)
    (reinitialize-instance parameter-group
                           :keyword (car children)
                           :parameters (cdr (butlast children))
                           :allow-other-keys (car (last children)))))

(defclass generic-function-key-parameter-group (key-parameter-group)
  ())

(defclass aux-parameter-group (explicit-multi-parameter-group)
  ())

(defclass generic-function-optional-parameter-group (optional-parameter-group)
  ())

(defclass specialized-required-parameter-group (implicit-parameter-group)
  ())

(defclass destructuring-required-parameter-group (implicit-parameter-group)
  ())

;;; This class is the root class of parameter groups that take a
;;; keyword and a single parameter, such as &WHOLE, &ENVIRONMENT,
;;; &REST, &BODY.
(defclass singleton-parameter-group
    (explicit-parameter-group singleton-parameter-group-mixin)
  ())

;;; When an instance of a singleton parameter group is created, we
;;; want to separate the keyword from the parameter itself.
(defmethod initialize-instance :after
    ((parameter singleton-parameter-group) &key children)
  (reinitialize-instance parameter
                         :keyword (car children)
                         :parameter (cadr children)))

(defclass ordinary-rest-parameter-group (singleton-parameter-group)
  ())

(defclass destructuring-rest-parameter-group (singleton-parameter-group)
  ())

(defclass environment-parameter-group (singleton-parameter-group)
  ())

(defclass whole-parameter-group (singleton-parameter-group)
  ())

;;; This class is the root of all classes that correspond to
;;; individual parameters.  Instance of (subclasses of) this class are
;;; handled by the scanner.
(defclass parameter (grammar-symbol)
  ((%name :initarg :name :reader name)))

(defclass form-mixin ()
  ((%form :initarg :form :reader form)))

(defclass supplied-p-mixin ()
  ((%supplied-p :initarg :supplied-p :reader supplied-p)))

(defclass keyword-mixin ()
  ((%keyword :initarg :keyword :reader keyword)))

(defclass simple-variable (parameter)
  ())

(defclass ordinary-optional-parameter (parameter form-mixin supplied-p-mixin)
  ())

(defclass ordinary-key-parameter
    (parameter form-mixin supplied-p-mixin keyword-mixin)
  ())

(defclass generic-function-key-parameter (parameter keyword-mixin)
  ())

(defclass aux-parameter (parameter form-mixin)
  ())

;;; A generic-function optional parameter differs from an ordinary
;;; optional parameter in that it can have neither a form to determine
;;; a default value, nor an associated supplied-p parameter.
(defclass generic-function-optional-parameter (parameter)
  ())

(defclass specialized-required-parameter (parameter)
  ((%specializer :initarg :specializer :reader specializer )))

;;; This class will never be part of a parse tree.  When the scanner
;;; sees an instance of this class, it looks at the input to determine
;;; whether it is a symbol or a CONS cell.  If it is a symbol, it
;;; creates a SIMPLE-VARIABLE, and if it is a CONS cell, it
;;; recursively parses the list as a DESTRUCTURING-LAMBDA-LIST which
;;; then becomes the resulting parse tree.
(defclass destructuring-parameter (grammar-symbol) ())

(defclass lambda-list-keyword (grammar-symbol)
  ((%name :initarg :name :reader name)))

(defclass keyword-optional (lambda-list-keyword) ())

(defclass keyword-rest (lambda-list-keyword) ())

(defclass keyword-body (lambda-list-keyword) ())

(defclass keyword-key (lambda-list-keyword) ())

(defclass keyword-allow-other-keys (lambda-list-keyword) ())

(defclass keyword-aux (lambda-list-keyword) ())

(defclass keyword-environment (lambda-list-keyword) ())

(defclass keyword-whole (lambda-list-keyword) ())

(defclass lambda-list-type (grammar-symbol)
  ((%children :initarg :children :reader children)))

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

(defclass target (grammar-symbol)
  ((%children :initarg :children :reader children)))
