(cl:in-package #:concrete-syntax-tree)

(defclass grammar-symbol ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parameter groups.
;;;
;;; A parameter group represents a list of parameters of the same kind
;;; and that appear together in a lambda list.

;;; The root of all classes that represent parameter groups.  This
;;; class has a single slot, named PARAMETERS which contains a
;;; (possibly empty) list of parameters of the kind admitted by this
;;; particular kind of parameter group.
(defclass parameter-group (grammar-symbol)
  ((%parameters :initarg :parameters :reader parameters)))

;;; An instance of this class represents a parameter group that does
;;; not have any associated lambda-list keyword.  Every different kind
;;; of required parameter group is a subclass of this class.
(defclass implicit-parameter-group (parameter-group)
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

;;; When an instance of an explicit parameter group is created, we
;;; want to separate the keyword from the list of parameters.
(defmethod initialize-instance :after
    ((parameter-group explicit-parameter-group) &key children)
  (reinitialize-instance parameter-group
                         :keyword (car children)
                         :parameters (cdr children)))

(defclass ordinary-required-parameter-group (implicit-parameter-group)
  ())

(defclass ordinary-optional-parameter-group (explicit-parameter-group)
  ())

(defclass ordinary-key-parameter-group (explicit-parameter-group)
  (;; This slot can be either &ALLOW-OTHER-KEYS, if that lambda-list
   ;; keyword is present, or NIL if it is absent.
   (%allow-other-keys :initarg :allow-other-keys :reader allow-other-keys)))

(defmethod initialize-instance :after
    ((parameter-group ordinary-key-parameter-group) &key children)
  (when (typep (car (last children)) 'keyword-allow-other-keys)
    (reinitialize-instance parameter-group
                           :keyword (car children)
                           :parameters (cdr (butlast children))
                           :allow-other-keys (car (last children)))))

(defclass generic-function-key-parameter-group (explicit-parameter-group)
  (;; This slot can be either &ALLOW-OTHER-KEYS, if that lambda-list
   ;; keyword is present, or NIL if it is absent.
   (%allow-other-keys :initarg :allow-other-keys :reader allow-other-keys)))

(defmethod initialize-instance :after
    ((parameter-group generic-function-key-parameter-group) &key children)
  (when (typep (car (last children)) 'keyword-allow-other-keys)
    (reinitialize-instance parameter-group
                           :keyword (car children)
                           :parameters (cdr (butlast children))
                           :allow-other-keys (car (last children)))))

(defclass aux-parameter-group (explicit-parameter-group)
  ())

(defclass generic-function-optional-parameter-group (explicit-parameter-group)
  ())

(defclass specialized-required-parameter-group (implicit-parameter-group)
  ())

(defclass destructuring-required-parameter-group (implicit-parameter-group)
  ())

;;; This class is the root class of parameter groups that take a
;;; keyword and a single parameter, such as &WHOLE, &ENVIRONMENT,
;;; &REST, &BODY.
(defclass singleton-parameter-group (grammar-symbol)
  ((%keyword :initarg :keyword :reader keyword)
   (%parameter :initarg :parameter :reader parameter)))

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

(defclass simple-variable (parameter)
  ())

(defclass ordinary-optional-parameter (parameter)
  ((%form :initarg :form :reader form)
   (%supplied-p :initarg :supplied-p :reader supplied-p)))

(defclass ordinary-key-parameter (parameter)
  ((%form :initarg :form :reader form)
   (%keyword :initarg :keyword :reader keyword)
   (%supplied-p :initarg :supplied-p :reader supplied-p)))

(defclass generic-function-key-parameter (parameter)
  ((%keyword :initarg :keyword :reader keyword)))

(defclass aux-parameter (parameter)
  ((%form :initarg :form :reader form)))

;;; A generic-function optional parameter differs from an ordinary
;;; optional parameter in that it can have neither a form to determine
;;; a default value, nor an associated supplied-p parameter.
(defclass generic-function-optional-parameter (parameter)
  ())

(defclass specialized-required-parameter (parameter)
  ((%specializer :initarg :specializer :reader specializer )))

(defclass environment-parameter (parameter) ())

(defclass whole-parameter (parameter) ())

(defclass destructuring-required-parameter (parameter) ())


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
