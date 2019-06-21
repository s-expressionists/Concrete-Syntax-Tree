(cl:in-package #:concrete-syntax-tree)

;;;; Generally speaking, these functions collectively take a macro
;;;; lambda list and a variable, and return a list of LET* bindings
;;;; that will bind the variables in that lambda list to the value
;;;; in that variable.

;;;; Each function rhandles a different part of the lambda list.
;;;; CLIENT is some object representing the client. ARGUMENT-VARIABLE
;;;; is a symbol that, when the resulting macro function is executed
;;;; on some compound form corresponding to a macro call, will hold
;;;; the remaining part of the arguments of that macro call yet to be
;;;; processed. If the lambda list is nontrivial, the LET* bindings
;;;; returned will repeatedly rebind this variable for the sake of
;;;; later parts of the lambda list.

;;; Given an entire lambda list, which can be a macro lambda list or
;;; a destructuring lambda list, return LET* bindings according to
;;; the parameters of the lambda list.
(defgeneric destructuring-lambda-list-bindings
    (client lambda-list argument-variable))

;;; Return LET* bindings corresponding to the parameters in the list
;;; of parameter groups, PARAMETER-GROUPS.
(defgeneric parameter-groups-bindings
    (client parameter-groups argument-variable))

;;; Return LET* bindings for a single &AUX parameter. Since &AUX
;;; parameters are independent of the macro-call arguments, there is
;;; no need for an ARGUMENT-VARIABLE. The &AUX parameter itself
;;; provides all the information required to determine the bindings.
(defgeneric aux-parameter-bindings (client parameter))

;;; Return LET* bindings for a list of &AUX parameters.
(defgeneric aux-parameters-bindings (client parameters))

;;; Return LET* bindings for a single &KEY parameter.
(defgeneric key-parameter-bindings (client parameter argument-variable))

;;; Return LET* bindings for a list of &KEY parameters.
(defgeneric key-parameters-bindings (client parameters argument-variable))

;;; Return LET* bindings for a &REST parameter.
(defgeneric rest-parameter-bindings (client parameter argument-variable))

;;; Return LET* bindings for a single &OPTIONAL parameter.
(defgeneric optional-parameter-bindings
    (client parameter argument-variable))

;;; Return LET* bindings for a list of &OPTIONAL parameters.
(defgeneric optional-parameters-bindings
    (client parameters argument-variable))

;;; Return LET* bindings for a single required parameter.
(defgeneric required-parameter-bindings
    (client parameter argument-variable))

;;; Return LET* bindings for a list of required parameters.
(defgeneric required-parameters-bindings
    (client parameters argument-variable))
