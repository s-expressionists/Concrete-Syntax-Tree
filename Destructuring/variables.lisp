(cl:in-package #:concrete-syntax-tree)

;;; This variable is used internally to maintain a reference to a lambda list
;;; that bindings are being generated for. It is used in error reporting.
(defvar *current-lambda-list*)

;;; This variable is used internally to maintain a reference to the name of a
;;; macro for which a lambda list is being parsed, for error reporting.
;;; If no macro is being parsed, its value is NIL.
(defvar *current-macro-name* nil)
