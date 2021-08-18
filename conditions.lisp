(cl:in-package #:concrete-syntax-tree)

(define-condition cst-error (error acclimation:condition)
  ())

;;; This condition is signaled whenever a CST instance satisfying NULL
;;; was required, but something else was given.
(define-condition null-cst-required (cst-error)
  ((%cst :initarg :cst :reader cst)))

;;; This condition is signaled whenever an instance of the class
;;; CONS-CST was required, but something else was given.
(define-condition cons-cst-required (cst-error)
  ((%cst :initarg :cst :reader cst)))

(define-condition unquote-error (error) ())

(define-condition unquote-splicing-in-dotted-list (unquote-error) ())
(define-condition unquote-splicing-at-top (unquote-error) ())
