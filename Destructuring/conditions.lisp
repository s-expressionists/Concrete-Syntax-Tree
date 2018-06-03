(cl:in-package #:concrete-syntax-tree)

;;; Supertype for condition types related to structure mismatches
;;; during CST destructuring.
(define-condition structure-mismatch-error (cst-error)
  ((%pattern :initarg :pattern :reader pattern)
   (%whole-cst :initarg :whole-cst :reader whole-cst)))

;;; This condition is signaled whenever during destructuring of a CST
;;; instance, a CST instance satisfying NULL was expected at a
;;; particular location, but something else was given.
(define-condition null-structure-mismatch-error (structure-mismatch-error
                                                 null-cst-required)
  ())

;;; This condition is signaled whenever during destructuring of a CST
;;; instance, a CONS-CST instance was expected at a particular
;;; location, but something else was given.
(define-condition cons-structure-mismatch-error (structure-mismatch-error
                                                 cons-cst-required)
  ())
