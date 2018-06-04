(cl:in-package #:concrete-syntax-tree)

;;; Given a CST, return the location of the source for it.  The source
;;; location is represented by a client-defined object.
(defgeneric source (cst))

;;; Return the parent of CST.  If CST does not have a parent, then NIL
;;; is returned instead.
(defgeneric parent (cst))

;;; Return true if and only if CST is an instance of NULL-CST.
(defgeneric null (cst))

;;; Return true if and only if CST represents an atomic expression.
(defgeneric atom (cst))

;;; Return true if and only if CST represents a CONS expression.
(defgeneric consp (cst))

;;; Given a CST, return the underlying Common Lisp expression.
(defgeneric raw (cst))

;;; Given a CONS-CST, return the FIRST of that CST.  If some other CST
;;; type is given, including an ATOM-CST representing NIL, then an
;;; error is signaled.
(defgeneric first (cons-cst))

;;; Given a CONS-CST, return the REST of that CST.  If some other CST
;;; type is given, including an ATOM-CST representing NIL, then an
;;; error is signaled.
(defgeneric rest (cons-cst))

;;; Given a CST representing a proper list, return an ordinary Common
;;; Lisp list of the CSTs that are elements of that CST.
(defgeneric listify (cst))

;;; Given an ordinary proper Common Lisp list of CSTs, return a CST,
;;; the elements of which are the CSTs of the input.
(defgeneric cstify (list))

;;; Given a body in the form of a CST that may contain declarations
;;; but not a documentation string, return two values, a list of the
;;; declarations and a list of the forms in the body.  Each return
;;; value is an ordinary Common Lisp list, but the elements are CSTs.
;;; It is assumed that the input has already been determined to be a
;;; proper list represented as a CST.
(defgeneric separate-ordinary-body (body-cst))

;;; Given a body in the form of a CST that may contain both
;;; declarations and a documentation string, return three values, a
;;; list of the declarations, an ATOM-CST representing the
;;; documentation string (or NIL if no documentation string is present
;;; in the body) and a list of the forms in the body.  The first and
;;; third return values are ordinary Common Lisp lists, but the
;;; elements are CSTs.  It is assumed that the input has already been
;;; determined to be a proper list represented as a CST.
(defgeneric separate-function-body (body-cst))

;;; Given a CST and an expression that is presumably some transformed
;;; version of the raw version of the CST, create a new CST that tries
;;; to reuse as much as possible of the given CST, so as to preserve
;;; source information.
(defgeneric reconstruct (expression cst client &key default-source))
