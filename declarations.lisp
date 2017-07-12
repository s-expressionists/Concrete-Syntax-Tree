(cl:in-package #:concrete-syntax-tree)

;;; This function has four parameters.  The first parameter identifies
;;; the client system.  We do not specialize on that parameter, but
;;; client code can customize the behavior by overriding or extending
;;; the behavior of the methods defined here.  The second parameter is
;;; the declaration identifier.  It is a symbol that identifies what
;;; kind of declaration we are dealing with.  The third parameter is
;;; the CST version of the declaration identifier.  The fourth
;;; parameter is the declaration data, i.e. whatever follows the
;;; declaration identifier in a declaration.  This function returns an
;;; ordinary Common Lisp list of CSTs.  Each CST represents a list of
;;; the declaration identifier given as an argument, and a single item
;;; in the declaration data.
(defgeneric canonicalize-declaration-specifier
    (system declaration-identifier declaration-identifier-cst declaration-data))

(defmethod  canonicalize-declaration-specifier
    (system declaration-identifier declaration-identifier-cst declaration-data)
  '())

;;; Given a PREFIX P and a list of ITEMS, say (I1 I2 ... In), return a
;;; list of the items prefixed with P, i.e. ((P I1) (P I2) ... (P
;;; In)).  The twist is that the list of items is represented in the
;;; form of a concrete syntax tree.
(defun map-prefix (prefix items)
  (loop for remaining = items then (rest remaining)
        until (null remaining)
        collect (list prefix (first remaining))))

(defmacro define-simple-canonicalize-method (declaration-identifier)
  `(defmethod  canonicalize-declaration-specifier
       (system
        (declaration-identifier (eql ',declaration-identifier))
        declaration-identifier-cst
        declaration-data)
     (map-prefix declaration-identifier-cst declaration-data)))

(progn
  . #.(loop for declaration-identifier in
            '(declaration dynamic-extent ignore ignorable
              inline notinline optimize special)
            collect `(define-simple-canonicalize-method ,declaration-identifier)))

(defmethod  canonicalize-declaration-specifier
    (system
     (declaration-identifier (eql 'ftype))
     declaration-identifier-cst
     declaration-data)
  (loop with type = (first declaration-data)
        for remaining = (rest declaration-data) then (rest remaining)
        until (null remaining)
        collect (list declaration-identifier-cst type (first remaining))))

(defmethod  canonicalize-declaration-specifier
    (system
     (declaration-identifier (eql 'type))
     declaration-identifier-cst
     declaration-data)
  (loop with type = (first declaration-data)
        for remaining = (rest declaration-data) then (rest remaining)
        until (null remaining)
        collect (list declaration-identifier-cst type (first remaining))))
