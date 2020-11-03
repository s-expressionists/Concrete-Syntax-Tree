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
  (declare (ignore declaration-identifier))
  ;; Treat as a type declaration.
  ;; Declarations from PROCLAIM DECLARATION will have already been filtered out
  ;; in CANONICALIZE-DECLARATION-SPECIFIERS, below.
  (canonicalize-declaration-specifier
   system 'type
   (make-instance 'atom-cst :raw 'type :source (source declaration-identifier-cst))
   (cons declaration-identifier-cst declaration-data)))

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
     (declare (ignore system))
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
  (declare (ignore system))
  (loop with type = (first declaration-data)
        for remaining = (rest declaration-data) then (rest remaining)
        until (null remaining)
        collect (list declaration-identifier-cst type (first remaining))))

(defmethod  canonicalize-declaration-specifier
    (system
     (declaration-identifier (eql 'type))
     declaration-identifier-cst
     declaration-data)
  (declare (ignore system))
  (loop with type = (first declaration-data)
        for remaining = (rest declaration-data) then (rest remaining)
        until (null remaining)
        collect (list declaration-identifier-cst type (first remaining))))

;;; IGNORE-DECLS is a list of symbols. These symbols are declaration identifiers
;;; that CST should ignore, i.e., these declarations will be canonicalized as NIL.
(defun canonicalize-declaration-specifiers (system ignore-decls declaration-specifiers)
  (reduce #'append
          (mapcar (lambda (specifier)
                    (let* ((declaration-identifier-cst (first specifier))
                           (declaration-data-cst (rest specifier))
                           (declaration-identifier (raw declaration-identifier-cst)))
                      ;; Filter out ignored declarations.
                      ;; (Intended for PROCLAIM DECLARATION.)
                      (if (member declaration-identifier ignore-decls :test #'eq)
                          nil
                          (canonicalize-declaration-specifier
                           system
                           declaration-identifier
                           declaration-identifier-cst
                           declaration-data-cst))))
                  declaration-specifiers)
          :from-end t))

;;; Given an ordinary Common Lisp list of declarations, each
;;; declaration being represented as a CST, return an ordinary Common
;;; Lisp list of all the declaration specifiers.  The raw form of a
;;; CST of the input is:
;;;
;;;  (DECLARE <declaration-specifier> ... <declaration-specifier>)
(defun declaration-specifiers (declaration-csts)
  (loop for declaration-cst in declaration-csts
        append (loop for cst = (rest declaration-cst) then (rest cst)
                     until (null cst)
                     collect (first cst))))

;;; Given an ordinary Common Lisp list of declarations, each being
;;; represented as a CST, return a list of canonicalized declaration
;;; specifiers of all the declarations.
(defun canonicalize-declarations (system ignore-decls declarations)
  (canonicalize-declaration-specifiers
   system ignore-decls (declaration-specifiers declarations)))
