(cl:in-package #:concrete-syntax-tree)

;;;; The purpose of the functions and the macro in this file is to
;;;; help handle source-tracking information.  The main entry point in
;;;; this file is the macro DB which is very similar to the standard
;;;; Common Lisp macro DESTRUCTURING-BIND, except that DB is less
;;;; general than DESTRUCTURING-BIND in that it does not handle an
;;;; arbitrary lambda list; only a trees of variables, similar to the
;;;; destructuring done by LOOP.  Also, DB does not destructure an
;;;; ordinary Common Lisp tree, and instead works on a concrete syntax
;;;; tree.  Finally, DP takes an additional argument (the first one)
;;;; compared to DESTRUCTURING-BIND.  That argument is a variable that
;;;; will be bound to the SOURCE slot of the CST.

;;; This function generates code for destructuring a concrete syntax
;;; tree according to a tree of variables.  TREE is a tree of variable
;;; names (symbols).  FORM is a form that, at runtime, computes the
;;; concrete syntax tree to be assigned to the root of TREE.  This
;;; function returns a list of bindings to be used in a LET* form.
;;; These bindings destructure the root value until the leaves of the
;;; tree are reached, generating intermediate temporary variables as
;;; necessary.  The destructuring code calls the functions CST:FIRST
;;; and CST:REST so that an error is signaled whenever the
;;; corresponding place in the value tree is not a CONS-CST
(defun destructure-variables (tree form)
  (let ((bindings '()))
    (labels ((traverse (tree form)
	       (cond ((null tree)
		      nil)
		     ((symbolp tree)
		      (push `(,tree ,form) bindings))
		     ((not (cl:consp tree))
		      (error 'expectetree-but-found
			     :found tree))
		     (t
		      (let ((temp (gensym)))
			(push `(,temp ,form) bindings)
			(traverse (first tree) `(first ,temp))
			(traverse (rest tree) `(rest ,temp)))))))
      (traverse tree form)
      (reverse bindings))))

(defmacro db (source-var tree form &body body)
  (let ((form-var (gensym)))
    `(let* ((,form-var ,form)
	    (,source-var (source ,form-var))
            ,@(destructure-variables tree form-var))
       (declare (ignorable ,source-var))
       ,@body)))
