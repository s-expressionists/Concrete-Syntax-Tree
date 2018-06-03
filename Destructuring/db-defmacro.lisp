(cl:in-package #:concrete-syntax-tree)

(defun %null-or-lose (cst whole-cst pattern)
  (unless (null cst)
    (error 'null-structure-mismatch-error
           :pattern pattern
           :whole-cst whole-cst
           :cst cst)))

(defun %first-or-lose (cst whole-cst pattern)
  (if (consp cst)
      (first cst)
      (error 'cons-structure-mismatch-error
             :pattern pattern
             :whole-cst whole-cst
             :cst cst)))

(defun %rest-or-lose (cst whole-cst pattern)
  (if (consp cst)
      (rest cst)
      (error 'cons-structure-mismatch-error
             :pattern pattern
             :whole-cst whole-cst
             :cst cst)))

;;;; The purpose of the functions and the macro in this file is to
;;;; help handle source-tracking information.  The main entry point in
;;;; this file is the macro DB which is very similar to the standard
;;;; Common Lisp macro DESTRUCTURING-BIND, except that DB is less
;;;; general than DESTRUCTURING-BIND in that it does not handle an
;;;; arbitrary lambda list; only a trees of variables, similar to the
;;;; destructuring done by LOOP.  Also, DB does not destructure an
;;;; ordinary Common Lisp tree, and instead works on a concrete syntax
;;;; tree.  Finally, DB takes an additional argument (the first one)
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
  (let ((bindings '())
        (body-forms '()))
    (labels ((traverse (sub-tree sub-form)
               (cond ((cl:null sub-tree)
                      (push `(%null-or-lose ,sub-form ,form ',tree)
                            body-forms))
                     ((symbolp sub-tree)
                      (push `(,sub-tree ,sub-form) bindings))
                     ((not (cl:consp sub-tree))
                      (error 'expectetree-but-found ; TODO undefined?
                             :found sub-tree))
                     (t
                      (let ((temp (gensym)))
                        (push `(,temp ,sub-form) bindings)
                        (traverse (cl:first sub-tree)
                                  `(%first-or-lose ,temp ,form ',tree))
                        (traverse (cl:rest sub-tree)
                                  `(%rest-or-lose ,temp ,form ',tree)))))))
      (traverse tree form))
    (values (reverse bindings) (nreverse body-forms))))

(defmacro db (source-var tree form &body body)
  ;; We use the DUMMY-VAR hack so we can execute BODY-FORMS after
  ;; BINDINGS but before BODY without messing with BODY's
  ;; declarations.
  (let ((form-var (gensym)) (dummy-var (gensym)))
    (multiple-value-bind (bindings body-forms)
        (destructure-variables tree form-var)
      `(let* ((,form-var ,form)
              (,source-var (source ,form-var))
              ,@bindings
              (,dummy-var ,@body-forms))
         (declare (ignorable ,source-var ,dummy-var))
         ,@body))))
