(cl:in-package #:concrete-syntax-tree)

(defun find-var (parsed-lambda-list parameter-group-type)
  (let* ((group (find-if (lambda (x) (typep x parameter-group-type))
                         (children parsed-lambda-list))))
    (if (cl:null group)
        nil
        (raw (name (parameter group))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-MACRO
;;;
;;; According to CLtL2, except that we have added a CLIENT parameter
;;; so that it will be possible to parse implementation-specific
;;; lambda-list keywords.

(defun parse-macro (client name lambda-list body &optional environment)
  (declare (ignore environment)) ; For now.
  (let* ((parsed-lambda-list (parse-macro-lambda-list client lambda-list))
         (*current-lambda-list* parsed-lambda-list)
         (raw-name (raw name))
         (*current-macro-name* raw-name) 
	 (env-var (find-var parsed-lambda-list 'environment-parameter-group))
	 (final-env-var (if (cl:null env-var) (gensym "ENV") env-var))
         (form-var (gensym "WHOLE"))
         (children (children parsed-lambda-list))
         (toplevel-whole-group
           (find-if (lambda (x) (typep x 'whole-parameter-group)) children))
         (relevant-children
           (remove-if (lambda (x) (typep x 'environment-parameter-group))
                      (remove-if (lambda (x) (typep x 'whole-parameter-group))
                                 children)))
         (relevant-lambda-list
           (make-instance 'cst:macro-lambda-list :children relevant-children))
	 (args-var (gensym)))
    (multiple-value-bind (main-bindings main-ignorables)
        (destructuring-lambda-list-bindings
         client relevant-lambda-list args-var)
      ;; Any toplevel &WHOLE parameter is handled separately, because it
      ;; starts with a different argument-variable.
      (multiple-value-bind (whole-bindings whole-ignorables)
          (if toplevel-whole-group
              (parameter-group-bindings client toplevel-whole-group form-var)
              (values nil nil))
        `(lambda (,form-var ,final-env-var)
           (block ,raw-name
             (let* ((,args-var (cdr ,form-var))
                    ,@whole-bindings
                    ,@main-bindings
                    ;; We rebind the environment variable here, so that any
                    ;; user declarations for them are scoped, properly.
                    (,final-env-var ,final-env-var))
               (declare (ignorable ,@whole-ignorables ,@main-ignorables)
                        ;; If the lambda list does not contain &environment,
                        ;; then we IGNORE the GENSYMed parameter to avoid
                        ;; warnings.
                        ;; If the lambda list does contain &environment, we do
                        ;; not want to make it IGNORABLE because we would want
                        ;; a warning if it is not used then.
                        ,@(if (cl:null env-var)
                              `((ignore ,final-env-var))
                              `()))
               ,@body)))))))
