(cl:in-package #:concrete-syntax-tree)

(defun find-var (parsed-lambda-list parameter-group-type)
  (let* ((group (find-if (lambda (x) (typep x parameter-group-type))
                         (children parsed-lambda-list))))
    (if (cl:null group)
        nil
        (name (parameter group)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-MACRO
;;;
;;; According to CLtL2, except that we have added a CLIENT parameter
;;; so that it will be possible to parse implementation-specific
;;; lambda-list keywords, and a SOURCE parameter so that the overall
;;; source location of the macro can be recorded.

(defun parse-macro (client name lambda-list body
                    &optional environment source)
  (declare (ignore environment)) ; For now.
  (let* ((parsed-lambda-list (parse-macro-lambda-list client lambda-list))
	 (env-var (find-var parsed-lambda-list 'environment-parameter-group))
	 (final-env-var (if (cl:null env-var)
                            (make-instance 'atom-cst
                              :raw (gensym "ENV") :source source)
                            env-var))
	 (form-var (find-var parsed-lambda-list 'whole-parameter-group))
	 (final-form-var (if (cl:null form-var)
                             (make-instance 'atom-cst
                               :raw (gensym "WHOLE") :source source)
                             form-var))
         (children (children parsed-lambda-list))
         (relevant-children
           (remove-if (lambda (x) (typep x 'environment-parameter-group))
                      (remove-if (lambda (x) (typep x 'whole-parameter-group))
                                 children)))
         (relevant-lambda-list
           (make-instance 'cst:macro-lambda-list :children relevant-children))
         (args-var
           (make-instance 'atom-cst :raw (gensym) :source source)))
    (multiple-value-bind (bindings ignorables)
        (destructuring-lambda-list-bindings
         client relevant-lambda-list args-var source)
      (quasiquote
       source
       (lambda ((unquote final-form-var) (unquote final-env-var))
         (block (unquote name)
           (let* (((unquote args-var) (cdr (unquote final-form-var)))
                  (unquote-splicing (cstify bindings))
                  ;; We rebind the whole and environment variables
                  ;; here, so that any user declarations for them
                  ;; are scoped, properly.
                  ;; We do this AFTER the args-var binding so that
                  ;; if, e.g., a &whole is declared ignore, the
                  ;; compiler does not complain that it was used
                  ;; for the args-var binding.
                  (unquote-splicing
                    (if (cl:null form-var)
                        (quasiquote source ())
                        (quasiquote
                         source
                         (((unquote final-form-var)
                           (unquote final-form-var))))))
                  ((unquote final-env-var) (unquote final-env-var)))
             (declare (ignorable (unquote-splicing (cstify ignorables)))
                      ;; If the lambda list does not contain &environment, then
                      ;; we IGNORE the GENSYMed parameter to avoid warnings.
                      ;; If the lambda list does contain &environment, we do
                      ;; not want to make it IGNORABLE because we would want a
                      ;; warning if it is not used then.
                      (unquote-splicing
                       (if (cl:null env-var)
                           (quasiquote source
                                       ((ignore (unquote final-env-var))))
                           (quasiquote source ()))))
             (unquote-splicing body))))))))
