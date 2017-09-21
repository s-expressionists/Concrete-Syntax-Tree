(cl:in-package #:concrete-syntax-tree)

(defun find-var (parsed-lambda-list parameter-group-type)
  (let* ((group (find-if (lambda (x) (typep x parameter-group-type))
                         (children parsed-lambda-list))))
    (if (cl:null group)
        nil
        (raw (parameter group)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-MACRO
;;;
;;; According to CLtL2, except that we have added a CLIENT parameter
;;; so that it will be possible to parse implementation-specific
;;; lambda-list keywords.

(defun parse-macro (client name lambda-list body &optional environment)
  (declare (ignore name environment)) ; For now.
  (let* ((parsed-lambda-list (parse-macro-lambda-list client lambda-list))
	 (env-var (find-var parsed-lambda-list 'environment-parameter-group))
	 (final-env-var (if (cl:null env-var) (gensym) env-var))
	 (form-var (find-var parsed-lambda-list 'whole-parameter-group))
	 (final-form-var (if (cl:null form-var) (gensym) form-var))
	 (args-var (gensym))
         (tail-var (gensym)))
      `(lambda (,final-form-var ,final-env-var)
	 ;; If the lambda list does not contain &environment, then
	 ;; we IGNORE the GENSYMed parameter to avoid warnings.
	 ;; If the lambda list does contain &environment, we do
	 ;; not want to make it IGNORABLE because we would want a
	 ;; warning if it is not used then.
	 ,@(if (cl:null env-var)
	       `((declare (ignore ,final-env-var)))
	       `())
	 (let ((,args-var (cdr ,final-form-var)))
           ,(destructure-lambda-list client
                                     parsed-lambda-list
                                     args-var
                                     tail-var
                                     body)))))
