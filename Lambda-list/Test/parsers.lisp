(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defun parse-ordinary-required-parameter (parameter)
  (make-instance 'cst::ordinary-required-parameter :name parameter))

(defun parse-ordinary-required-parameters (remaining-input)
  (make-instance 'cst::ordinary-required-parameters
    :children 
    (loop for input = remaining-input then (cdr input)
          until (or (null input)
                    (member (car input) lambda-list-keywords :test #'eq))
          collect (parse-ordinary-required-parameter (car input)))))

(defun parse-ordinary-optional-parameter (parameter)
  (cond ((symbolp parameter)
         (make-instance 'cst::ordinary-optional-parameter
           :name parameter
           :form nil
           :supplied-p (gensym)))
        ((null (cdr parameter))
         (make-instance 'cst::ordinary-optional-parameter
           :name (car parameter)
           :form (cadr parameter)
           :supplied-p (gensym)))
        (t
         (make-instance 'cst::ordinary-optional-parameter
           :name (car parameter)
           :form (cadr parameter)
           :supplied-p (caddr parameter)))))
