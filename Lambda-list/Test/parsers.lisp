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
           :form nil
           :supplied-p (gensym)))
        ((null (cddr parameter))
         (make-instance 'cst::ordinary-optional-parameter
           :name (car parameter)
           :form (cadr parameter)
           :supplied-p (gensym)))
        (t
         (make-instance 'cst::ordinary-optional-parameter
           :name (car parameter)
           :form (cadr parameter)
           :supplied-p (caddr parameter)))))

(defun parse-ordinary-optional-parameters (remaining-input)
  (loop for input = remaining-input then (cdr input)
        until (or (null input)
                  (member (car input) lambda-list-keywords :test #'eq))
        collect (parse-ordinary-optional-parameter (car input))))

(defun parse-ordinary-key-parameter (parameter)
  (cond ((symbolp parameter)
         (make-instance 'cst::ordinary-key-parameter
           :name parameter
           :form nil
           :keyword (intern (symbol-name parameter) :keyword)
           :supplied-p (gensym)))
        ((null (cdr parameter))
         (if (symbolp (car parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (car parameter)
               :form nil
               :keyword (intern (symbol-name (car parameter)) :keyword)
               :supplied-p (gensym))
             (make-instance 'cst::ordinary-key-parameter
               :name (caar parameter)
               :form nil
               :keyword (cadar parameter)
               :supplied-p (gensym))))
        ((null (cddr parameter))
         (if (symbolp (car parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (car parameter)
               :form (cadr parameter)
               :keyword (intern (symbol-name (car parameter)) :keyword)
               :supplied-p (gensym))
             (make-instance 'cst::ordinary-key-parameter
               :name (caar parameter)
               :form (cadr parameter)
               :keyword (cadar parameter)
               :supplied-p (gensym))))
        (t
         (if (symbolp (car parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (car parameter)
               :form (cadr parameter)
               :keyword (intern (symbol-name (car parameter)) :keyword)
               :supplied-p (caddr parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (caar parameter)
               :form (cadr parameter)
               :keyword (cadar parameter)
               :supplied-p (caddr parameter))))))

(defun parse-ordinary-key-parameters (remaining-input)
  (loop for input = remaining-input then (cdr input)
        until (or (null input)
                  (member (car input) lambda-list-keywords :test #'eq))
        collect (parse-ordinary-key-parameter (car input))))

