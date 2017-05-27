(cl:in-package #:concrete-syntax-tree)

(defgeneric scanner-action (client item lambda-list terminal input))

(defmethod scanner-action (client item lambda-list terminal input)
  '())

(defun advance-dot-position (item parse-tree)
  (make-instance 'earley-item
    :rule (rule item)
    :origin (origin item)
    :parse-trees (cl:cons parse-tree (parse-trees item))
    :dot-position (1+ (dot-position item))))

(defmethod scanner-action
    (client item lambda-list (terminal ordinary-required-parameter) input)
  (let ((allowed-keywords (allowed-lambda-list-keywords client lambda-list)))
    (if (and (symbolp input) (not (member input allowed-keywords)))
        (cl:list (advance-dot-position
                  item
                  (make-instance 'ordinary-required-parameter
                    :name input)))
        '())))

(defmethod scanner-action
    (client item lambda-list (terminal ordinary-optional-parameter) input)
  (let ((allowed-keywords (allowed-lambda-list-keywords client lambda-list)))
    (cond ((and (symbolp input) (not (member input allowed-keywords)))
           (cl:list (advance-dot-position
                     item
                     (make-instance 'ordinary-optional-parameter
                       :name input
                       :form nil
                       :supplied-p (gensym)))))
          ((cl:consp input)
           (cond ((cl:null (cdr input))
                  (cl:list (advance-dot-position
                            item
                            (make-instance 'ordinary-optional-parameter
                              :name (car input)
                              :form nil
                              :supplied-p (gensym)))))
                 ((cl:atom (cdr input))
                  '())
                 ((cl:null (cddr input))
                  (cl:list (advance-dot-position
                            item
                            (make-instance 'ordinary-optional-parameter
                              :name (car input)
                              :form (cadr input)
                              :supplied-p (gensym)))))
                 ((cl:atom (cddr input))
                  '())
                 ((cl:null (cdddr input))
                  (cl:list (advance-dot-position
                            item
                            (make-instance 'ordinary-optional-parameter
                              :name (car input)
                              :form (cadr input)
                              :supplied-p (caddr input)))))
                 (t
                  '())))
          (t
           '()))))

(defmethod scanner-action
    (client item lambda-list (terminal cl:cons) input)
  (let* ((new-terminal (cadr terminal))
         (terminal-class (find-class new-terminal))
         (proto (make-instance terminal-class))
         (result (scanner-action client item lambda-list proto input)))
    (cond ((string-equal (symbol-name (car terminal)) "?")
           result)
          ((string-equal (symbol-name (car terminal)) "*")
           (append result
                   (loop for item in result
                         collect (make-instance 'earley-item
                                   :rule (rule item)
                                   :origin (origin item)
                                   :parse-trees (parse-trees item)
                                   :dot-position (1- (dot-position item))))))
          (t (error "Unknown operator: ~s" (car terminal))))))

(defmethod scanner-action
    (client item lambda-list (terminal generic-function-optional-parameter) input)
  (let ((allowed-keywords (allowed-lambda-list-keywords client lambda-list)))
    (cond ((and (symbolp input) (not (member input allowed-keywords)))
           (cl:list (advance-dot-position
                     item
                     (make-instance 'generic-function-optional-parameter
                       :name input))))
          ((and (cl:consp input) (cl:null (cdr input)))
           (cl:list (advance-dot-position
                     item
                     (make-instance 'generic-function-optional-parameter
                       :name (car input)))))
          (t
           '()))))

(defmacro define-keyword-scanner-action (keyword-class-name symbol)
  `(defmethod scanner-action
       (client item lambda-list (terminal ,keyword-class-name) input)
     (declare (ignore client lambda-list))
     (if (eq input ',symbol)
         (cl:list (advance-dot-position
                   item
                   (make-instance ',keyword-class-name
                     :name input)))
         '())))

(define-keyword-scanner-action keyword-optional &optional)
(define-keyword-scanner-action keyword-rest &rest)
(define-keyword-scanner-action keyword-body &rest)
(define-keyword-scanner-action keyword-key &key)
(define-keyword-scanner-action keyword-key &allow-other-keys)
(define-keyword-scanner-action keyword-aux &aux)
(define-keyword-scanner-action keyword-environment &environment)
(define-keyword-scanner-action keyword-whole &whole)
