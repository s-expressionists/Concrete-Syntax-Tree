(cl:in-package #:concrete-syntax-tree)

(defun shapep (list shape)
  (if (cl:atom shape)
      (typep list shape)
      (and ;; FIXME, add test that list is a proper list
       (cl:consp list)
       (= (length list) (length shape))
       (every #'shapep list shape))))

(defun path (list path)
  (if (cl:null path)
      list
      (path (cl:nth (car path) list) (cdr path))))

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
    (client item lambda-list (terminal simple-variable) input)
  (let ((allowed-keywords (allowed-lambda-list-keywords client lambda-list)))
    (if (and (symbolp input) (not (member input allowed-keywords)))
        (cl:list (advance-dot-position
                  item
                  (make-instance 'simple-variable
                    :name input)))
        '())))

(defmethod scanner-action
    (client item lambda-list (terminal ordinary-optional-parameter) input)
  (let ((allowed-keywords (allowed-lambda-list-keywords client lambda-list))
        (correct-syntax-p t)
        name form supplied-p)
    (cond ((and (shapep input 'symbol) (not (member input allowed-keywords)))
           (setf name input)
           (setf form nil)
           (setf supplied-p (gensym)))
          ((shapep input '(symbol))
           (setf name (path input '(0)))
           (setf form nil)
           (setf supplied-p (gensym)))
          ((shapep input '(symbol t))
           (setf name (path input '(0)))
           (setf form (path input '(1)))
           (setf supplied-p (gensym)))
          ((shapep input '(symbol t symbol))
           (setf name (path input '(0)))
           (setf form (path input '(1)))
           (setf supplied-p (path input '(2))))
          (t
           (setf correct-syntax-p nil)))
    (if correct-syntax-p
        (cl:list (advance-dot-position
                  item
                  (make-instance 'ordinary-optional-parameter
                    :name name :form form :supplied-p supplied-p)))
        '())))

(defmethod scanner-action
    (client item lambda-list (terminal ordinary-key-parameter) input)
  (let ((allowed-keywords (allowed-lambda-list-keywords client lambda-list))
        (correct-syntax-p t)
        name keyword form supplied-p)
    (cond ((and (symbolp input) (not (member input allowed-keywords)))
           (setf name input)
           (setf keyword (intern (symbol-name input) '#:keyword))
           (setf form nil)
           (setf supplied-p (gensym)))
          ((cl:consp input)
           (cond ((symbolp (car input))
                  (setf name (car input))
                  (setf keyword (intern (symbol-name (car input)) '#:keyword)))
                 ((cl:consp (car input))
                  (if (and (symbolp (caar input))
                           (cl:consp (cdar input))
                           (cl:null (cddar input))
                           (symbolp (cadar input)))
                      (setf name (cadar input)
                            keyword (caar input))
                      (setf correct-syntax-p nil)))
                 (t
                  (setf correct-syntax-p nil)))
           (cond ((cl:null (cdr input))
                  (setf form nil)
                  (setf supplied-p (gensym)))
                 ((cl:atom (cdr input))
                  (setf correct-syntax-p nil))
                 (t
                  (setf form (cadr input))
                  (cond ((cl:null (cddr input))
                         (setf supplied-p (gensym)))
                        ((cl:atom (cddr input))
                         (setf correct-syntax-p nil))
                        (t
                         (if (symbolp (caddr input))
                             (setf supplied-p  (caddr input))
                             (setf correct-syntax-p nil)))))))
          (t
           (setf correct-syntax-p nil)))
    (if correct-syntax-p
        (cl:list (advance-dot-position
                  item
                  (make-instance 'ordinary-key-parameter
                    :name name
                    :keyword keyword
                    :form form
                    :supplied-p supplied-p)))
        '())))

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

(defmethod scanner-action
    (client item lambda-list (terminal generic-function-key-parameter) input)
  (let ((allowed-keywords (allowed-lambda-list-keywords client lambda-list))
        (correct-syntax-p t)
        name keyword)
    (cond ((and (symbolp input) (not (member input allowed-keywords)))
           (setf name input)
           (setf keyword (intern (symbol-name input) '#:keyword)))
          ((cl:consp input)
           (cond ((symbolp (car input))
                  (setf name (car input))
                  (setf keyword (intern (symbol-name (car input)) '#:keyword)))
                 ((cl:consp (car input))
                  (if (and (symbolp (caar input))
                           (cl:consp (cdar input))
                           (cl:null (cddar input))
                           (symbolp (cadar input)))
                      (setf name (cadar input)
                            keyword (caar input))
                      (setf correct-syntax-p nil)))
                 (t
                  (setf correct-syntax-p nil)))
           (unless (cl:null (cdr input))
             (setf correct-syntax-p nil)))
          (t
           (setf correct-syntax-p nil)))
    (if correct-syntax-p
        (cl:list (advance-dot-position
                  item
                  (make-instance 'generic-function-key-parameter
                    :name name
                    :keyword keyword)))
        '())))

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
(define-keyword-scanner-action keyword-allow-other-keys &allow-other-keys)
(define-keyword-scanner-action keyword-aux &aux)
(define-keyword-scanner-action keyword-environment &environment)
(define-keyword-scanner-action keyword-whole &whole)
