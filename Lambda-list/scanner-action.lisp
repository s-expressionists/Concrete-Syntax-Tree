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

(defun allowed-keyword-p (symbol client lambda-list)
  (member symbol (allowed-lambda-list-keywords client lambda-list)))

;;; At the moment, PARAMETER is a symbol, so we return the parameter
;;; wrapped in an instance of SIMPLE-VARIABLE.  Later when PARAMETER
;;; will be a CST, we will change its class and return it.
(defun make-simple-variable (parameter)
  (make-instance 'simple-variable :name parameter))

(defmethod scanner-action
    (client item lambda-list (terminal simple-variable) input)
  (if (and (shapep input 'symbol)
           (not (allowed-keyword-p input client lambda-list)))
      (cl:list (advance-dot-position
                item
                (make-instance 'simple-variable
                  :name input)))
      '()))

(defmethod scanner-action
    (client item lambda-list (terminal ordinary-optional-parameter) input)
  (let ((correct-syntax-p t)
        name form supplied-p)
    (cond ((and (shapep input 'symbol)
                (not (allowed-keyword-p input client lambda-list)))
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
    (client item lambda-list (terminal aux-parameter) input)
  (let ((correct-syntax-p t)
        name form)
    (cond ((and (shapep input 'symbol)
                (not (allowed-keyword-p input client lambda-list)))
           (setf name input)
           (setf form nil))
          ((shapep input '(symbol))
           (setf name (path input '(0)))
           (setf form nil))
          ((shapep input '(symbol t))
           (setf name (path input '(0)))
           (setf form (path input '(1))))
          (t
           (setf correct-syntax-p nil)))
    (if correct-syntax-p
        (cl:list (advance-dot-position
                  item
                  (make-instance 'aux-parameter
                    :name name :form form)))
        '())))

(defmethod scanner-action
    (client item lambda-list (terminal ordinary-key-parameter) input)
  (let ((correct-syntax-p t)
        name keyword form supplied-p)
    (cond ((and (shapep input 'symbol)
                (not (allowed-keyword-p input client lambda-list)))
           (setf name input)
           (setf keyword (intern (symbol-name input) '#:keyword))
           (setf form nil)
           (setf supplied-p (gensym)))
          ((shapep input '(symbol))
           (setf name (path input '(0)))
           (setf keyword (intern (symbol-name name) '#:keyword))
           (setf form nil)
           (setf supplied-p (gensym)))
          ((shapep input '(symbol t))
           (setf name (path input '(0)))
           (setf keyword (intern (symbol-name name) '#:keyword))
           (setf form (path input '(1)))
           (setf supplied-p (gensym)))
          ((shapep input '(symbol t symbol))
           (setf name (path input '(0)))
           (setf keyword (intern (symbol-name name) '#:keyword))
           (setf form (path input '(1)))
           (setf supplied-p (path input '(2))))
          ((shapep input '((symbol symbol)))
           (setf name (path input '(0 1)))
           (setf keyword (path input '(0 0)))
           (setf form nil)
           (setf supplied-p (gensym)))
          ((shapep input '((symbol symbol) t))
           (setf name (path input '(0 1)))
           (setf keyword (path input '(0 0)))
           (setf form (path input '(1)))
           (setf supplied-p (gensym)))
          ((shapep input '((symbol symbol) t symbol))
           (setf name (path input '(0 1)))
           (setf keyword (path input '(0 0)))
           (setf form (path input '(1)))
           (setf supplied-p (path input '(2))))
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
  (cond ((and (symbolp input)
              (not (allowed-keyword-p input client lambda-list)))
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
         '())))

(defmethod scanner-action
    (client item lambda-list (terminal generic-function-key-parameter) input)
  (let ((correct-syntax-p t)
        name keyword)
    (cond ((and (symbolp input)
                (not (allowed-keyword-p input client lambda-list)))
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

(defmethod scanner-action
    (client item lambda-list (terminal specialized-required-parameter) input)
  (let ((correct-syntax-p t)
        name specializer)
    (cond ((and (shapep input 'symbol)
                (not (allowed-keyword-p input client lambda-list)))
           (setf name input)
           (setf specializer t))
          ((shapep input '(symbol))
           (setf name (path input '(0)))
           (setf specializer t))
          ((shapep input '(symbol t))
           (setf name (path input '(0)))
           (setf specializer (path input '(1))))
          (t
           (setf correct-syntax-p nil)))
    (if correct-syntax-p
        (cl:list (advance-dot-position
                  item
                  (make-instance 'specialized-required-parameter
                    :name name :specializer specializer)))
        '())))

(defmethod scanner-action
    (client item lambda-list (terminal destructuring-parameter) input)
  (cond ((and (shapep input 'symbol)
              (not (allowed-keyword-p input client lambda-list)))
         (cl:list (advance-dot-position
                   item
                   (make-instance 'simple-variable
                     :name input))))
        ((shapep input 'cl:cons)
         ;; FIXME: we should define a top-level parser that does not
         ;; call ERROR when parse fails and call it, rather than calling
         ;; PARSE-DESTRUCTURING-LAMBDA-LIST here.
         (let ((parse-tree (parse-destructuring-lambda-list client input)))
           (cl:list (advance-dot-position item parse-tree))))
        (t
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
