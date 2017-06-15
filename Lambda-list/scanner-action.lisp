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

;;; At the moment, the PUTATIVE-KEYWORD is just a Common Lisp
;;; S-expression.  Later it will be a CST instead.
(defun lambda-list-keyword-p (putative-keyword keyword)
  (eq putative-keyword keyword))

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

(defun make-ordinary-optional-parameter
    (name &key (form nil form-p) (supplied-p nil supplied-p-p))
  (make-instance 'ordinary-optional-parameter
    :name name
    :form (if form-p form nil)
    :supplied-p (if supplied-p-p supplied-p (gensym))))

;;; If PARAMETER can be parsed as an ordinary optional parameter, then
;;; return an instance of ORDINARY-OPTIONAL-PARAMETER with the various
;;; slots filled in from the contents of PARAMETER.  If not, return
;;; NIL to indicate that PARAMETER can not be parsed as an ordinary
;;; optional parameter.
(defun parse-ordinary-optional-parameter (parameter)
  (cond ((shapep parameter 'symbol)
         (make-ordinary-optional-parameter
          parameter))
        ((shapep parameter '(symbol))
         (make-ordinary-optional-parameter
          (path parameter '(0))))
        ((shapep parameter '(symbol t))
         (make-ordinary-optional-parameter
          (path parameter '(0))
          :form (path parameter '(1))))
        ((shapep parameter '(symbol t symbol))
         (make-ordinary-optional-parameter
          (path parameter '(0))
          :form (path parameter '(1))
          :supplied-p (path parameter '(2))))
        (t nil)))

(defun make-aux-parameter
    (name &key (form nil form-p))
  (make-instance 'aux-parameter
    :name name
    :form (if form-p form nil)))

(defun parse-aux-parameter (parameter)
  (cond ((shapep parameter 'symbol)
         (make-aux-parameter
           parameter))
        ((shapep parameter '(symbol))
         (make-aux-parameter
           (path parameter '(0))))
        ((shapep parameter '(symbol t))
         (make-aux-parameter
           (path parameter '(0))
           :form (path parameter '(1))))
        (t nil)))

(defmethod scanner-action
    (client item lambda-list (terminal simple-variable) input)
  (if (and (shapep input 'symbol)
           (not (allowed-keyword-p input client lambda-list)))
      (cl:list (advance-dot-position
                item
                (make-simple-variable input)))
      '()))

(defmethod scanner-action
    (client item lambda-list (terminal ordinary-optional-parameter) input)
  (if (allowed-keyword-p input client lambda-list)
      '()
      (let ((result (parse-ordinary-optional-parameter input)))
        (if (cl:null result)
            '()
            (cl:list (advance-dot-position item result))))))

(defmethod scanner-action
    (client item lambda-list (terminal aux-parameter) input)
  (if (allowed-keyword-p input client lambda-list)
      '()
      (let ((result (parse-aux-parameter input)))
        (if (cl:null result)
            '()
            (cl:list (advance-dot-position item result))))))

(defun make-ordinary-key-parameter (name &key
                                           (keyword nil keyword-p)
                                           (form nil form-p)
                                           (supplied-p nil supplied-p-p))
  (make-instance 'ordinary-key-parameter
    :name name
    :keyword (if keyword-p keyword (intern (symbol-name name) '#:keyword))
    :form (if form-p form nil)
    :supplied-p (if supplied-p-p supplied-p (gensym))))

(defun parse-ordinary-key-parameter (parameter)
  (cond ((shapep parameter 'symbol)
         (make-ordinary-key-parameter
           parameter))
        ((shapep parameter '(symbol))
         (make-ordinary-key-parameter
           (path parameter '(0))))
        ((shapep parameter '((symbol symbol)))
         (make-ordinary-key-parameter
           (path parameter '(0 1))
           :keyword (path parameter '(0 0))))
        ((shapep parameter '(symbol t))
         (make-ordinary-key-parameter
           (path parameter '(0))
           :form (path parameter '(1))
           :supplied-p (gensym)))
        ((shapep parameter '((symbol symbol) t))
         (make-ordinary-key-parameter
           (path parameter '(0 1))
           :form (path parameter '(1))
           :keyword (path parameter '(0 0))))
        ((shapep parameter '(symbol t symbol))
         (make-ordinary-key-parameter
           (path parameter '(0))
           :form (path parameter '(1))
           :supplied-p (path parameter '(2))))
        ((shapep parameter '((symbol symbol) t symbol))
         (make-ordinary-key-parameter
           (path parameter '(0 1))
           :form (path parameter '(1))
           :keyword (path parameter '(0 0))
           :supplied-p (path parameter '(2))))
        (t nil)))

(defmethod scanner-action
    (client item lambda-list (terminal ordinary-key-parameter) input)
  (if (allowed-keyword-p input client lambda-list)
      '()
      (let ((result (parse-ordinary-key-parameter input)))
        (if (cl:null result)
            '()
            (cl:list (advance-dot-position item result))))))

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

(defun make-generic-function-optional-parameter (name)
  (make-instance 'generic-function-optional-parameter
    :name name))

(defun parse-generic-function-optional-parameter (parameter)
  (cond ((shapep parameter 'symbol)
         (make-generic-function-optional-parameter parameter))
        ((shapep parameter '(symbol))
         (make-generic-function-optional-parameter (path parameter '(0))))
        (t nil)))

(defmethod scanner-action
    (client item lambda-list (terminal generic-function-optional-parameter) input)
  (if (allowed-keyword-p input client lambda-list)
      '()
      (let ((result (parse-generic-function-optional-parameter input)))
        (if (cl:null result)
            '()
            (cl:list (advance-dot-position item result))))))

(defun make-generic-function-key-parameter
    (name &key (keyword nil keyword-p))
  (make-instance 'generic-function-key-parameter
    :name name
    :keyword (if keyword-p keyword (intern (symbol-name name) '#:keyword))))

(defun parse-generic-function-key-parameter (parameter)
  (cond ((cst::shapep parameter 'symbol)
         (make-generic-function-key-parameter
          parameter))
        ((cst::shapep parameter '(symbol))
         (make-generic-function-key-parameter
          (path parameter '(0))))
        ((cst::shapep parameter '((symbol symbol)))
         (make-generic-function-key-parameter
          (path parameter '(0 1))
          :keyword (path parameter '(0 0))))
        (t nil)))

(defmethod scanner-action
    (client item lambda-list (terminal generic-function-key-parameter) input)
  (if (allowed-keyword-p input client lambda-list)
      '()
      (let ((result (parse-generic-function-key-parameter input)))
        (if (cl:null result)
            '()
            (cl:list (advance-dot-position item result))))))

(defun make-specialized-required-parameter
    (name &key (specializer nil specializer-p))
  (make-instance 'specialized-required-parameter
    :name name
    :specializer (if specializer-p specializer t)))

(defun parse-specialized-required-parameter (parameter)
  (cond ((shapep parameter 'symbol)
         (make-instance 'specialized-required-parameter
           :name parameter
           :specializer t))
        ((shapep parameter '(symbol))
         (make-instance 'specialized-required-parameter
           :name (path parameter '(0))
           :specializer t))
        ((shapep parameter '(symbol t))
         (make-instance 'specialized-required-parameter
           :name (path parameter '(0))
           :specializer (path parameter '(1))))
        (t nil)))

(defmethod scanner-action
    (client item lambda-list (terminal specialized-required-parameter) input)
  (if (allowed-keyword-p input client lambda-list)
      '()
      (let ((result (parse-specialized-required-parameter input)))
        (if (cl:null result)
            '()
            (cl:list (advance-dot-position item result))))))

(defmethod scanner-action
    (client item lambda-list (terminal destructuring-parameter) input)
  (cond ((and (shapep input 'symbol)
              (not (allowed-keyword-p input client lambda-list)))
         (cl:list (advance-dot-position
                   item
                   (make-simple-variable input))))
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
