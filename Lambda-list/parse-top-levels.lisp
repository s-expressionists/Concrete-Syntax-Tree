(cl:in-package #:concrete-syntax-tree)

(defun parse-top-level (client rules class lambda-list)
  (let ((p (make-instance 'cst::parser
             :rules rules
             :input lambda-list
             :lambda-list (make-instance class)
             :client client)))
    (parse p)
    (let ((item (find-final-item p)))
      (if (cl:null item)
          (error "Parse failed")
          (car (parse-trees item))))))
  
(defun parse-ordinary-lambda-list (client lambda-list)
  (parse-top-level client
                   *ordinary-lambda-list-grammar*
                   'ordinary-lambda-list
                   lambda-list))

(defun parse-generic-function-lambda-list (client lambda-list)
  (parse-top-level client
                   *generic-function-lambda-list-grammar*
                   'generic-function-lambda-list
                   lambda-list))

(defun parse-specialized-lambda-list (client lambda-list)
  (parse-top-level client
                   *specialized-lambda-list-grammar*
                   'specialized-lambda-list
                   lambda-list))

(defun parse-defsetf-lambda-list (client lambda-list)
  (parse-top-level client
                   *defsetf-lambda-list-grammar*
                   'defsetf-lambda-list
                   lambda-list))

(defun parse-define-modify-macro-lambda-list (client lambda-list)
  (parse-top-level client
                   *define-modify-macro-lambda-list-grammar*
                   'define-modify-macro-lambda-list
                   lambda-list))

(defun parse-define-method-combination-lambda-list (client lambda-list)
  (parse-top-level client
                   *define-method-combination-lambda-list-grammar*
                   'define-method-combination-lambda-list
                   lambda-list))

(defun parse-destructuring-lambda-list (client lambda-list)
  (parse-top-level client
                   *destructuring-lambda-list-grammar*
                   'destructuring-lambda-list
                   lambda-list))

(defun parse-macro-lambda-list (client lambda-list)
  (parse-top-level client
                   *macro-lambda-list-grammar*
                   'macro-lambda-list
                   lambda-list))
