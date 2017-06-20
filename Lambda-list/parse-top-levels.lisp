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
  
(defmacro define-top-level-parser (name grammar type)
  `(defun ,name (client lambda-list)
     (parse-top-level client ,grammar ',type lambda-list)))

(define-top-level-parser parse-ordinary-lambda-list
  *ordinary-lambda-list-grammar*
  ordinary-lambda-list)

(define-top-level-parser parse-generic-function-lambda-list
  *generic-function-lambda-list-grammar*
  generic-function-lambda-list)

(define-top-level-parser parse-specialized-lambda-list
  *specialized-lambda-list-grammar*
  specialized-lambda-list)

(define-top-level-parser parse-defsetf-lambda-list
  *defsetf-lambda-list-grammar*
  defsetf-lambda-list)

(define-top-level-parser parse-define-modify-macro-lambda-list
  *define-modify-macro-lambda-list-grammar*
  define-modify-macro-lambda-list)

(define-top-level-parser parse-define-method-combination-lambda-list
  *define-method-combination-lambda-list-grammar*
  define-method-combination-lambda-list)

(define-top-level-parser parse-destructuring-lambda-list
  *destructuring-lambda-list-grammar*
  destructuring-lambda-list)

(define-top-level-parser parse-macro-lambda-list
  *macro-lambda-list-grammar*
  macro-lambda-list)
