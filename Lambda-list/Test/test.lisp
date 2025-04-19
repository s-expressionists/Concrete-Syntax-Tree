(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(in-suite :concrete-syntax-tree-lambda-list)

(defun assert-success (parser)
  (let ((item (cst::find-final-item parser)))
    (is-true (not (null item)))
    (car (cst::parse-trees item))))

(defun test-ordinary (lambda-list)
  (let* ((p (make-instance 'cst:parser
              :grammar cst:*ordinary-lambda-list-grammar*
              :input (cst:cst-from-expression lambda-list)
              :lambda-list (make-instance 'cst:ordinary-lambda-list)
              :client nil)))
    (cst:parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees result (parse-ordinary-lambda-list lambda-list)))))

(defun test-generic-function (lambda-list)
  (let* ((p (make-instance 'cst:parser
              :grammar cst:*generic-function-lambda-list-grammar*
              :input (cst:cst-from-expression lambda-list)
              :lambda-list
              (make-instance 'cst:generic-function-lambda-list)
              :client nil)))
    (cst:parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees
       result (parse-generic-function-lambda-list lambda-list)))))

(defun test-specialized (lambda-list)
  (let* ((p (make-instance 'cst:parser
              :grammar cst:*specialized-lambda-list-grammar*
              :input (cst:cst-from-expression lambda-list)
              :lambda-list
              (make-instance 'cst:specialized-lambda-list)
              :client nil)))
    (cst:parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees
       result (parse-specialized-lambda-list lambda-list)))))

(defun test-defsetf (lambda-list)
  (let* ((p (make-instance 'cst:parser
              :grammar cst:*defsetf-lambda-list-grammar*
              :input (cst:cst-from-expression lambda-list)
              :lambda-list
              (make-instance 'cst:defsetf-lambda-list)
              :client nil)))
    (cst:parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees
       result (parse-defsetf-lambda-list lambda-list)))))

(defun test-define-modify-macro (lambda-list)
  (let* ((p (make-instance 'cst:parser
              :grammar cst:*define-modify-macro-lambda-list-grammar*
              :input (cst:cst-from-expression lambda-list)
              :lambda-list
              (make-instance 'cst:define-modify-macro-lambda-list)
              :client nil)))
    (cst:parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees
       result (parse-define-modify-macro-lambda-list lambda-list)))))

(defun test-define-method-combination (lambda-list)
  (let* ((p (make-instance 'cst:parser
              :grammar cst:*define-method-combination-lambda-list-grammar*
              :input (cst:cst-from-expression lambda-list)
              :lambda-list
              (make-instance 'cst:define-method-combination-lambda-list)
              :client nil)))
    (cst:parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees
       result (parse-define-method-combination-lambda-list lambda-list)))))

(defun test-destructuring (lambda-list)
  (let* ((p (make-instance 'cst:parser
              :grammar cst:*destructuring-lambda-list-grammar*
              :input (cst:cst-from-expression lambda-list)
              :lambda-list
              (make-instance 'cst:destructuring-lambda-list)
              :client nil)))
    (cst:parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees
       result (parse-destructuring-lambda-list lambda-list)))))

(defun test-macro (lambda-list)
  (let* ((p (make-instance 'cst:parser
              :grammar cst:*macro-lambda-list-grammar*
              :input (cst:cst-from-expression lambda-list)
              :lambda-list
              (make-instance 'cst:macro-lambda-list)
              :client nil)))
    (cst:parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees
       result (parse-macro-lambda-list lambda-list)))))

(macrolet ((define (name)
             (let ((test-name (intern (format nil "~A-LAMBDA-LIST" name) *package*))
                   (checker (intern (format nil "TEST-~A" name) *package*))
                   (generator (intern (format nil "RANDOM-~A-LAMBDA-LIST" name)
                                      *package*)))
               `(test ,test-name
                  (let ((fiveam:*test-dribble* nil)) ; too much output otherwise
                    (loop repeat 10000
                          do (assert (,checker (,generator)))))))))


  (define #:ordinary)
  (define #:generic-function)
  (define #:specialized)
  (define #:defsetf)
  (define #:define-modify-macro)
  (define #:define-method-combination)
  (define #:destructuring)
  (define #:macro))
