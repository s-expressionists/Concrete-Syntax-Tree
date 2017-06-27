(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defun assert-success (parser)
  (let ((item (cst::find-final-item parser)))
    (assert (not (null item)))
    (car (cst::parse-trees item))))

(defun test-ordinary (lambda-list)
  (let* ((p (make-instance 'cst:parser
              :rules cst:*ordinary-lambda-list-grammar*
              :input (cst:cst-from-expression lambda-list)
              :lambda-list (make-instance 'cst:ordinary-lambda-list)
              :client nil)))
    (cst:parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees result (parse-ordinary-lambda-list lambda-list)))))

(defun test-generic-function (lambda-list)
  (let* ((p (make-instance 'cst:parser
              :rules cst:*generic-function-lambda-list-grammar*
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
              :rules cst:*specialized-lambda-list-grammar*
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
              :rules cst:*defsetf-lambda-list-grammar*
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
              :rules cst:*define-modify-macro-lambda-list-grammar*
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
              :rules cst:*define-method-combination-lambda-list-grammar*
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
              :rules cst:*destructuring-lambda-list-grammar*
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
              :rules cst:*macro-lambda-list-grammar*
              :input (cst:cst-from-expression lambda-list)
              :lambda-list
              (make-instance 'cst:macro-lambda-list)
              :client nil)))
    (cst:parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees
       result (parse-macro-lambda-list lambda-list)))))

(defun test-ordinary-lambda-lists ()
  (loop repeat 10000
        do (assert (test-ordinary (random-ordinary-lambda-list)))))

(defun test-generic-function-lambda-lists ()
  (loop repeat 10000
        do (assert (test-generic-function (random-generic-function-lambda-list)))))

(defun test-specialized-lambda-lists ()
  (loop repeat 10000
        do (assert (test-specialized (random-specialized-lambda-list)))))

(defun test-defsetf-lambda-lists ()
  (loop repeat 10000
        do (assert (test-defsetf (random-defsetf-lambda-list)))))

(defun test-define-modify-macro-lambda-lists ()
  (loop repeat 10000
        do (assert (test-define-modify-macro
                    (random-define-modify-macro-lambda-list)))))

(defun test-define-method-combination-lambda-lists ()
  (loop repeat 10000
        do (assert (test-define-method-combination
                    (random-define-method-combination-lambda-list)))))

(defun test-destructuring-lambda-lists ()
  (loop repeat 10000
        do (assert (test-destructuring (random-destructuring-lambda-list)))))

(defun test-macro-lambda-lists ()
  (loop repeat 10000
        do (assert (test-macro (random-macro-lambda-list)))))

(defun test ()
  (test-ordinary-lambda-lists)
  (test-generic-function-lambda-lists)
  (test-specialized-lambda-lists)
  (test-defsetf-lambda-lists)
  (test-define-modify-macro-lambda-lists)
  (test-define-method-combination-lambda-lists)
  (test-destructuring-lambda-lists)
  (test-macro-lambda-lists))
