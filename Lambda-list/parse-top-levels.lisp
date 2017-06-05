(cl:in-package #:concrete-syntax-tree)

(defun find-final-item (parser)
  (let ((initial-state (car (all-states parser)))
        (final-state (car (cl:last (all-states parser)))))
    (find-if (lambda (item)
               (let* ((rule (rule item))
                      (len (length (right-hand-side rule)))
                      (pos (dot-position item)))
                 (and (eq (left-hand-side (rule item))
                          'target)
                      (= pos len)
                      (eq (origin item) initial-state))))
             (items final-state))))

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
