(cl:in-package #:concrete-syntax-tree)

(defgeneric scanner-action
    (client item lambda-list terminal input))

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
        (advance-dot-position
         item
         (make-instance 'ordinary-required-parameter
           :parse-tree input))
        nil)))

(defmethod scanner-action
    (client item lambda-list (terminal keyword-optional) input)
  (declare (ignore client lambda-list))
  (if (eq input '&optional)
      (advance-dot-position
       item
       (make-instance 'keyword-optional
         :parse-tree input))
      nil))
