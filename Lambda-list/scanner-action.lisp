(cl:in-package #:concrete-syntax-tree)

(defgeneric scanner-action
    (client item lambda-list terminal input))

(defun advance-dot-position (item parse-tree)
  (make-instance 'earley-item
    :rule (rule item)
    :origin (origin item)
    :parse-trees (cl:cons parse-tree (parse-trees item))))
    :dot-position (1+ (dot-position item)))

(defmethod scanner-action
    (client item lambda-list (terminal ordinary-required-parameter) input)
  (let ((allowed-keywords (allowed-lambda-list-keywords client lambda-list)))
    (if (and (symbolp input) (not (member input allowed-keywords)))
        (make-instance 'earley-item
          :rule (rule item)
          :origin (origin item)
          :parse-trees (cl:cons (make-instance 'ordinary-required-parameter
                                  :parse-tree input)
                        (parse-trees item))
          :dot-position (1+ (dot-position item)))
        nil)))
