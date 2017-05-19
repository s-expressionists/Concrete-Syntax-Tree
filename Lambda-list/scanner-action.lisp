(cl:in-package #:concrete-syntax-tree)

(defgeneric scanner-action
    (client item lambda-list terminal input))

(defmethod scanner-action
    (client item lambda-list (terminal ordinary-required-parameter) input)
  (if (symbolp input)
      (make-instance 'earley-item
        :rule (rule item)
        :origin (origin item)
        :parse-trees (cl:cons (make-instance 'ordinary-required-parameter
                                :parse-tree input)
                              (parse-trees item))
        :dot-position (1+ (dot-position item)))
      nil))
