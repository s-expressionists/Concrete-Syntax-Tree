(cl:in-package #:concrete-syntax-tree)

(defclass rule ()
  ((%left-hand-side :initarg :left-hand-side :reader left-hand-side)
   (%right-hand-side :initarg :right-hand-side :reader right-hand-side)))

(defclass earley-item ()
  ((%rule :initarg :rule :reader rule)
   (%dot-position :initarg :dot-position :reader dot-position)
   (%origin :initarg :origin :reader origin)
   (%parse-trees :initarg :parse-trees :reader parse-trees)))

(defclass earley-state ()
  ((%items :initform '() :accessor items)))

(defgeneric scanner-action
    (client item lambda-list terminal input))

(defclass grammar-symbol ()
  ((%parse-tree :initarg :parse-tree :reader parse-tree)))

(defclass simple-variable (grammar-symbol) ())

(defmethod scanner-action
    (client item lambda-list (terminal simple-variable) input)
  (if (symbolp input)
      (make-instance 'earley-item
        :rule (rule item)
        :parse-trees (cons (parse-trees item)
                           (make-instance 'simple-variable
                             :parse-tree input))
        :dot-position (1+ (dot-position item)))
      nil))
