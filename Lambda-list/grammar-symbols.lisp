(cl:in-package #:concrete-syntax-tree)

(defclass grammar-symbol ()
  ((%parse-tree :initarg :parse-tree :reader parse-tree)))

(defclass ordinary-required-parameter (grammar-symbol) ())
