(cl:in-package #:concrete-syntax-tree)

(defclass grammar ()
  ((%rules :reader rules)
   (%nullable-symbols :reader nullable-symbols)))
