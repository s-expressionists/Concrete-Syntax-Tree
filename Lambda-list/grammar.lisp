(cl:in-package #:concrete-syntax-tree)

(defclass grammar ()
  ((%rules :initarg :rules :reader rules)
   (%nullable-symbols :reader nullable-symbols)))
