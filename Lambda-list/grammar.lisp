(cl:in-package #:concrete-syntax-tree)

(defclass grammar ()
  ((%rules :initarg :rules :reader rules)
   (%nullable-symbols :reader nullable-symbols)))

(defclass rule ()
  ((&left-hand-slide :initarg :left-hand-side :reader left-hand-side)
   (&right-hand-slide :initarg :right-hand-side :reader right-hand-side)))

(defmethod initialize-instance :after ((object grammar) &key rules)
  (reinitialize-instance
   object
   :rules (loop for rule in rules
                collect (make-instance 'rule
                          :left-hand-side (car rule)
                          :right-hand-side (cddr rule)))))
