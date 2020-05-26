(cl:in-package #:concrete-syntax-tree)

(defclass rule ()
  ((&left-hand-side :initarg :left-hand-side :reader left-hand-side)
   (&right-hand-side :initarg :right-hand-side :reader right-hand-side)))

(defmethod print-object ((object rule) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s <- " (left-hand-side object))
    (loop for symbol in (right-hand-side object)
          do (format stream "~s " symbol))
    (terpri stream)))

(defclass grammar ()
  ((%target-rule :initarg :target-rule :reader target-rule)
   (%rules :initarg :rules :reader rules)))

(defun nullable-p (right-hand-side-element)
  (and (cl:consp right-hand-side-element)
       (member (car right-hand-side-element) '(? *) :test #'eq)))

(defun generate-grammar (target-rule rules)
  (make-instance 'grammar
                 :target-rule (make-instance 'rule
                                             :left-hand-side 'target
                                             :right-hand-side (cl:list target-rule))
                 :rules (mapcar (lambda (rule)
                                  (make-instance 'rule
                                                 :left-hand-side (car rule)
                                                 :right-hand-side (cddr rule)))
                                rules)))
