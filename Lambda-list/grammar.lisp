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

(defun extract-symbols (right-hand-side-element)
  (if (symbolp right-hand-side-element)
      (list right-hand-side-element)
      (loop for element in (cdr right-hand-side-element)
            append (extract-symbols element))))

(defun compute-all-symbols (grammar)
  (let ((non-nullable (make-hash-table :test #'eq)))
    (loop for rule in (rules grammar)
          do (setf (gethash (left-hand-side rule) non-nullable) t)
             (loop for element in (right-hand-side rule)
                   do (loop for symbol in (extract-symbols element)
                            do (setf (gethash symbol non-nullable) t))))
    non-nullable))
