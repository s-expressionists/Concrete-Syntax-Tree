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

(defun extract-symbols (right-hand-side-element)
  (if (symbolp right-hand-side-element)
      (list right-hand-side-element)
      (loop for element in (cdr right-hand-side-element)
            append (extract-symbols element))))

(defclass grammar ()
  ((%rules :initarg :rules :reader rules)
   (%nullable-symbols :initarg :nullable-symbols :reader nullable-symbols)))

(defun compute-all-symbols (grammar)
  (let ((symbols (make-hash-table :test #'eq)))
    (loop for rule in (rules grammar)
          do (setf (gethash (left-hand-side rule) symbols) t)
             (loop for element in (right-hand-side rule)
                   do (loop for symbol in (extract-symbols element)
                            do (setf (gethash symbol symbols) t))))
    symbols))

(defun nullable-p (right-hand-side-element)
  (and (cl:consp right-hand-side-element)
       (member (car right-hand-side-element) '(? *) :test #'eq)))

(defun compute-nullable-symbols (rules)
  (let ((nullable-symbols (make-hash-table :test #'eq)))
    (loop with modified-p = t
          while modified-p
          do (setf modified-p nil)
             (loop for rule in rules
                   do (unless (gethash (left-hand-side rule) nullable-symbols)
                        (when (every (lambda (x)
                                       (nullable-p x nullable-symbols))
                                     (right-hand-side rule))
                          (setf (gethash (left-hand-side rule) nullable-symbols)
                                t)
                          (setf modified-p t)))))
    nullable-symbols))

(defmethod initialize-instance :after ((object grammar) &key rules)
  (let ((new-rules (loop for rule in rules
                         collect (make-instance 'rule
                                   :left-hand-side (car rule)
                                   :right-hand-side (cddr rule)))))
    (reinitialize-instance
     object
     :rules new-rules
     :nullable-symbols (compute-nullable-symbols new-rules))))
