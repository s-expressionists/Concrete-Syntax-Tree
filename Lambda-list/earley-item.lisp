(cl:in-package #:concrete-syntax-tree)

(defclass earley-item ()
  ((%rule :initarg :rule :reader rule)
   (%dot-position :initarg :dot-position :reader dot-position)
   (%origin :initarg :origin :reader origin)
   (%parse-trees :initarg :parse-trees :reader parse-trees)))

(defmethod print-object ((object earley-item) stream)
  (let ((rule (rule object))
        (pos (dot-position object)))
    (print-unreadable-object (rule stream :type t)
      (format stream "~s <- " (left-hand-side rule))
      (loop for symbol in (right-hand-side rule)
            for i from 0
            do (when (= pos i)
                 (format stream " . "))
               (format stream "~s " symbol))
      (when (= pos (length (right-hand-side rule)))
        (format stream " . "))
      (terpri stream))))

(defgeneric item-equal (item1 item2))

(defmethod item-equal ((item1 earley-item) (item2 earley-item))
  (and (eq (rule item1) (rule item2))
       (eq (dot-position item1) (dot-position item2))
       (eq (origin item1) (origin item2))))
