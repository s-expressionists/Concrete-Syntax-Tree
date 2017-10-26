(cl:in-package #:concrete-syntax-tree)

(defclass cons-cst (cst)
  (;; This slot contains a CST that represents the CAR of the
   ;; corresponding expression.
   (%first :initform nil :initarg :first :reader first)
   ;; This slot contains a CST that represents the CDR of the
   ;; corresponding expression.
   (%rest :initform nil :initarg :rest :reader rest)))

(defmethod consp ((cst cons-cst))
  (declare (ignorable cst))
  t)

(defgeneric cons (first rest &key source))

(defun raw-or-nil (cst)
  (raw cst))

(defmethod cons (first rest &key source)
  (let ((result (make-instance 'cons-cst
                  :raw (cl:cons (raw-or-nil first) (raw-or-nil rest))
                  :source source
                  :first first
                  :rest rest)))
    (setf (parent first) result)
    (setf (parent rest) result)
    result))

(defun list (&rest csts)
  (loop for result = (make-instance 'atom-cst :raw nil) then (cons cst result)
        for cst in (reverse csts)
        finally (return result)))

(defgeneric nthrest (n cst)
  (:method (n (cst cons-cst))
    (loop for tail = cst then (rest tail)
          repeat n
          finally (return tail))))

(defgeneric nth (n cst)
  (:method (n (cst cons-cst))
    (first (nthrest n cst))))

(defmethod second ((cst cons-cst))
  (nth 1 cst))

(defmethod third ((cst cons-cst))
  (nth 2 cst))

(defmethod fourth ((cst cons-cst))
  (nth 3 cst))

(defmethod fifth ((cst cons-cst))
  (nth 4 cst))

(defmethod sixth ((cst cons-cst))
  (nth 5 cst))

(defmethod seventh ((cst cons-cst))
  (nth 6 cst))

(defmethod eighth ((cst cons-cst))
  (nth 7 cst))

(defmethod ninth ((cst cons-cst))
  (nth 8 cst))

(defmethod tenth ((cst cons-cst))
  (nth 9 cst))
