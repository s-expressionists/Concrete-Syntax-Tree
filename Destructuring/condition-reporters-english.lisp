(cl:in-package #:concrete-syntax-tree)

(defmethod acclimation:report-condition
    ((condition null-structure-mismatch-error) stream (language acclimation:english))
  (format stream "~@<Could not destructure ~S according to ~A because ~
                  ~S is not null.~@:>"
          (raw (whole-cst condition)) (pattern condition)
          (cst condition)))

(defmethod acclimation:report-condition
    ((condition cons-structure-mismatch-error) stream (language acclimation:english))
  (format stream "~@<Could not destructure ~S according to ~A because ~
                  ~S is not a ~A.~@:>"
          (raw (whole-cst condition)) (pattern condition)
          (cst condition) 'cons-cst))
