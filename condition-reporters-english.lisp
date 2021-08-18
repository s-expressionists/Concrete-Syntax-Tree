(cl:in-package #:concrete-syntax-tree)

(defmethod acclimation:report-condition
    ((condition null-cst-required) stream (language acclimation:english))
  (format stream "~@<Encountered ~S where a CST satisfying ~A is ~
                  required.~@:>"
          (cst condition) 'null))

(defmethod acclimation:report-condition
    ((condition cons-cst-required) stream (language acclimation:english))
  (format stream "~@<Encountered ~S where a ~A is required.~@:>"
          (cst condition) 'cons-cst))

(defmethod acclimation:report-condition
    ((condition unquote-splicing-in-dotted-list) stream
     (language acclimation:english))
  (format stream "Splicing unquote at end of list (like a . ,@b)."))

(defmethod acclimation:report-condition
    ((condition unquote-splicing-at-top) stream (language acclimation:english))
  (format stream "Splicing unquote as quasiquotation form (like `,@foo)."))
