(cl:in-package #:concrete-syntax-tree)

(defclass client () ())

(defclass sbcl (client) ())

(defclass sicl (client) ())

(defclass clasp (client) ())

(defclass ecl (client) ())

(defclass ccl (client) ())
