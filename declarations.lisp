(cl:in-package #:concrete-syntax-tree)

(defgeneric canonicalize-declaration-specifier
    (system
     declaration-identifier
     declaration-identifier-cst
     declaration-data))
