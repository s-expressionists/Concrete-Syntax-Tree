(cl:in-package #:concrete-syntax-tree)

(defgeneric canonicalize-declaration-specifier
    (system declaration-identifier declaration-identifier-cst declaration-data))

(defmethod  canonicalize-declaration-specifier
    (system declaration-identifier declaration-identifier-cst declaration-data)
  '())

(defmethod  canonicalize-declaration-specifier
    (system
     (declaration-identifier (eql 'optimize))
     declaration-identifier-cst
     declaration-data)
  (loop for data = declaration-data then (rest data)
        until (null data)
        collect (list declaration-identifier-cst data)))
