(cl:in-package #:concrete-syntax-tree)

(defclass earley-state ()
  ((%items :initform '() :accessor items)))

(defgeneric possibly-add-item (item state))

(defmethod possibly-add-item ((item earley-item) (state earley-state))
  (unless (find item (items state) :test #'item-equal)
    (setf (items state)
          (nconc (items state) (cl:list item)))))
