(cl:in-package #:concrete-syntax-tree)

(defclass rule ()
  ((%left-hand-side :initarg :left-hand-side :reader left-hand-side)
   (%right-hand-side :initarg :right-hand-side :reader right-hand-side)))

(defclass earley-item ()
  ((%rule :initarg :rule :reader rule)
   (%dot-position :initarg :dot-position :reader dot-position)
   (%origin :initarg :origin :reader origin)
   (%parse-trees :initarg :parse-trees :reader parse-trees)))

(defgeneric item-equal (item1 item2))

(defmethod item-equal ((item1 earley-item) (item2 earley-item))
  (and (eq (rule item1) (rule item2))
       (eq (dot-position item1) (dot-position item2))
       (eq (origin item1) (origin item2))))

(defclass earley-state ()
  ((%items :initform '() :accessor items)))

(defgeneric possibly-add-item (item state))

(defmethod possibly-add-item ((item earley-item) (state earley-state))
  (unless (find item (items state) :test #'item-equal)
    (setf (items state)
          (nconc (items state) (list item)))))

(defgeneric scanner-action
    (client item lambda-list terminal input))

(defclass grammar-symbol ()
  ((%parse-tree :initarg :parse-tree :reader parse-tree)))

(defclass simple-variable (grammar-symbol) ())

(defmethod scanner-action
    (client item lambda-list (terminal simple-variable) input)
  (if (symbolp input)
      (make-instance 'earley-item
        :rule (rule item)
        :parse-trees (cons (parse-trees item)
                           (make-instance 'simple-variable
                             :parse-tree input))
        :dot-position (1+ (dot-position item)))
      nil))

(defgeneric completer-action (symbol origin state))

(defmethod completer-action
    ((symbol grammar-symbol) (origin earley-state) (state earley-state))
  (loop for item in (items origin)
        when (subtypep symbol (left-hand-side item))
          do (let ((new (make-instance 'earley-item
                          :rule (rule item)
                          :dot-position (1+ (dot-position item))
                          :origin (origin item)
                          :parse-trees (cons symbol (parse-trees item)))))
               (possibly-add-item new state))))

(defgeneric predictor-action (symbol grammar state))

(defmethod predictor-action
    ((symbol grammar-symbol) (grammar grammar) (state earley-state))
  (loop for rule in (rules grammar)
        when (subtypep (left-hand-side rule) symbol)
          do (let ((new (make-instance 'earley-item
                          :rule rule
                          :dot-position 0
                          :origin state
                          :parse-trees '())))
               (possibly-add-item new state))))

(defclass parser ()
  ((%client :initarg :states :reader client)
   (%lambda-list :initarg :lambda-list :reader lambda-list)
   (%grammar :initarg :states :reader grammar)
   (%all-states :initarg :states :reader all-states)
   (%all-input :initarg :input :reader all-input)
   (%remaining-states :initarg :states :reader remaining-states)
   (%remaining-input :initarg :input :reader remaining-input)))
