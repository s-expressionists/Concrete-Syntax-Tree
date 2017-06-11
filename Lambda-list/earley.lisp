(cl:in-package #:concrete-syntax-tree)

(defgeneric completer-action (symbol grammar origin state))

;; (defmethod completer-action (symbol origin state)
;;   (declare (ignore symbol state origin))
;;   nil)

(defmethod completer-action
    ((symbol grammar-symbol)
     (grammar grammar)
     (origin earley-state)
     (state earley-state))
  (loop for item in (items origin)
        for rule = (rule item)
        for length = (length (right-hand-side rule))
        for dot-position = (dot-position item)
        when (and (< dot-position length)
                  (let* ((element (elt (right-hand-side rule) dot-position))
                         (type (if (cl:consp element) (cadr element) element)))
                    (typep symbol type)))
          do (loop for i from (1+ dot-position)
                   do (let ((new (make-instance 'earley-item
                                   :rule (rule item)
                                   :dot-position i
                                   :origin (origin item)
                                   :parse-trees
                                   (cl:cons symbol (parse-trees item)))))
                        (possibly-add-item new state))
                   while (and (< i (length (right-hand-side rule)))
                              (nullable-p (elt (right-hand-side rule) i))))))

(defgeneric predictor-action (symbol grammar state))

(defmethod predictor-action
    ((symbol grammar-symbol) (grammar grammar) (state earley-state))
  (loop for rule in (rules grammar)
        when (typep symbol (left-hand-side rule))
          do (loop for i from 0
                   until (= i (length (right-hand-side rule)))
                   while (nullable-p (elt (right-hand-side rule) i))
                   do  (let ((new (make-instance 'earley-item
                                    :rule rule
                                    :dot-position i
                                    :origin state
                                    :parse-trees '())))
                         (possibly-add-item new state))
                   finally (let ((new (make-instance 'earley-item
                                        :rule rule
                                        :dot-position i
                                        :origin state
                                        :parse-trees '())))
                             (possibly-add-item new state)))))

(defun all-items (rule origin parse-trees dot-position)
  (loop with right-hand-side = (right-hand-side rule)
        with length = (length right-hand-side)
        for i from dot-position
        collect (make-instance 'earley-item
                  :rule rule
                  :origin origin
                  :parse-trees parse-trees
                  :dot-position i)
        while (and (< i length) (nullable-p (elt right-hand-side i)))))

(defgeneric process-current-state (parser))

(defmethod process-current-state ((parser parser))
  (let ((states (remaining-states parser))
        (client (client parser))
        (lambda-list (lambda-list parser))
        (remaining-input (remaining-input parser)))
    (loop with grammar = (grammar parser)
          with state = (car states)
          for remaining-items = (items state) then (cdr remaining-items)
          until (cl:null remaining-items)
          do (let* ((item (car remaining-items))
                    (pos (dot-position item))
                    (rule (rule item))
                    (lhs (left-hand-side rule))
                    (rhs (right-hand-side rule)))
               (if (= pos (length rhs))
                   (let* ((lhs-class (find-class lhs))
                          (proto (make-instance lhs-class
                                   :children (reverse (parse-trees item)))))
                     (completer-action proto grammar (origin item) state))
                   (let* ((terminal (cl:nth pos rhs))
                          (terminal-class
                            (find-class (if (cl:consp terminal)
                                            (cadr terminal)
                                            terminal)))
                          (proto (make-instance terminal-class))
                          (scan-result
                            (if (cl:null remaining-input)
                                nil
                                (scanner-action client
                                                item
                                                lambda-list
                                                (if (cl:consp terminal)
                                                    terminal
                                                    proto)
                                                (car remaining-input)))))
                     (loop with next-state = (cadr states)
                           for item in scan-result
                           for items = (cl:cons item
                                                (all-items (rule item)
                                                           (origin item)
                                                           (parse-trees item)
                                                           (dot-position item)))
                           do (loop for item in items
                                    do (possibly-add-item item next-state)))
                     (predictor-action proto grammar state)))))))

(defgeneric parse-step (parser))

(defmethod parse-step ((parser parser))
  (process-current-state parser)
  (cl:pop (remaining-input parser))
  (cl:pop (remaining-states parser)))

(defgeneric parse (parser))

(defmethod parse ((parser parser))
  (loop repeat (1+ (length (all-input parser)))
        do (parse-step parser)))
