(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defun parse-ordinary-required-parameter (parameter)
  (make-instance 'cst::ordinary-required-parameter :name parameter))

(defun parse-ordinary-required-parameter-group (remaining-input)
  (loop for input = remaining-input then (cdr input)
        until (or (null input)
                  (member (car input) lambda-list-keywords :test #'eq))
        collect (parse-ordinary-required-parameter (car input))))

(defun parse-ordinary-optional-parameter (parameter)
  (cond ((symbolp parameter)
         (make-instance 'cst::ordinary-optional-parameter
           :name parameter
           :form nil
           :supplied-p (gensym)))
        ((null (cdr parameter))
         (make-instance 'cst::ordinary-optional-parameter
           :name (car parameter)
           :form nil
           :supplied-p (gensym)))
        ((null (cddr parameter))
         (make-instance 'cst::ordinary-optional-parameter
           :name (car parameter)
           :form (cadr parameter)
           :supplied-p (gensym)))
        (t
         (make-instance 'cst::ordinary-optional-parameter
           :name (car parameter)
           :form (cadr parameter)
           :supplied-p (caddr parameter)))))

(defun parse-ordinary-optional-parameter-group (remaining-input)
  (loop for input = remaining-input then (cdr input)
        until (or (null input)
                  (member (car input) lambda-list-keywords :test #'eq))
        collect (parse-ordinary-optional-parameter (car input))))

(defun parse-ordinary-key-parameter (parameter)
  (cond ((symbolp parameter)
         (make-instance 'cst::ordinary-key-parameter
           :name parameter
           :form nil
           :keyword (intern (symbol-name parameter) :keyword)
           :supplied-p (gensym)))
        ((null (cdr parameter))
         (if (symbolp (car parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (car parameter)
               :form nil
               :keyword (intern (symbol-name (car parameter)) :keyword)
               :supplied-p (gensym))
             (make-instance 'cst::ordinary-key-parameter
               :name (caar parameter)
               :form nil
               :keyword (cadar parameter)
               :supplied-p (gensym))))
        ((null (cddr parameter))
         (if (symbolp (car parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (car parameter)
               :form (cadr parameter)
               :keyword (intern (symbol-name (car parameter)) :keyword)
               :supplied-p (gensym))
             (make-instance 'cst::ordinary-key-parameter
               :name (caar parameter)
               :form (cadr parameter)
               :keyword (cadar parameter)
               :supplied-p (gensym))))
        (t
         (if (symbolp (car parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (car parameter)
               :form (cadr parameter)
               :keyword (intern (symbol-name (car parameter)) :keyword)
               :supplied-p (caddr parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (caar parameter)
               :form (cadr parameter)
               :keyword (cadar parameter)
               :supplied-p (caddr parameter))))))

(defun parse-ordinary-key-parameter-group (remaining-input)
  (loop for input = remaining-input then (cdr input)
        until (or (null input)
                  (member (car input) lambda-list-keywords :test #'eq))
        collect (parse-ordinary-key-parameter (car input))))

(defun position-of-first-keyword (lambda-list)
  (position-if (lambda (element)
                 (member element lambda-list-keywords))
               lambda-list))

(defun split-lambda-list (lambda-list)
  (let ((remaining lambda-list)
        (result '()))
    (let ((pos (position-of-first-keyword lambda-list)))
      (if (null pos)
          (list lambda-list)
          (progn (push (subseq lambda-list 0 pos) result)
                 (setf remaining (subseq lambda-list pos))
                 (setf pos 0)
                 (loop for pos = (position-of-first-keyword (cdr remaining))
                       until (null pos)
                       do (push (subseq remaining 0 (1+ pos)) result)
                          (setf remaining (subseq remaining (1+ pos)))
                       finally (push remaining result)
                               (return (reverse result))))))))

(defun parse-ordinary-lambda-list (lambda-list)
  (let ((groups (split-lambda-list lambda-list))
        (result '()))
    (push (make-instance 'cst::ordinary-required-parameter-group
            :parameters (mapcar #'parse-ordinary-required-parameter
                         (car groups)))
          result)
    (pop groups)
    (when (and (not (null groups)) (eq (caar groups) '&optional))
      (push (make-instance 'cst::ordinary-optional-parameter-group
              :keyword (caar groups)
              :parameters (mapcar #'parse-ordinary-optional-parameter
                           (cdar groups)))
            result)
      (pop groups))
    (when (and (not (null groups)) (eq (caar groups) '&rest))
      (push (make-instance 'cst::ordinary-rest-parameter-group
              :keyword (caar groups)
              :parameter (cadar groups))
            result)
      (pop groups))
    (when (and (not (null groups)) (eq (caar groups) '&key))
      (push (make-instance 'cst::ordinary-key-parameter-group
              :keyword (caar groups)
              :parameters (mapcar #'parse-ordinary-key-parameter
                           (cdar groups)))
            result)
      (pop groups))
    (make-instance 'cst::ordinary-lambda-list
      :children (reverse result))))
