(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defun parse-simple-variable (parameter)
  (make-instance 'cst::simple-variable :name parameter))

(defun position-of-first-keyword (lambda-list)
  (position-if (lambda (element)
                 (member element lambda-list-keywords))
               lambda-list))

;;; Split a lambda list into groups according to role.  Each group is
;;; a list.  If the first element of the lambda-list is the
;;; lambda-list keyword &WHOLE, then the &WHOLE parameter group is
;;; processed first.  The required parameters are assumed to follow
;;; &WHOLE, or to be first on the list if there is no &WHOLE.  This
;;; assumption does not work for the macro lambda list, because in
;;; that list, there can be an &ENVIRONMENT parameter group preceding
;;; the required parameters.
(defun split-lambda-list (lambda-list)
  (let ((remaining lambda-list)
        (result '()))
    (when (and (not (null lambda-list))
               (cst::lambda-list-keyword-p
                (cst:cst-from-expression (car lambda-list))
                '&whole))
      (push (mapcar #'cst::cst-from-expression (subseq lambda-list 0 2))
            result)
      (setf lambda-list (cddr lambda-list)))
    (let ((pos (position-of-first-keyword lambda-list)))
      (if (null pos)
          (append (reverse result)
                  (list (mapcar #'cst::cst-from-expression lambda-list)))
          (progn (push (mapcar #'cst::cst-from-expression
                               (subseq lambda-list 0 pos))
                       result)
                 (setf remaining (subseq lambda-list pos))
                 (setf pos 0)
                 (loop for pos = (position-of-first-keyword (cdr remaining))
                       until (null pos)
                       do (push (mapcar #'cst::cst-from-expression
                                        (subseq remaining 0 (1+ pos)))
                                result)
                          (setf remaining (subseq remaining (1+ pos)))
                       finally (push (mapcar #'cst::cst-from-expression
                                             remaining)
                                     result)
                               (return (reverse result))))))))

;;; This function differs from the previous one in that if there is an
;;; &ENVIRONMENT parameter group first on list, or immediately
;;; following the &WHOLE parameter group, then the required parameters
;;; follow the &ENVIRONMENT parameter group.  Unfortunately, there is
;;; a case where the parse is ambiguous, and that is when the list of
;;; required parameters is empty.  In that case, two parses are
;;; possible, namely the empty required parameter group either
;;; precedes or follows the &ENVIRONMENT parameter group.  We "solve"
;;; this problem by only testing macro lambda lists with non-empty
;;; required parameter groups so that the parse is unambiguous.
(defun split-macro-lambda-list (lambda-list)
  (let ((remaining lambda-list)
        (result '()))
    (when (and (not (null lambda-list))
               (cst::lambda-list-keyword-p
                (cst:cst-from-expression (car lambda-list))
                '&whole))
      (push (mapcar #'cst::cst-from-expression (subseq lambda-list 0 2))
            result)
      (setf lambda-list (cddr lambda-list)))
    (when (and (not (null lambda-list))
               (cst::lambda-list-keyword-p
                (cst:cst-from-expression (car lambda-list))
                '&environment))
      (push (mapcar #'cst::cst-from-expression (subseq lambda-list 0 2))
            result)
      (setf lambda-list (cddr lambda-list)))
    (let ((pos (position-of-first-keyword lambda-list)))
      (if (null pos)
          (append (reverse result)
                  (list (mapcar #'cst::cst-from-expression lambda-list)))
          (progn (push (mapcar #'cst::cst-from-expression
                               (subseq lambda-list 0 pos))
                       result)
                 (setf remaining (subseq lambda-list pos))
                 (setf pos 0)
                 (loop for pos = (position-of-first-keyword (cdr remaining))
                       until (null pos)
                       do (push (mapcar #'cst::cst-from-expression
                                        (subseq remaining 0 (1+ pos)))
                                result)
                          (setf remaining (subseq remaining (1+ pos)))
                       finally (push (mapcar #'cst::cst-from-expression remaining)
                                     result)
                               (return (reverse result))))))))

(defmacro do-ordinary-required-parameter-group ()
  `(progn (push (make-instance 'cst::ordinary-required-parameter-group
                  :children (mapcar #'parse-simple-variable
                             (car groups)))
                result)
          (pop groups)))

(defmacro do-ordinary-optional-parameter-group ()
  `(when (and (not (null groups))
              (cst::lambda-list-keyword-p (caar groups) '&optional))
     (push (make-instance 'cst::ordinary-optional-parameter-group
             :children (cl:cons (make-instance 'cst::keyword-optional
                                  :name (caar groups))
                        (mapcar #'cst::parse-ordinary-optional-parameter
                                (cdar groups))))
           result)
     (pop groups)))

(defmacro do-ordinary-key-parameter-group ()
  `(when (and (not (null groups))
              (cst::lambda-list-keyword-p (caar groups) '&key))
     (let ((parameters (mapcar #'cst::parse-ordinary-key-parameter (cdar groups)))
           (keyword (make-instance 'cst::keyword-key :name (caar groups))))
       (push (make-instance 'cst::ordinary-key-parameter-group
               :children (append
                          (cl:list keyword)
                          parameters
                          (if (or (cl:null (cdr groups))
                                  (not (cst::lambda-list-keyword-p (caadr groups) '&allow-other-keys)))
                              '()
                              (prog1
                                  (cl:list
                                   (make-instance 'cst::keyword-allow-other-keys
                                     :name (caadr groups)))
                                (pop groups)))))
             result))
     (pop groups)))

(defmacro do-ordinary-rest-parameter-group ()
  `(when (and (not (null groups))
              (cst::lambda-list-keyword-p (caar groups) '&rest))
     (push (make-instance 'cst::ordinary-rest-parameter-group
             :children (cl:list
                        (make-instance 'cst::keyword-rest
                          :name (caar groups))
                        (cst::make-simple-variable (cadar groups))))
           result)
     (pop groups)))

(defmacro do-aux-parameter-group ()
  `(when (and (not (null groups))
              (cst::lambda-list-keyword-p (caar groups) '&aux))
     (let ((parameters (mapcar #'cst::parse-aux-parameter (cdar groups)))
           (keyword (make-instance 'cst::keyword-aux :name (caar groups))))
       (push (make-instance 'cst::aux-parameter-group
               :children (cl:cons keyword parameters))
             result))
     (pop groups)))

(defmacro do-destructuring-optional-parameter-group ()
  `(when (and (not (null groups))
              (cst::lambda-list-keyword-p (caar groups) '&optional))
     (push (make-instance 'cst:destructuring-optional-parameter-group
                          :children (cl:cons (make-instance 'cst::keyword-optional
                                                            :name (caar groups))
                                             (mapcar #'cst::parse-ordinary-optional-parameter
                                                     (cdar groups))))
           result)
     (pop groups)))

(defmacro do-destructuring-rest-parameter-group ()
  `(when (and (not (null groups))
              (or (cst::lambda-list-keyword-p (caar groups) '&rest)
                  (cst::lambda-list-keyword-p (caar groups) '&body)))
     (push (make-instance 'cst::destructuring-rest-parameter-group
             :children (cl:list
                        (make-instance 'cst::keyword-rest
                          :name (caar groups))
                        (parse-destructuring-parameter (cadar groups))))
           result)
     (pop groups)))

(defmacro do-destructuring-key-parameter-group ()
  `(when (and (not (null groups))
              (cst::lambda-list-keyword-p (caar groups) '&key))
     (let ((parameters (mapcar #'cst::parse-ordinary-key-parameter (cdar groups)))
           (keyword (make-instance 'cst::keyword-key :name (caar groups))))
       (push (make-instance 'cst:destructuring-key-parameter-group
                            :children (append
                                       (cl:list keyword)
                                       parameters
                                       (if (or (cl:null (cdr groups))
                                               (not (cst::lambda-list-keyword-p (caadr groups) '&allow-other-keys)))
                                           '()
                                           (prog1
                                               (cl:list
                                                (make-instance 'cst::keyword-allow-other-keys
                                                               :name (caadr groups)))
                                             (pop groups)))))
             result))
     (pop groups)))

(defmacro do-whole-parameter-group ()
  `(progn (push (make-instance 'cst::whole-parameter-group
                  :children (cl:list
                             (make-instance 'cst::keyword-whole
                               :name (caar groups))
                             (cst::make-simple-variable (cadar groups))))
                result)
          (pop groups)))

(defun parse-ordinary-lambda-list (lambda-list)
  (let ((groups (split-lambda-list lambda-list))
        (result '()))
    (do-ordinary-required-parameter-group)
    (do-ordinary-optional-parameter-group)
    (do-ordinary-rest-parameter-group)
    (do-ordinary-key-parameter-group)
    (do-aux-parameter-group)
    (make-instance 'cst::ordinary-lambda-list
      :children (reverse result))))

(defun parse-generic-function-lambda-list (lambda-list)
  (let ((groups (split-lambda-list lambda-list))
        (result '()))
    (do-ordinary-required-parameter-group)
    (when (and (not (null groups))
               (cst::lambda-list-keyword-p (caar groups) '&optional))
      (push (make-instance 'cst::generic-function-optional-parameter-group
              :children (cl:cons (make-instance 'cst::keyword-optional
                                   :name (caar groups))
                         (mapcar #'cst::parse-generic-function-optional-parameter
                                 (cdar groups))))
            result)
      (pop groups))
    (do-ordinary-rest-parameter-group)
    (when (and (not (null groups))
               (cst::lambda-list-keyword-p (caar groups) '&key))
      (let ((parameters (mapcar #'cst::parse-generic-function-key-parameter
                                (cdar groups)))
            (keyword (make-instance 'cst::keyword-key :name (caar groups))))
        (push (make-instance 'cst::generic-function-key-parameter-group
                :children (append
                           (cl:list keyword)
                           parameters
                           (if (cl:null (cdr groups))
                               '()
                               (cl:list
                                (make-instance 'cst::keyword-allow-other-keys
                                  :name (cadar groups))))))
            result))
      (pop groups))
    (make-instance 'cst::generic-function-lambda-list
      :children (reverse result))))

(defun parse-specialized-lambda-list (lambda-list)
  (let ((groups (split-lambda-list lambda-list))
        (result '()))
    (push (make-instance 'cst::specialized-required-parameter-group
            :children (mapcar #'cst::parse-specialized-required-parameter
                       (car groups)))
          result)
    (pop groups)
    (do-ordinary-optional-parameter-group)
    (do-ordinary-rest-parameter-group)
    (do-ordinary-key-parameter-group)
    (do-aux-parameter-group)
    (make-instance 'cst::specialized-lambda-list
      :children (reverse result))))

(defun parse-defsetf-lambda-list (lambda-list)
  (let ((groups (split-lambda-list lambda-list))
        (result '()))
    (do-ordinary-required-parameter-group)
    (do-ordinary-optional-parameter-group)
    (do-ordinary-rest-parameter-group)
    (do-ordinary-key-parameter-group)
    (when (and (not (null groups))
               (cst::lambda-list-keyword-p (caar groups) '&environment))
      (push (make-instance 'cst::environment-parameter-group
              :children (cl:list
                         (make-instance 'cst::keyword-environment
                           :name (caar groups))
                         (cst::make-simple-variable (cadar groups))))
            result)
      (pop groups))
    (make-instance 'cst::defsetf-lambda-list
      :children (reverse result))))

(defun parse-define-modify-macro-lambda-list (lambda-list)
  (let ((groups (split-lambda-list lambda-list))
        (result '()))
    (do-ordinary-required-parameter-group)
    (do-ordinary-optional-parameter-group)
    (do-ordinary-rest-parameter-group)
    (make-instance 'cst::define-modify-macro-lambda-list
      :children (reverse result))))

(defun parse-define-method-combination-lambda-list (lambda-list)
  (let ((result '())
        groups)
    (if (and (not (null lambda-list))
             (eq (car lambda-list) '&whole))
        (let* ((keyword-name (cst:cst-from-expression (car lambda-list)))
               (keyword (make-instance 'cst::keyword-whole :name keyword-name))
               (variable-name (cst:cst-from-expression (cadr lambda-list)))
               (variable (cst::make-simple-variable variable-name))
               (whole-group (make-instance 'cst:ordinary-whole-parameter-group
                              :children (cl:list keyword variable))))
          (push whole-group result)
          (setf groups (split-lambda-list (cddr lambda-list))))
        (setf groups (split-lambda-list lambda-list)))
    (do-ordinary-required-parameter-group)
    (do-ordinary-optional-parameter-group)
    (do-ordinary-rest-parameter-group)
    (do-ordinary-key-parameter-group)
    (do-aux-parameter-group)
    (make-instance 'cst::define-method-combination-lambda-list
      :children (reverse result))))

(defun parse-destructuring-parameter (parameter)
  (if (typep (cst:raw parameter) '(and symbol (not null)))
      (parse-simple-variable parameter)
      (parse-destructuring-lambda-list (cst:raw parameter))))

(defun parse-destructuring-lambda-list (lambda-list)
  (let ((result '())
        groups)
    (if (and (not (null lambda-list))
             (eq (car lambda-list) '&whole))
        (let* ((keyword-name (cst:cst-from-expression (car lambda-list)))
               (keyword (make-instance 'cst::keyword-whole :name keyword-name))
               (variable-name (cst:cst-from-expression (cadr lambda-list)))
               (variable (cst::make-simple-variable variable-name))
               (whole-group (make-instance 'cst:destructuring-whole-parameter-group
                              :children (cl:list keyword variable))))
          (push whole-group result)
          (setf groups (split-lambda-list (cddr lambda-list))))
        (setf groups (split-lambda-list lambda-list)))
    (push (make-instance 'cst::destructuring-required-parameter-group
            :children (mapcar #'parse-destructuring-parameter
                       (car groups)))
          result)
    (pop groups)
    (do-ordinary-optional-parameter-group)
    (do-destructuring-rest-parameter-group)
    (do-ordinary-key-parameter-group)
    (do-aux-parameter-group)
    (make-instance 'cst::destructuring-lambda-list
      :children (reverse result))))

(defun parse-macro-lambda-list (lambda-list)
  (let ((result '())
        (groups (split-macro-lambda-list lambda-list)))
    (when (and (not (null groups))
               (cst::lambda-list-keyword-p (caar groups) '&whole))
      (push (make-instance 'cst:destructuring-whole-parameter-group
              :children (cl:list
                         (make-instance 'cst::keyword-whole
                           :name (caar groups))
                         (cst::make-simple-variable (cadar groups))))
            result)
      (pop groups))
    (flet ((do-environment ()
             (when (and (not (null groups))
                        (cst::lambda-list-keyword-p (caar groups) '&environment))
               (push (make-instance 'cst::environment-parameter-group
                       :children (cl:list
                                  (make-instance 'cst::keyword-environment
                                    :name (caar groups))
                                  (cst::make-simple-variable (cadar groups))))
                     result)
               (pop groups))))
      (do-environment)
      (push (make-instance 'cst::destructuring-required-parameter-group
              :children (mapcar #'parse-destructuring-parameter
                         (car groups)))
            result)
      (pop groups)
      (do-environment)
      (do-destructuring-optional-parameter-group)
      (do-environment)
      (do-destructuring-rest-parameter-group)
      (do-environment)
      (do-destructuring-key-parameter-group)
      (do-environment)
      (do-aux-parameter-group)
      (do-environment)
      (make-instance 'cst::macro-lambda-list
        :children (reverse result)))))
