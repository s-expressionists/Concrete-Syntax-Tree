(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defun parse-simple-variable (parameter)
  (make-instance 'cst::simple-variable :name parameter))

(defun parse-specialized-required-parameter (parameter)
  (cond ((symbolp parameter)
         (make-instance 'cst::specialized-required-parameter
           :name parameter
           :specializer t))
        ((null (cdr parameter))
         (make-instance 'cst::specialized-required-parameter
           :name (car parameter)
           :specializer t))
        (t
         (make-instance 'cst::specialized-required-parameter
           :name (car parameter)
           :specializer (cadr parameter)))))

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
               :name (cadar parameter)
               :form nil
               :keyword (caar parameter)
               :supplied-p (gensym))))
        ((null (cddr parameter))
         (if (symbolp (car parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (car parameter)
               :form (cadr parameter)
               :keyword (intern (symbol-name (car parameter)) :keyword)
               :supplied-p (gensym))
             (make-instance 'cst::ordinary-key-parameter
               :name (cadar parameter)
               :form (cadr parameter)
               :keyword (caar parameter)
               :supplied-p (gensym))))
        (t
         (if (symbolp (car parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (car parameter)
               :form (cadr parameter)
               :keyword (intern (symbol-name (car parameter)) :keyword)
               :supplied-p (caddr parameter))
             (make-instance 'cst::ordinary-key-parameter
               :name (cadar parameter)
               :form (cadr parameter)
               :keyword (caar parameter)
               :supplied-p (caddr parameter))))))

(defun parse-aux-parameter (parameter)
  (cond ((symbolp parameter)
         (make-instance 'cst::aux-parameter
           :name parameter
           :form nil))
        ((null (cdr parameter))
         (make-instance 'cst::aux-parameter
           :name (car parameter)
           :form nil))
        (t
         (make-instance 'cst::aux-parameter
           :name (car parameter)
           :form (cadr parameter)))))

(defun parse-generic-function-optional-parameter (parameter)
  (make-instance 'cst::generic-function-optional-parameter
    :name (if (symbolp parameter) parameter (car parameter))))

(defun parse-generic-function-key-parameter (parameter)
  (cond ((symbolp parameter)
         (make-instance 'cst::generic-function-key-parameter
           :name parameter
           :keyword (intern (symbol-name parameter) :keyword)))
        ((symbolp (car parameter))
         (make-instance 'cst::generic-function-key-parameter
           :name (car parameter)
           :keyword (intern (symbol-name (car parameter)) :keyword)))
        (t
         (make-instance 'cst::generic-function-key-parameter
           :name (cadar parameter)
           :keyword (caar parameter)))))

(defun position-of-first-keyword (lambda-list)
  (position-if (lambda (element)
                 (member element lambda-list-keywords))
               lambda-list))

;;; Split a lambda list into groups according to role.  Each group is
;;; a list.  The first group is a list of required parameters, so it
;;; does not start with a lambda-list keyword.  Each of the remaining
;;; groups starts with a lambda-list keyword that characterizes the
;;; group.  Notice that this function does not handle lambda lists
;;; that start with &WHOLE.  It assumes that everything that precedes
;;; the first lambda-list keyword belongs to the group of required
;;; parameters.  A list of the groups is returned.
(defun split-lambda-list (lambda-list)
  (let ((remaining lambda-list)
        (result '()))
    (when (and (not (null lambda-list)) (eq (car lambda-list) '&whole))
      (push (subseq lambda-list 0 2) result)
      (setf lambda-list (cddr lambda-list)))
    (let ((pos (position-of-first-keyword lambda-list)))
      (if (null pos)
          (append (reverse result) (list lambda-list))
          (progn (push (subseq lambda-list 0 pos) result)
                 (setf remaining (subseq lambda-list pos))
                 (setf pos 0)
                 (loop for pos = (position-of-first-keyword (cdr remaining))
                       until (null pos)
                       do (push (subseq remaining 0 (1+ pos)) result)
                          (setf remaining (subseq remaining (1+ pos)))
                       finally (push remaining result)
                               (return (reverse result))))))))

(defmacro do-ordinary-required-parameter-group ()
  `(progn (push (make-instance 'cst::ordinary-required-parameter-group
                  :children (mapcar #'parse-simple-variable
                             (car groups)))
                result)
          (pop groups)))

(defmacro do-ordinary-optional-parameter-group ()
  `(when (and (not (null groups)) (eq (caar groups) '&optional))
     (push (make-instance 'cst::ordinary-optional-parameter-group
             :children (cl:cons (make-instance 'cst::keyword-optional
                                  :name (caar groups))
                        (mapcar #'parse-ordinary-optional-parameter
                                (cdar groups))))
           result)
     (pop groups)))

(defmacro do-ordinary-key-parameter-group ()
  `(when (and (not (null groups)) (eq (caar groups) '&key))
     (let ((parameters (mapcar #'parse-ordinary-key-parameter (cdar groups)))
           (keyword (make-instance 'cst::keyword-key :name (caar groups))))
       (push (make-instance 'cst::ordinary-key-parameter-group
               :children (append
                          (cl:list keyword)
                          parameters
                          (if (or (cl:null (cdr groups))
                                  (not (eq (caadr groups) '&allow-other-keys)))
                              '()
                              (prog1
                                  (cl:list
                                   (make-instance 'cst::keyword-allow-other-keys
                                     :name (caadr groups)))
                                (pop groups)))))
             result))
     (pop groups)))

(defmacro do-ordinary-rest-parameter-group ()
  `(when (and (not (null groups)) (eq (caar groups) '&rest))
     (push (make-instance 'cst::ordinary-rest-parameter-group
             :children (cl:list
                        (make-instance 'cst::keyword-rest
                          :name (caar groups))
                        (make-instance 'cst::simple-variable
                          :name (cadar groups))))
           result)
     (pop groups)))

(defmacro do-aux-parameter-group ()
  `(when (and (not (null groups)) (eq (caar groups) '&aux))
     (let ((parameters (mapcar #'parse-aux-parameter (cdar groups)))
           (keyword (make-instance 'cst::keyword-aux :name (caar groups))))
       (push (make-instance 'cst::aux-parameter-group
               :children (cl:cons keyword parameters))
             result))
     (pop groups)))

(defmacro do-destructuring-rest-parameter-group ()
  `(when (and (not (null groups))
              (or (eq (caar groups) '&rest)
                  (eq (caar groups) '&body)))
     (push (make-instance 'cst::destructuring-rest-parameter-group
             :children (cl:list
                        (make-instance 'cst::keyword-rest
                          :name (caar groups))
                        (parse-destructuring-parameter (cadar groups))))
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
    (when (and (not (null groups)) (eq (caar groups) '&optional))
      (push (make-instance 'cst::generic-function-optional-parameter-group
              :children (cl:cons (make-instance 'cst::keyword-optional
                                   :name (caar groups))
                         (mapcar #'parse-generic-function-optional-parameter
                                 (cdar groups))))
            result)
      (pop groups))
    (do-ordinary-rest-parameter-group)
    (when (and (not (null groups)) (eq (caar groups) '&key))
      (let ((parameters (mapcar #'parse-generic-function-key-parameter
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
            :children (mapcar #'parse-specialized-required-parameter
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
    (when (and (not (null groups)) (eq (caar groups) '&environment))
      (push (make-instance 'cst::environment-parameter-group
              :children (cl:list
                         (make-instance 'cst::keyword-environment
                           :name (caar groups))
                         (make-instance 'cst::simple-variable
                           :name (cadar groups))))
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
    (if (and (not (null lambda-list)) (eq (car lambda-list) '&whole))
        (let ((whole-group (make-instance 'cst::whole-parameter-group
                             :children (cl:list
                                        (make-instance 'cst::keyword-whole
                                          :name (car lambda-list))
                                        (make-instance 'cst::simple-variable
                                          :name (cadr lambda-list))))))
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
  (if (symbolp parameter)
      (parse-simple-variable parameter)
      (parse-destructuring-lambda-list parameter)))

(defun parse-destructuring-lambda-list (lambda-list)
  (let ((result '())
        groups)
    (if (and (not (null lambda-list)) (eq (car lambda-list) '&whole))
        (let ((whole-group (make-instance 'cst::whole-parameter-group
                             :children (cl:list
                                        (make-instance 'cst::keyword-whole
                                          :name (car lambda-list))
                                        (make-instance 'cst::simple-variable
                                          :name (cadr lambda-list))))))
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
        groups)
    (if (and (not (null lambda-list)) (eq (car lambda-list) '&whole))
        (let ((whole-group (make-instance 'cst::whole-parameter-group
                             :children (cl:list
                                        (make-instance 'cst::keyword-whole
                                          :name (car lambda-list))
                                        (make-instance 'cst::simple-variable
                                          :name (cadr lambda-list))))))
          (push whole-group result)
          (setf groups (split-lambda-list (cddr lambda-list))))
        (setf groups (split-lambda-list lambda-list)))
    (flet ((do-environment ()
             (when (and (not (null groups)) (eq (caar groups) '&environment))
               (push (make-instance 'cst::environment-parameter-group
                       :children (cl:list
                                  (make-instance 'cst::keyword-environment
                                    :name (caar groups))
                                  (make-instance 'cst::simple-variable
                                    :name (cadar groups))))
                     result)
               (pop groups))))
      (do-environment)
      (push (make-instance 'cst::destructuring-required-parameter-group
              :children (mapcar #'parse-destructuring-parameter
                         (car groups)))
            result)
      (pop groups)
      (do-environment)
      (do-ordinary-optional-parameter-group)
      (do-environment)
      (do-destructuring-rest-parameter-group)
      (do-environment)
      (do-ordinary-key-parameter-group)
      (do-environment)
      (do-aux-parameter-group)
      (do-environment)
      (make-instance 'cst::macro-lambda-list
        :children (reverse result)))))
