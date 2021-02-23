(in-package #:concrete-syntax-tree)

(defun cst-append (&rest csts) (cstify (mapcan #'listify csts)))

(defun expand-quasiquotation (sourcef argument)
  (if (cl:atom argument)
      `(make-instance 'atom-cst :raw ',argument :source ,sourcef)
      (case (cl:car argument)
        ((unquote) (cl:second argument))
        ((unquote-splicing) (error "Bad quasiquotation ~a" argument))
        (t
         (expand-quasiquoted-list sourcef argument)))))

(defun expand-quasiquoted-list (sourcef list)
  (if (loop for expr in list
            always (or (cl:atom expr)
                       (not (eq (cl:car expr) 'unquote-splicing))))
      `(list ,@(loop for expr in list
                     collect (if (cl:atom expr)
                                 `(make-instance 'atom-cst
                                    :raw ',expr :source ,sourcef)
                                 (expand-quasiquotation sourcef expr))))
      `(cst-append
        ,@(loop for expr in list
                collect (cond ((cl:atom expr)
                               `(list (make-instance 'atom-cst
                                        :raw ',expr :source ,sourcef)))
                              ((eq (cl:car expr) 'unquote-splicing)
                               (cl:second expr))
                              (t
                               `(list
                                 ,(expand-quasiquotation sourcef expr))))))))

(defmacro quasiquote (sourcef argument)
  (expand-quasiquotation sourcef argument))
