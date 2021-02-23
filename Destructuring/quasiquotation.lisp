(in-package #:concrete-syntax-tree)

(defun cst-append (&rest csts) (cstify (mapcan #'listify csts)))

(defun cst-list* (first &rest rest)
  (if (cl:null rest)
      first
      (cons first (apply #'cst-list* rest))))

(defun needs-append-p (expr)
  (and (cl:consp expr)
       (eq (cl:car expr) 'unquote-splicing)))

(defun expand-quasiquotation (sourcef argument)
  (if (cl:atom argument)
      `(make-instance 'atom-cst :raw ',argument :source ,sourcef)
      (case (cl:car argument)
        ((unquote) (cl:second argument))
        ((unquote-splicing) (error "Bad quasiquotation ~a" argument))
        (t
         (expand-quasiquoted-list sourcef argument)))))

(defun expand-quasiquoted-list (sourcef list)
  (cond ((notany #'needs-append-p list)
         `(list ,@(loop for expr in list
                        collect (expand-quasiquotation sourcef expr))))
        ((notany #'needs-append-p (butlast list))
         `(cst-list* ,@(loop for expr in (butlast list)
                             collect (expand-quasiquotation sourcef expr))
                     ;; The last must be an unquote-splicing, so:
                     ,(cl:second (cl:first (last list)))))
        (t ; full append
         `(cst-append
           ,@(loop for expr in list
                   collect (cond
                             ((eq (cl:car expr) 'unquote-splicing)
                              (cl:second expr))
                             (t
                              `(list
                                ,(expand-quasiquotation sourcef expr)))))))))

(defmacro quasiquote (sourcef argument)
  (expand-quasiquotation sourcef argument))
