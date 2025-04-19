(cl:in-package #:concrete-syntax-tree)

;;;; The QUASIQUOTE operator allows concrete syntax trees to be constructed in
;;;; a manner analogous to list quasiquotation (backquote).
;;;; The syntax is (QUASIQUOTE source template), where
;;;; template := atom
;;;;             | (UNQUOTE form-yielding-cst)
;;;;             | (template . template)
;;;;             | ((UNQUOTE-SPLICING form-yielding-list-cst-or-list)
;;;;                . template)
;;;; Unquotation means the CST yielded by a form will be inserted.
;;;; Splicing unquotation accepts both list CSTs
;;;; (i.e. CSTs suitable for LISTIFY) and lists of CSTs.
;;;; Atoms are made into ATOM-CSTs.
;;;; All CSTs created by quasiquotation will have the given source (evaluated)
;;;; as their source.
;;;; Note that unlike CL quasiquotation, vector templates are not allowed, as
;;;; there are no vector CSTs. Vectors in templates will be treated as atoms
;;;; and wrapped in CSTs without processing of the elements.
;;;; There is no equivalent to ,.

(defun %quote (source atom) (make-instance 'atom-cst :raw atom :source source))

(defun %append2 (source x y)
  (etypecase x
    (atom-cst y) ; could check that X is really NIL
    (cons-cst (cons (first x) (%append2 source (rest x) y) :source source))
    (cl:null y)
    (cl:cons
     (cons (cl:first x) (%append2 source (cl:rest x) y) :source source))))

(defun %append (source &rest list-csts-and-lists)
  (cond ((cl:null list-csts-and-lists) (%quote source nil))
        ((cl:null (cl:rest list-csts-and-lists))
         (cl:first list-csts-and-lists))
        (t (%append2 source (cl:first list-csts-and-lists)
                     (apply #'%append source (cl:rest list-csts-and-lists))))))

(defun transform (sourcef form)
  (typecase form
    ;; We can use CL:LIST instead of building a CST because TRANSFORM always
    ;; returns a form used as a non-final argument to %APPEND.
    (cl:atom `(cl:list (%quote ,sourcef ',form)))
    ((cl:cons (eql unquote)) `(cl:list ,(cl:second form)))
    ((cl:cons (eql unquote-splicing)) (cl:second form))
    (t `(cl:list ,(appender sourcef form)))))

(defun transform-compound (sourcef object)
  (labels ((rec (object)
             (typecase object
               ((cl:cons t cl:atom) ; (a . b)
                (cl:list (transform sourcef (cl:car object))
                         `(%quote ,sourcef ',(cl:cdr object))))
               ((cl:cons t (cl:cons (eql unquote))) ; (a . ,b)
                (cl:list (transform sourcef (cl:car object))
                         (cl:second (cl:cdr object))))
               ((cl:cons t (cl:cons (eql unquote-splicing))) ; (a . ,@b)
                (error 'unquote-splicing-in-dotted-list))
               (t (cl:list* (transform sourcef (cl:car object))
                            (rec (cl:cdr object)))))))
    (rec object)))

(defun appender (sourcef argument)
  ;; We could do some optimization here - transforming to a %LIST*, etc.
  `(%append ,sourcef ,@(transform-compound sourcef argument)))

(defun transform-qq-argument (sourcef argument)
  (if (cl:atom argument)
      `(%quote ,sourcef ',argument)
      (case (cl:car argument)
        ((unquote) (cl:second argument))
        ((unquote-splicing) (error 'unquote-splicing-at-top))
        (t (appender sourcef argument)))))

(defmacro quasiquote (sourcef argument)
  (let ((gsource (gensym "SOURCE")))
    `(let ((,gsource ,sourcef))
       ,(transform-qq-argument gsource argument))))
