(cl:in-package #:concrete-syntax-tree-test)

(def-suite* :concrete-syntax-tree.quasiquote
  :in :concrete-syntax-tree)

(test quasiquotation.smoke
  (let* ((cst1 (cst:cst-from-expression 'a))
         (cst2 (cst:cst-from-expression '(b c)))
         (source (gensym))
         (qq (cst:quasiquote source
                             (d (cst:unquote cst1) (cst:unquote-splicing cst2)
                                (cst:unquote cst2) (e (f f) . g)
                                (cst:unquote-splicing (cl:list cst1 cst1))
                                ((cst:unquote cst1) . (cst:unquote cst1))))))
    (is (equal '(d a b c (b c) (e (f f) . g) a a (a . a)) (cst:raw qq)))
    (is (eq source (cst:source qq)))
    (is (eq cst1 (cst:second qq)))
    (is (eq cst2 (cst:fifth qq)))
    (is (eq cst1 (cst:first (cst:ninth qq))))
    (is (eq cst1 (cst:rest (cst:ninth qq))))))
