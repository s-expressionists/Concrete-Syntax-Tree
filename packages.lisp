(cl:in-package #:common-lisp-user)

(defpackage #:concrete-syntax-tree
  (:nicknames #:cst)
  (:use #:common-lisp)
  (:shadow #:first
           #:second
           #:third
           #:fourth
           #:fifth
           #:sixth
           #:seventh
           #:eighth
           #:ninth
           #:tenth
           #:nth
           #:rest
           #:atom
           #:consp
           #:null)
  (:export #:cst
           #:parent
           #:source
           #:first
           #:second
           #:third
           #:fourth
           #:fifth
           #:sixth
           #:seventh
           #:eighth
           #:ninth
           #:tenth
           #:nth
           #:rest
           #:atom
           #:consp
           #:null
           #:cst-from-expression
           #:reconstruct))
