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
           #:cons
           #:list
           #:null
           #:keyword)
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
           #:nthrest
           #:nth
           #:rest
           #:atom
           #:consp
           #:cons
           #:list
           #:null
           #:raw
           #:listify
           #:separate-ordinary-body
           #:separate-function-body
           #:list-structure
           #:proper-list-p
           #:cst-from-expression
           #:canonicalize-declaration-specifier
           #:reconstruct
           #:parse-ordinary-lambda-list
           #:parse-generic-function-lambda-list
           #:parse-specialized-lambda-list
           #:parse-defsetf-lambda-list
           #:parse-define-modify-macro-lambda-list
           #:parse-define-method-combination-lambda-list
           #:parse-destructuring-lambda-list
           #:parse-macro-lambda-list))
