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
           #:cons-cst
           #:null-cst
           #:atom-cst
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
           #:canonicalize-declaration-specifiers
           #:canonicalize-declarations
           #:declaration-specifiers
           #:reconstruct
           #:parser
           #:parse
           #:grammar-symbol
           #:parameter-group
           #:singleton-parameter-group-mixin
           #:multi-parameter-group-mixin
           #:implicit-parameter-group
           #:explicit-parameter-group
           #:explicit-multi-parameter-group
           #:ordinary-required-parameter-group
           #:optional-parameter-group
           #:ordinary-optional-parameter-group
           #:key-parameter-group
           #:ordinary-key-parameter-group
           #:generic-function-key-parameter-group
           #:aux-parameter-group
           #:generic-function-optional-parameter-group
           #:specialized-required-parameter-group
           #:destructuring-required-parameter-group
           #:singleton-parameter-group
           #:ordinary-rest-parameter-group
           #:destructuring-rest-parameter-group
           #:environment-parameter-group
           #:whole-parameter-group
           #:parameter
           #:simple-variable
           #:ordinary-optional-parameter
           #:ordinary-key-parameter
           #:generic-function-key-parameter
           #:aux-parameter
           #:generic-function-optional-parameter
           #:specialized-required-parameter
           #:destructuring-parameter
           #:lambda-list-keyword
           #:keyword-optional
           #:keyword-rest
           #:keyword-body
           #:keyword-key
           #:keyword-allow-other-keys
           #:keyword-aux
           #:keyword-environment
           #:keyword-whole
           #:ordinary-lambda-list
           #:generic-function-lambda-list
           #:specialized-lambda-list
           #:defsetf-lambda-list
           #:define-modify-macro-lambda-list
           #:define-method-combination-lambda-list
           #:destructuring-lambda-list
           #:macro-lambda-list
           #:parse-ordinary-lambda-list
           #:parse-generic-function-lambda-list
           #:parse-specialized-lambda-list
           #:parse-defsetf-lambda-list
           #:parse-define-modify-macro-lambda-list
           #:parse-define-method-combination-lambda-list
           #:parse-destructuring-lambda-list
           #:parse-macro-lambda-list
           #:target
           #:*ordinary-required-parameter-group*
           #:*ordinary-optional-parameter-group*
           #:*ordinary-rest-parameter-group*
           #:*ordinary-key-parameter-group*
           #:*aux-parameter-group*
           #:*ordinary-lambda-list*
           #:*generic-function-optional-parameter-group*
           #:*generic-function-key-parameter-group*
           #:*generic-function-lambda-list*
           #:*specialized-required-parameter-group*
           #:*specialized-lambda-list*
           #:*environment-parameter-group*
           #:*defsetf-lambda-list*
           #:*define-modify-macro-lambda-list*
           #:*whole-parameter-group*
           #:*define-method-combination-lambda-list*
           #:*destructuring-required-parameter-group*
           #:*destructuring-rest-parameter-group*
           #:*destructuring-lambda-list*
           #:*macro-lambda-list*
           #:*standard-grammar*
           #:*ordinary-lambda-list-grammar*
           #:*generic-function-lambda-list-grammar*
           #:*specialized-lambda-list-grammar*
           #:*defsetf-lambda-list-grammar*
           #:*define-modify-macro-lambda-list-grammar*
           #:*define-method-combination-lambda-list-grammar*
           #:*destructuring-lambda-list-grammar*
           #:*macro-lambda-list-grammar*
           #:destructure-lambda-list
           #:destructure-aux-parameter
           #:destructure-aux-parameters
           #:destructure-key-parameter
           #:destructure-key-parameters
           #:destructure-rest-parameter
           #:destructure-optional-parameter
           #:destructure-optional-parameters
           #:destructure-required-parameter
           #:destructure-required-parameters
           #:destructure-parameter-group
           #:parse-macro
           #:db))
