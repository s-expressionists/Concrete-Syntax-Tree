(cl:in-package #:asdf-user)

(defsystem :concrete-syntax-tree
  :description "Library for parsing Common Lisp code into a concrete syntax tree."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :license "FreeBSD, see file LICENSE.text"
  :depends-on (:concrete-syntax-tree-base
               :concrete-syntax-tree-lambda-list))
