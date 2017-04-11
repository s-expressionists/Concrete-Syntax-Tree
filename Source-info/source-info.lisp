(cl:in-package #:concrete-syntax-tree-source-info)

;;; This module is provided for convenience.  It is not required in
;;; order for the rest of this library to work.

(defclass source-info () ())

(defclass standard-source-info (source-info)
  (;; This slot contains some kind of document from which this source
   ;; code was read.  It can be a file name, a string, an editor
   ;; buffer, or any other document that client code is able to
   ;; manipulate.
   (%document :initarg :document :reader document)
   ;; Lines in source code are numbered starting at 0.  
   (%start-line-number :initarg :start-line-number :reader start-line-number)
   ;; A height of 0 indicates that the source of this code starts and
   ;; ends on the same line.
   (%height :initarg height :reader height)
   ;; Columns in source code are numbered starting at 0.  
   (%start-column-number :initarg :start-column-number :reader start-column-number)
   ;; This slot contains a value that is one greater than the last
   ;; column of this source code, following the convention of similar
   ;; functionality in other parts of Common Lisp.
   (%end-column-number :initarg :end-column-number :reader end-column-number)))
