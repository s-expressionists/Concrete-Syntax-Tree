(cl:defpackage #:concrete-syntax-tree.tools.texinfo
  (:use
   #:cl)

  (:export
   #:write-escaped
   #:write-emph
   #:write-var
   #:write-code
   #:write-reference)

  (:export
   #:write-section)

  (:export
   #:call-with-code-environment
   #:code
   #:call-with-item-environment
   #:item
   #:call-with-itemize-environment
   #:itemize
   #:call-with-table-environment
   #:table))

(cl:in-package #:concrete-syntax-tree.tools.texinfo)

;;; Inline

(defun write-escaped (string stream)
  (loop :for character :across string
        :do (case character
              (#\{ (write-string "@{" stream))
              (#\} (write-string "@}" stream))
              (#\@ (write-string "@@" stream))
              (t   (write-char character stream)))))

(defun write-or-call (string-or-continuation stream)
  (etypecase string-or-continuation
    (string               (write-escaped string-or-continuation stream))
    ((or symbol function) (funcall string-or-continuation stream))))

(defun write-emph (content stream)
  (format stream "@emph{")
  (write-or-call content stream)
  (format stream "}"))

(defun write-var (content stream)
  (format stream "@var{")
  (write-or-call content stream)
  (format stream "}"))

(defun write-code (code stream)
  (format stream "@t{")
  (write-or-call code stream)
  (format stream "}"))

(defun write-reference (target label stream)
  (write-string "@ref{" stream)
  (write-or-call target stream)
  (when label
    (write-string "," stream)
    (write-or-call label stream))
  (write-string "}" stream))

;;; Structure

(defun write-section (title stream &key (kind :section))
  (check-type kind (member             :section       :subsection
                           :unnumbered :unnumberedsec :unnumberedsubsec
                                       :heading       :subheading))
  (unless (member kind '(:heading :subheading))
    (format stream "@node ~A~@:_" title))
  (format stream "@~(~A~) ~A~@:_~@:_" kind title))

;;; Environments

(defun call-with-code-environment (continuation stream &key language)
  (let ((command (ecase language
                   ((nil)        "example")
                   (:common-lisp "lisp"))))
    (format stream "@~A~@:_" command)
    (funcall continuation stream)
    (format stream "~@:_@end ~A~@:_~@:_" command)))

(defmacro code ((stream &key language) &body body)
  `(call-with-code-environment
    (lambda (,stream) ,@body) ,stream :language ,language))

(defun call-with-item-environment (continuation stream &key first-line)
  (format stream "@item")
  (when first-line
    (write-char #\Space stream)
    (write-or-call first-line stream)
    (format stream "~@:_"))
  (format stream "~@:_")
  (funcall continuation stream)
  (format stream "~@:_"))

(defmacro item ((stream) first-line &body body)
  `(call-with-item-environment
    (lambda (,stream) ,@body) ,stream :first-line ,first-line))

(defun call-with-itemize-environment (continuation stream)
  (format stream "@itemize~@:_")
  (funcall continuation stream)
  (format stream "@end itemize~@:_"))

(defmacro itemize ((stream) &body body)
  `(call-with-itemize-environment (lambda (,stream) ,@body) ,stream))

(defun call-with-table-environment (continuation stream &key (item-command "asis"))
  (format stream "@table @~A~@:_" item-command)
  (funcall continuation stream)
  (format stream "@end table~@:_"))

(defmacro table ((stream &key (item-command "asis")) &body body)
  `(call-with-table-environment
    (lambda (,stream) ,@body) ,stream :item-command ,item-command))
