(cl:in-package #:concrete-syntax-tree)

;;;; This code provides useful operations on bindings as used by LET
;;;; and LET*.  The bindings are in the form of a CST.  Recall that
;;;; such a binding can have three different shapes, namely VAR,
;;;; (VAR), or (VAR INIT-FORM).  The last of these three shapes is
;;;; considered canonical.

;;; Check whether a binding is valid, i.e., has one of the three valid
;;; shapes.
(defun valid-binding-p (binding-cst)
  (let ((raw (raw binding-cst)))
    (or (symbolp raw)
        (and (cl:consp raw)
             (symbolp (car raw))
             (or (cl:null (cdr raw))
                 (and (cl:consp (cdr raw))
                      (cl:null (cddr raw))))))))

;;; Check whether each binding in a list of bindings represented as a
;;; CST is valid.
(defun valid-bindings-p (bindings-cst)
  (and (proper-list-p bindings-cst)
       (loop for rest = bindings-cst then (rest rest)
             until (null rest)
             always (valid-binding-p (first rest)))))

;;; Check whether a single binding in the form of a CST represents a
;;; canonical binding.  It is assumed that the binding is valid as
;;; checked by VALID-BINDING-P.
(defun canonical-binding-p (binding-cst)
  (= (length (raw binding-cst)) 2))

;;; Canonicalize a single binding represented as a CST.  It is assumed
;;; that the binding is valid, but we do not know whether the binding
;;; is already canonical.  If it is canonical, we return it as is.  If
;;; not, we return a canonicalized version of it.
(defun canonicalize-binding (binding-cst)
  (if (canonical-binding-p binding-cst)
      binding-cst
      (if (atom binding-cst)
          (let ((raw (cl:list (raw binding-cst) nil)))
            (make-instance 'cons-cst
              :raw raw
              :source (source binding-cst)
              :first binding-cst
              :rest (make-instance 'cons-cst
                      :raw (cdr raw)
                      :first (make-instance 'atom-cst :raw nil)
                      :rest (make-instance 'atom-cst :raw nil))))
          (let ((raw (cl:list (car (raw binding-cst)) nil)))
            (make-instance 'cons-cst
              :raw raw
              :source (source binding-cst)
              :first (first binding-cst)
              :rest (make-instance 'cons-cst
                      :raw (cdr raw)
                      :first (make-instance 'atom-cst :raw nil)
                      :rest (make-instance 'atom-cst :raw nil)))))))

;;; Check whether each binding in a list of bindings represented as a
;;; CST is canonical.  It is assumed that the bindings have been
;;; checked for validity as reported by VALID-BINDINGS-P.
(defun canonical-bindings-p (bindings-cst)
  (loop for rest = bindings-cst then (rest rest)
        until (null rest)
        always (canonical-binding-p (first rest))))

;;; Canonicalize a list of bindings represented as a CST.  If the list
;;; of bindings is already canonical, it is returned as is.  Otherwise
;;; a new CST is constructed in which each binding has been
;;; canonicalized.  It is assumed that the bindings have been checked
;;; for validity as reported by VALID-BINDINGS-P.
(defun canonicalize-bindings (bindings-cst)
  (if (null bindings-cst)
      bindings-cst
      (let ((rest (canonicalize-bindings (rest bindings-cst))))
        (if (and (eq rest (rest bindings-cst))
                 (canonical-binding-p (first bindings-cst)))
            bindings-cst
            (let ((new-first (canonicalize-binding (first bindings-cst))))
              (make-instance 'cons-cst
                :raw (cl:cons (raw new-first) (raw rest))
                :source (source bindings-cst)
                :first new-first
                :rest rest))))))

;;; Given a list of bindings represented as a CST, return a list of
;;; the variables bound in those bindings, also as a CST.  This
;;; function is useful for turning a LET form into a LAMBDA form.  It
;;; is assumed that the list of bindings is canonical.
(defun binding-variables (bindings-cst)
  (if (null bindings-cst)
      bindings-cst
      (let ((rest (binding-variables (rest bindings-cst))))
        (make-instance 'cons-cst
          :raw (cl:cons (car (raw (first bindings-cst))) (raw rest))
          :source nil
          :first (first (first bindings-cst))
          :rest rest))))

;;; Given a list of bindings represented as a CST, return a list of
;;; the initialization forms of those bindings, also as a CST.  This
;;; function is useful for turning a LET form into a LAMBDA form.  It
;;; is assumed that the list of bindings is canonical.
(defun binding-init-forms (bindings-cst)
  (if (null bindings-cst)
      bindings-cst
      (let ((rest (binding-init-forms (rest bindings-cst))))
        (make-instance 'cons-cst
          :raw (cl:cons (cadr (raw (first bindings-cst))) (raw rest))
          :source nil
          :first (second (first bindings-cst))
          :rest rest))))

;;;  LocalWords:  canonicalized, canonicalize
