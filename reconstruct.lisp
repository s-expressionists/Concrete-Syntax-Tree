(cl:in-package #:concrete-syntax-tree)

;;; This file contains code that allows us to reconstruct a concrete
;;; syntax tree.  The typical scenario is as follows: Let T be some
;;; expression concrete syntax tree, and let R be the raw version of
;;; it.  Let E be a Common Lisp expression obtained by macroexpanding
;;; R.  We want to construct an expression concrete syntax tree T'
;;; with E as its raw version in such a way that when E contains a
;;; subexpression S that is also in R, then we want the corresponding
;;; concrete syntax tree for S in E to be identical to the concrete
;;; syntax tree for S in T as much as possible.
;;;
;;;   CST    T                   T'
;;;          │                   ▲
;;;          │ raw               │ reconstruct
;;;          ▼                   │
;;;   s-expr R ───macroexpand──▶ E
;;;          │                   │
;;;          │ subexpression     │ subexpression
;;;          ▼                   ▼
;;;   s-expr S                   S
;;;
;;; Clearly what we want to accomplish can not always be precise.  It
;;; can only be precise when S is a CONS and E contains the identical
;;; (in the sense of EQ) CONS.  For atoms, we just have to guess.
;;;
;;; The technique we use works as follows: We first build an EQ hash
;;; table H1, mapping all CONS cells of R to a corresponding concrete
;;; syntax tree in T.  Notice that it is possible that several
;;; concrete syntax trees of T have the identical raw version (because
;;; of the #n= and #n# reader macros).  In that case we make an
;;; arbitrary choice of one such concrete syntax tree.  Next, we
;;; create an EQL hash table H2, and we traverse E.  For each CONS of
;;; E that has a corresponding concrete syntax tree in H1, we create
;;; the analogous correspondence in H2.  After that, we again traverse
;;; R, this time looking for atoms.  As long as we are outside a CONS
;;; in H2, we always replace a (or create a new) mapping when we see
;;; an atom.  If we are inside a CONS in H2, we only create a mapping
;;; when one does not already exist.  This way, preference is given to
;;; atoms outside of any CONS that is common between E and R.
;;; Finally, we build T' recursively by traversing E.  When a mapping
;;; in H2 is found, we return it.  Otherwise we create a new concrete
;;; syntax tree for it.

;;; Given a CST, return a hash table mapping every CONS of the
;;; underlying raw expression to a corresponding CST.  Notice that a
;;; CONS cells can be the raw version of several CSTs, so the mapping
;;; is not unique.  In this case, we just pick the first corresponding
;;; CST we encounter.  By doing it this way, we also avoid infinite
;;; computations when the expression contains cycles.
(defun cons-table (cst &optional (table (make-hash-table :test #'eq)))
  (labels ((traverse (cst)
             (when (and (consp cst) (cl:null (gethash (raw cst) table)))
               (setf (gethash (raw cst) table) cst)
               (traverse (first cst))
               (traverse (rest cst)))))
    (traverse cst))
  table)

;;; Given an expression E and a hash table H1 mapping CONS cells to
;;; CSTs, return a new EQL hash table H2 that contains the subset of
;;; the mappings of H1 with keys in E.
(defun referenced-cons-table (expression cons-table)
  (let ((table (make-hash-table :test #'eql))
        (seen (make-hash-table :test #'eq)))
    (labels ((traverse (expression)
               (when (and (cl:consp expression)
                          (not (gethash expression seen)))
                 (setf (gethash expression seen) t)
                 (let ((original-cst (gethash expression cons-table)))
                   (unless (cl:null original-cst)
                     (setf (gethash expression table) original-cst)))
                 (traverse (car expression))
                 (traverse (cdr expression)))))
      (traverse expression))
    table))

;;; Given a CST and a table containing mappings of some of the CONSes
;;; in the CST, add the atoms of the CST as mappings to the table.
;;; Mappings are added so that, when there are two or more EQL atoms
;;; in the CST, then priority is given to one of the atoms that is
;;; defined OUTSIDE one of the CONSes already in the table.
(defun add-atoms (cst table)
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((traverse (cst inside-p)
               (if (consp cst)
                   (unless (gethash cst seen)
                     (setf (gethash cst seen) t)
                     (let ((new-inside-p (or (gethash (raw cst) table)
                                             inside-p)))
                       (traverse (first cst) new-inside-p)
                       (traverse (rest cst) new-inside-p)))
                   (when (atom cst)
                     (if inside-p
                         (when (not (nth-value 1 (gethash (raw cst) table)))
                           (setf (gethash (raw cst) table) cst))
                         (setf (gethash (raw cst) table) cst))))))
      (traverse cst nil)))
  table)

;;; Given an expression and a hash table mapping expressions to CSTs,
;;; build a CST from the expression in such a way that if an
;;; expression is encountered that has a mapping in the table, then
;;; the corresponding CST in the table is used.
(defun build-cst (expression table default-source)
  (let ((cons-table (make-hash-table :test #'eq)))
    (labels ((traverse (expression)
               (multiple-value-bind (value found-p)
                   (gethash expression table)
                 (cond
                   (found-p
                    value)
                   ((cl:consp expression)
                    (multiple-value-bind (existing found-p)
                        (gethash expression cons-table)
                      (if found-p
                          existing
                          (let ((cst (make-instance 'cons-cst
                                                    :raw expression
                                                    :source default-source)))
                            (setf (gethash expression cons-table) cst)
                            (let ((first (traverse (car expression)))
                                  (rest (traverse (cdr expression))))
                              (reinitialize-instance cst
                                                     :first first
                                                     :rest rest))
                            cst))))
                   (t
                    (make-instance 'atom-cst
                                   :raw expression
                                   :source default-source))))))
      (traverse expression))))

(defmethod reconstruct (expression (cst cst) client &key default-source)
  (let* ((cons-table (cons-table cst))
         (referenced-cons-table (referenced-cons-table expression cons-table)))
    (add-atoms cst referenced-cons-table)
    (build-cst expression referenced-cons-table default-source)))

(defmethod reconstruct (expression (cst cl:sequence) client &key default-source)
  (let* ((cons-table (reduce #'cons-table cst
                             :initial-value (make-hash-table :test #'eq)
                             :from-end t))
         (referenced-cons-table (referenced-cons-table expression cons-table)))
    (reduce #'add-atoms cst :initial-value referenced-cons-table :from-end t)
    (build-cst expression referenced-cons-table default-source)))
