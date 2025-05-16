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
;;; atoms outside of any CONS that is common between E and R, so that
;;; we get somewhat better source information for the atom in E which
;;; is not in a shared cons.
;;; Finally, we build T' recursively by traversing E, When a mapping
;;; in H2 is found, we return it.  Otherwise we create a new concrete
;;; syntax tree for it.

;;; Given a CST, return a hash table mapping every CONS of the
;;; underlying raw expression to a corresponding CST.  Notice that a
;;; CONS cells can be the raw version of several CSTs, so the mapping
;;; is not unique.  In this case, we just pick the first corresponding
;;; CST we encounter.  By doing it this way, we also avoid infinite
;;; computations when the expression contains cycles.
(defun cons-table (cst &optional (cons->cst (make-hash-table :test #'eq)))
  (with-bounded-recursion (enqueue do-work worklist)
    (labels ((traverse (cst depth)
               (declare (type (integer 0 #.+recursion-depth-limit+) depth))
               (when (consp cst)
                 (let ((raw (raw cst)))
                   (cond ((nth-value 1 (gethash raw cons->cst)))
                         ((< depth +recursion-depth-limit+)
                          (setf (gethash raw cons->cst) cst)
                          (let ((depth+1 (1+ depth)))
                            (traverse (first cst) depth+1)
                            ;; If we could inquire about tail call
                            ;; optimization, we could make this second
                            ;; call without increasing the depth in
                            ;; case of TCO.
                            (traverse (rest cst) depth+1)))
                         (t
                          (enqueue cst)))))))
      (traverse cst 0)
      (do-work (cst)
        (traverse cst 0))))
  cons->cst)

;;; Given an expression E and a hash table H1 mapping CONS cells to
;;; CSTs, return a new EQL hash table H2 that contains the subset of
;;; the mappings of H1 with keys in E.
(defun referenced-cons-table (expression cons->cst)
  (let ((referenced-cons->cst (make-hash-table :test #'eql))
        (seen (make-hash-table :test #'eq)))
    (with-bounded-recursion (enqueue do-work worklist)
      (labels ((traverse (expression depth)
                 (declare (type (integer 0 #.+recursion-depth-limit+) depth))
                 (when (and (cl:consp expression)
                            (not (gethash expression seen)))
                   (setf (gethash expression seen) t)
                   (multiple-value-bind (cst foundp)
                       (gethash expression cons->cst)
                     (cond ((not foundp)
                            (let ((car (car expression))
                                  (cdr (cdr expression)))
                              (cond ((< depth +recursion-depth-limit+)
                                     (let ((depth+1 (1+ depth)))
                                       (traverse car depth+1)
                                       ;; If we could inquire about
                                       ;; tail call optimization, we
                                       ;; could make this second call
                                       ;; without increasing the depth
                                       ;; in case of TCO.
                                       (traverse cdr depth+1)))
                                    (t
                                     (enqueue car)
                                     (enqueue cdr)))))
                           ;; We found EXPRESSION in CONS->CST so we
                           ;; don't need to traverse the
                           ;; sub-expressions of EXPRESSION since
                           ;; we'll always use or substitute the full
                           ;; cons when building the final CST.
                           ((cl:null cst))
                           (t
                            (setf (gethash expression referenced-cons->cst)
                                  cst)))))))
        (traverse expression 0)
        (do-work (work-item)
          (traverse work-item 0))))
    referenced-cons->cst))

;;; Given a CST and a table containing mappings of some of the CONSes
;;; in the CST, add the atoms of the CST as mappings to the table.
;;; Mappings are added so that, when there are two or more EQL atoms
;;; in the CST, then priority is given to one of the atoms that is
;;; defined OUTSIDE one of the CONSes already in the table.
(defun add-atoms (cst table)
  (let ((seen (make-hash-table :test #'eq)))
    (with-bounded-recursion (enqueue do-work worklist)
      (labels ((traverse (cst inside-p depth)
                 (declare (type (integer 0 #.+recursion-depth-limit+) depth))
                 (cond ((consp cst)
                        (unless (gethash cst seen)
                          (setf (gethash cst seen) t)
                          (let ((first (first cst))
                                (rest (rest cst)))
                            (cond ((< depth +recursion-depth-limit+)
                                   (let ((new-inside-p (or inside-p
                                                           (gethash (raw cst) table)))
                                         (depth+1 (1+ depth)))
                                     (traverse first new-inside-p depth+1)
                                     (traverse rest new-inside-p depth+1)))
                                  (t
                                   (enqueue first)
                                   (enqueue rest))))))
                       ((atom cst)
                        (let ((raw (raw cst)))
                          (when (or (not inside-p)
                                    (not (nth-value 1 (gethash raw table))))
                            (setf (gethash raw table) cst)))))))
        (traverse cst nil 0)
        (do-work (work-item)
          (traverse work-item nil 0)))))
  table)

(defmethod reconstruct ((client t) (expression t) (cst cst)
                        &key (default-source (source cst)))
  (let* ((cons-table (cons-table cst))
         (referenced-cons-table (referenced-cons-table expression cons-table)))
    (add-atoms cst referenced-cons-table)
    (cst-from-expression expression :source default-source
                                    :expression->cst referenced-cons-table)))

(defmethod reconstruct ((client t) (expression t) (cst cl:sequence)
                        &key default-source)
  (let* ((cons-table (reduce #'cons-table cst
                             :initial-value (make-hash-table :test #'eq)
                             :from-end t))
         (referenced-cons-table (referenced-cons-table expression cons-table)))
    (reduce #'add-atoms cst :initial-value referenced-cons-table :from-end t)
    (cst-from-expression expression :source default-source
                                    :expression->cst referenced-cons-table)))
