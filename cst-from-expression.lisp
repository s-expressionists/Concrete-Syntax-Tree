(cl:in-package #:concrete-syntax-tree)

;;; Given EXPRESSION and a (possibly empty) hash-table mapping
;;; expressions to CSTs, build a CST from EXPRESSION in such a way
;;; that if a (sub-)expression is encountered that has a mapping in
;;; the table, then the corresponding CST in the table is used.
(defun cst-from-expression (expression &key source
                                            (expression->cst
                                             (make-hash-table :test #'eq)))
  ;; This function uses WITH-BOUNDED-RECURSION since, depending on the
  ;; structure of EXPRESSION, TRAVERSE calls could otherwise nest more
  ;; deeply than supported by the implementation.
  (let ((stack '()))
    (declare (type cl:list stack))
    (with-bounded-recursion (cst-from-expression enqueue do-work worklist)
      (labels ((finalize-cons-cst (cst first rest)
                 ;; Should we make this function user-extensible or
                 ;; let the user control the class of the created CSTs
                 ;; in some way, we would probably need something like
                 ;;
                 ;;   (reinitialize-instance cst :first (traverse car depth+1)
                 ;;                              :rest (traverse cdr depth+1))
                 ;;
                 ;; But setting the slots directly reduces the
                 ;; overall(!) runtime of a semi-realistic benchmark
                 ;; to around 30 %.
                 (setf (slot-value cst '%first) first
                       (slot-value cst '%rest) rest)
                 cst)
               (make-cons-cst (expression depth)
                 (declare (type (integer 0 #.+recursion-depth-limit+) depth))
                 (let ((car (car expression))
                       (cdr (cdr expression))
                       (cst (make-instance 'cons-cst :raw expression
                                                     :source source)))
                   (setf (gethash expression expression->cst) cst)
                   (cond ((< depth +recursion-depth-limit+)
                          (let ((depth+1 (1+ depth)))
                            (finalize-cons-cst
                             cst (traverse car depth+1) (traverse cdr depth+1))))
                         (t
                          ;; First and second work items: restart
                          ;; recursion for CAR and CDR and push
                          ;; results onto STACK.
                          (enqueue car)
                          (enqueue cdr)
                          ;; Third work item: pop results for CDR and
                          ;; CAR, update CST and push result onto
                          ;; STACK.
                          (enqueue
                           (lambda ()
                             (assert (>= (length stack) 2))
                             (let ((rest (pop stack))
                                   (first (pop stack)))
                               (finalize-cons-cst cst first rest))))))
                   ;; Return CST now so it can be added to its parent
                   ;; even though there may be a work item to update
                   ;; CST later.
                   cst))
               (traverse (expression depth)
                 (multiple-value-bind (existing-cst foundp)
                     (gethash expression expression->cst)
                   (cond (foundp
                          existing-cst)
                         ((cl:consp expression)
                          (make-cons-cst expression depth))
                         (t
                          ;; The was no existing CST for EXPRESSION,
                          ;; so create one.  But enter the new CST
                          ;; into EXPRESSION->CST only if its identity
                          ;; matters.
                          (let ((cst (make-instance 'atom-cst :raw expression
                                                              :source source)))
                            (if (typep expression '(or number
                                                       character
                                                       symbol))
                                cst
                                (setf (gethash expression expression->cst) cst))))))))
        (declare (inline finalize-cons-cst))
        (let ((cst (traverse expression 0)))
          (cond ((cl:null worklist)
                 ;; For small inputs, WORKLIST is not populated and
                 ;; STACK is not needed either.
                 cst)
                (t
                 ;; Since WORKLIST has been populated, we have use
                 ;; STACK as well.  We optimistically did not push CST
                 ;; onto STACK.  Do that now, then process the queued
                 ;; work items.
                 (push cst stack)
                 (do-work (work-item)
                   (format *trace-output* "~A ~30@<~V,,,'*<~>~> ~V,,,'*<~>~%"
                           'cst-from-expression (length worklist) (length stack))
                   (if (functionp work-item)
                       (funcall work-item)
                       (push (traverse work-item 0) stack)))
                 ;; After all work is done, the stack contents must
                 ;; have been reduced to a single element.
                 (assert (= (length stack) 1))
                 (cl:first stack))))))))
