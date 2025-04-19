(cl:in-package #:concrete-syntax-tree)

(defun cst-from-expression (expression &key source)
  ;; This function uses WITH-BOUNDED-RECURSION since, depending on the
  ;; structure of EXPRESSION, TRAVERSE calls could otherwise nest more
  ;; deeply than supported by the implementation.
  (let ((seen (make-hash-table :test #'eq))
        (stack '()))
    (declare (type cl:list stack))
    (with-bounded-recursion (enqueue do-work worklist)
      (labels ((traverse (expression depth)
                 (declare (type (integer 0 #.+recursion-depth-limit+) depth))
                 (multiple-value-bind (existing-cst foundp)
                     (gethash expression seen)
                   (cond (foundp
                          existing-cst)
                         ((cl:atom expression)
                          (make-instance 'atom-cst :raw expression
                                                   :source source))
                         (t
                          (let ((car (car expression))
                                (cdr (cdr expression))
                                (cst (make-instance 'cons-cst :raw expression
                                                              :source source)))
                            (setf (gethash expression seen) cst)
                            (cond ((< depth +recursion-depth-limit+)
                                   (let* ((depth+1 (1+ depth))
                                          (first (traverse car depth+1))
                                          (rest (traverse cdr depth+1)))
                                     (reinitialize-instance cst :first first
                                                                :rest rest)))
                                  (t
                                   ;; First and second work items:
                                   ;; restart recursion for CAR and
                                   ;; CDR and push results onto STACK.
                                   (enqueue car)
                                   (enqueue cdr)
                                   ;; Third work item: pop results for
                                   ;; CDR and CAR, update CST and push
                                   ;; result onto STACK.
                                   (enqueue
                                    (lambda ()
                                      (assert (>= (length stack) 2))
                                      (let ((rest (pop stack))
                                            (first (pop stack)))
                                        (reinitialize-instance
                                         cst :first first :rest rest))))))
                            ;; Return CST now so it can be added to
                            ;; its parent even though there may be a
                            ;; work item to update CST later.
                            cst))))))
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
                   (if (functionp work-item)
                       (funcall work-item)
                       (push (traverse work-item 0) stack)))
                 ;; After all work is done, the stack contents must
                 ;; have been reduced to a single element.
                 (assert (= (length stack) 1))
                 (cl:first stack))))))))
