(cl:in-package #:concrete-syntax-tree)

(defconstant +recursion-depth-limit+ 1000)

;;; Evaluate BODY in a lexical environment in which
;;; * ENQUEUE-NAME is bound to a function of one argument that enqueues a
;;;   work item for later processing
;;; * PROCESS-NAME is bound to a do-style iteration macro for
;;;   processing queued work items with the following syntax
;;;
;;;     (PROCESS-NAME (item)
;;;       (do-something item))
;;;
;;; BODY should start by attempting to perform the intended processing
;;; recursively and enqueue work items when the recursion depth
;;; reaches `+recursion-depth-limit+'.  After that, BODY use
;;; PROCESS-NAME to process queued work items. This processing may add
;;; new work items if the maximum recursion depth is reached again.
;;;
;;; Thus a typical use looks like
;;;
;;;   (with-bounded-recursion (enqueue do-work)
;;;     (labels ((process-recursively (thing depth)
;;;                (let ((sub-thing ...))
;;;                  (if (< depth +recursion-depth-limit+)
;;;                      (process-recursively sub-thing (1+ depth))
;;;                      (enqueue sub-thing)))))
;;;       (process-recursively root-thing 0)
;;;       (do-work (item)
;;;         (process-recursively item 0))))
;;;
;;; For typical, small inputs, the worklist will remain empty and the
;;; DO-WORK loop will exit without a single iteration.
(defmacro with-bounded-recursion ((enqueue-name process-name
                                   &optional (worklist-var (gensym "WORKLIST")))
                                  &body body)
  (let ((tail (gensym "TAIL")))
    `(let ((,tail nil) (,worklist-var ()))
       (flet ((,enqueue-name (item)
                (let ((cell (cl:cons item nil)))
                  (if (cl:null ,tail)
                      (setf ,worklist-var cell)
                      (setf (cdr ,tail) cell))
                  (setf ,tail cell))))
         (declare (dynamic-extent (function ,enqueue-name)))
         (macrolet ((,process-name ((item-name) &body body)
                      `(loop until (cl:null ,',worklist-var)
                             for ,item-name = (pop ,',worklist-var)
                             do (progn ,@body))))
           ,@body)))))
