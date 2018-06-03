(cl:in-package #:concrete-syntax-tree)

;;; For any CST, determine its structure as represented by successive
;;; REST values.  Return the result as two values the first value
;;; contains the number of unique CONS-CSTs in the chain, and the
;;; second value is one of the keywords :proper, :dotted, and
;;; :circular.  For an ATOM-CST, 0 and :dotted is returned.
;;;
;;; This function is useful for processing code because lists
;;; representing code are not often very long, so the method used is
;;; fast and appropriate, and because we often need to check that such
;;; lists are proper, but the simple method would go into an infinite
;;; computation if the list is circular, whereas we would like to give
;;; an error message in that case.
(defun list-structure (cst)
  ;; First we attempt to just traverse the CST as usual, assuming that
  ;; it is fairly short.  If we reach the end, then that's great, and
  ;; we return the result.
  (loop for remaining = cst then (rest remaining)
	for count from 0 to 100
	while (consp remaining)
	finally (cond ((null remaining)
                       (return-from list-structure
                         (values count :proper)))
                      ((atom remaining)
                       (return-from list-structure
                         (values count :dotted)))
                      (t nil)))
  ;; Come here if the list has more than a few CONS-CSTs.  We traverse
  ;; it again, this time entering each CONS-CST in a hash table.  Stop
  ;; when we reach the end of the chain, or when we see the same
  ;; CONS-CST twice.
  (let ((table (make-hash-table :test #'eq)))
    (loop for remaining = cst then (rest remaining)
	  while (consp remaining)
	  until (gethash remaining table)
	  do (setf (gethash remaining table) t)
	  finally (return (values (hash-table-count table)
                                  (cond ((null remaining) :proper)
                                        ((atom remaining) :dotted)
                                        (t :circular)))))))

(defun proper-list-p (cst)
  (eq (nth-value 1 (list-structure cst)) :proper))
