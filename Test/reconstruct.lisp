(cl:in-package #:concrete-syntax-tree-test)

(def-suite* :concrete-syntax-tree.reconstruct
  :in :concrete-syntax-tree)

(test reconstruct.smoke
  (let* ((expression-1 (cons 'a 'c))
         (expression-2 (list 'b expression-1))
         (cst (cst:cst-from-expression expression-1))
         (reconstructed (cst:reconstruct nil expression-2 cst)))
    (is (eq cst (cst:first (cst:rest reconstructed))))))

(test reconstruct.circular
  (let* ((circular (cst:cons (make-instance 'cst:atom-cst :raw nil)
                             (make-instance 'cst:atom-cst :raw nil)))
         (result (progn
                   (setf (slot-value circular 'cst::%first) circular)
                   (cst:reconstruct nil '#1=(#1#) circular))))
    (is-true (typep result 'cst:cst))
    (is (eq result (cst:first result)))))

(test reconstruct.sharing.same-structure
  "Test the sharing-related behavior of `reconstruct' in case the
structure of the CST is the same as the structure of the original
expression."
  (let* ((expression '(#1=1 #1# #2=#\a #2# #3=#P"foo" #3#))
         (cst (cst:cst-from-expression expression))
         (result (cst:reconstruct nil expression cst)))
    ;; This test can't work if `cst-from-expression' behaves
    ;; unexpectedly.
    (assert (not (eq (cst:first cst) (cst:second cst))))
    (assert (not (eq (cst:third cst) (cst:fourth cst))))
    (assert (eq (cst:fifth cst) (cst:sixth cst)))
    ;; Since the structure matches, RESULT should use the
    ;; corresponding `atom-cst's in CST.
    #.`(progn
         ,@(loop for reader in '(cst:first cst:second cst:third
                                 cst:fourth cst:fifth cst:sixth)
                 collect `(is (eq (,reader cst) (,reader result)))))
    ;; Distinct CSTs for numbers and characters, unique CSTs for other
    ;; atoms.
    (is (not (eq (cst:first result) (cst:second result))))
    (is (not (eq (cst:third result) (cst:fourth result))))
    (is (eq (cst:fifth result) (cst:sixth result)))))

(test reconstruct.sharing.shuffled
  "Test the sharing-related behavior of `reconstruct' in case the CST
contains the same atoms as the original expression but with the order
permuted."
  (let* ((pathname #P"foo")
         (expression `(#1=1 #1# #2=#\a #2# ,pathname ,pathname))
         (cst (cst:cst-from-expression expression))
         (shuffled-expression `(#11=1 ,pathname #12=#\a #12# ,pathname #11#))
         (result (cst:reconstruct nil shuffled-expression cst)))
    ;; For any atom, RESULT uses the last occurrence (according to the
    ;; traversal order) of a matching CST in EXPRESSION.
    (is (eq (cst:second cst) (cst:first result)))
    (is (eq (cst:second cst) (cst:sixth result)))
    (is (eq (cst:fourth cst) (cst:third result)))
    (is (eq (cst:fourth cst) (cst:fourth result)))
    (is (eq (cst:sixth cst) (cst:second result)))
    (is (eq (cst:sixth cst) (cst:fifth result)))
    ;; For things like numbers and characters, we can't really tell
    ;; whether occurrences in SHUFFLED-EXPRESSION refer to particular
    ;; occurrences in CST.
    (is (eq (cst:second result) (cst:fifth result)))))

(test reconstruct.sharing.unrelated
  "Test the sharing-related behavior of `reconstruct' in case the CST has
no relation to the original expression"
  (let* ((expression nil)
         (cst (cst:cst-from-expression expression))
         (unrelated-expression '(#1=1 #1# #2=#\a #2# #3=#P"foo" #3#))
         (result (cst:reconstruct nil unrelated-expression cst)))
    ;; Since none of the `atom-cst's in RESULT come from CST, we
    ;; expect distinct CSTs for numbers and characters, shared CSTs
    ;; for other atoms.
    (is (not (eq (cst:first result) (cst:second result))))
    (is (not (eq (cst:third result) (cst:fourth result))))
    (is (eq (cst:fifth result) (cst:sixth result)))))

(test reconstruct.long-list.same-structure
  "Test `reconstruct' on a long list with a CST that has the same
structure as the original expression."
  (let* ((expression (make-long-list))
         (unrelated-expression (make-long-list))
         (cst (cst:cst-from-expression expression))
         (result (cst:reconstruct nil unrelated-expression cst)))
    (assert-equality result unrelated-expression)))

(test reconstruct.long-list.unrelated
  "Test `reconstruct' on a long list with a CST that has no relation to
the original expression."
  (let* ((expression (make-long-list))
         (cst (cst:cst-from-expression expression))
         (result (cst:reconstruct nil expression cst)))
    (assert-equality result expression)))
