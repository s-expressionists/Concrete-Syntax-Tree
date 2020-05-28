(cl:in-package #:concrete-syntax-tree)

(defclass rule ()
  ((&left-hand-side :initarg :left-hand-side :reader left-hand-side)
   (&right-hand-side :initarg :right-hand-side :reader right-hand-side)))

(defmethod print-object ((object rule) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s <- " (left-hand-side object))
    (loop for symbol in (right-hand-side object)
          do (format stream "~s " symbol))
    (terpri stream)))

(defclass grammar ()
  ((%target-rule :initarg :target-rule :reader target-rule)
   (%rules :initarg :rules :reader rules)))

(defun nullable-p (right-hand-side-element)
  (and (cl:consp right-hand-side-element)
       (member (car right-hand-side-element) '(? *) :test #'eq)))

;;; Generate a grammar from a target and description, making sure to
;;; prune out all rules irrelevant to the target, so that no extra
;;; effort is expended while parsing. This function should only be
;;; called at grammar generation time.
(defun generate-grammar (target grammar-description)
  (let ((relevant-rule-descriptions '())
        (relevant-symbols (cl:list target))
        (seen-symbols '()))
    (loop (unless relevant-symbols
            (return))
          (let ((symbol (cl:pop relevant-symbols)))
            (unless (member symbol seen-symbols)
              (dolist (description grammar-description)
                (when (eq (car description) symbol)
                  (push description relevant-rule-descriptions)
                  (dolist (item (cddr description))
                    (push symbol seen-symbols)
                    (push (if (symbolp item)
                              item
                              (cl:second item))
                          relevant-symbols)))))))
    (make-instance 'grammar
                   :target-rule (make-instance 'rule
                                               :left-hand-side 'target
                                               :right-hand-side (cl:list target))
                   :rules (mapcar (lambda (rule-description)
                                    (make-instance 'rule
                                                   :left-hand-side (car rule-description)
                                                   :right-hand-side (cddr rule-description)))
                                  relevant-rule-descriptions))))
