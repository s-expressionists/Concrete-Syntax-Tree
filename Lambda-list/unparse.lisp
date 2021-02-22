(in-package #:concrete-syntax-tree)

;;;; This file defines an "unparsing" system. Given a parsed lambda list, a
;;;; lambda list intended for human consumption is returned. The unparse
;;;; should resemble the originally parsed lambda list, but there is no
;;;; exactness requirement.
;;;; This is useful for displaying lambda lists to a user,
;;;; for example in error reports.

(defgeneric unparse-lambda-list (client lambda-list))

(defgeneric unparse-parameter-group (client parameter-group))

(defgeneric unparse-parameter (client parameter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod unparse-lambda-list (client (lambda-list lambda-list-type))
  (loop for parameter-group in (children lambda-list)
        appending (unparse-parameter-group client parameter-group)))

;;;

(defmethod unparse-parameter-group
    (client (parameter-group singleton-parameter-group))
  `(,(raw (name (keyword parameter-group)))
    ,(unparse-parameter client (parameter parameter-group))))

(defmethod unparse-parameter-group
    (client (parameter-group implicit-parameter-group))
  (loop for parameter in (parameters parameter-group)
        collect (unparse-parameter client parameter)))

(defmethod unparse-parameter-group
    (client (parameter-group explicit-multi-parameter-group))
  `(,(raw (name (keyword parameter-group)))
    ,@(loop for parameter in (parameters parameter-group)
            collect (unparse-parameter client parameter))))

(defmethod unparse-parameter-group
    (client (parameter-group key-parameter-group))
  `(,(raw (name (keyword parameter-group)))
    ,@(loop for parameter in (parameters parameter-group)
            collect (unparse-parameter client parameter))
    ,@(when (allow-other-keys parameter-group) '(&allow-other-keys))))

;;; &aux parameters don't affect parsing errors, so we skip them.
(defmethod unparse-parameter-group
    (client (parameter-group aux-parameter-group))
  (declare (ignore client))
  nil)

;;;

(defmethod unparse-parameter (client (parameter simple-variable))
  (declare (ignore client))
  (raw (name parameter)))

;;; Since unparsing is basically used for error reports at runtime,
;;; the specializer is probably not relevant.
(defmethod unparse-parameter (client (parameter specialized-required-parameter))
  (declare (ignore client))
  (raw (name parameter)))

(defmethod unparse-parameter (client (parameter ordinary-optional-parameter))
  (declare (ignore client))
  (raw (name parameter)))

(defmethod unparse-parameter
    (client (parameter generic-function-optional-parameter))
  (declare (ignore client))
  (raw (name parameter)))

(defmethod unparse-parameter (client (parameter ordinary-key-parameter))
  (declare (ignore client))
  (let ((rname (raw (name parameter)))
        (rkeyword (raw (keyword parameter))))
    (if (and (keywordp rkeyword)
             (string= (symbol-name rname) (symbol-name rkeyword)))
        ;; The keyword corresponds to the variable, so they don't
        ;; both need to be in the unparse.
        rname
        ;; The keyword was custom.
        `((,rkeyword ,rname)))))

(defmethod unparse-parameter (client (parameter aux-parameter))
  (declare (ignore client))
  (raw (name parameter)))

(defmethod unparse-parameter
    (client (parameter destructuring-optional-parameter))
  (unparse-lambda-list client (name parameter)))

(defmethod unparse-parameter (client (parameter destructuring-key-parameter))
  `((,(raw (keyword parameter))
     ,(unparse-lambda-list client (name parameter)))))

(defmethod unparse-parameter (client (parameter destructuring-lambda-list))
  (unparse-lambda-list client parameter))
