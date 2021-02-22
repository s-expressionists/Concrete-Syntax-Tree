(cl:in-package #:concrete-syntax-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions
;;;
;;; These are called by parse-macro, destructuring-lambda-list-bindings, etc.
;;; to produce code to report errors at runtime, i.e. when the lambda list does
;;; not match the given arguments. Methods on these functions should return
;;; forms that will signal the appropriate kind of error at runtime.
;;; These forms must not return normally.

(defgeneric too-many-arguments-error (client lambda-list
                                      argument-variable macro-name))

(defgeneric too-few-arguments-error (client lambda-list
                                     argument-variable macro-name))

(defgeneric odd-keywords-error
    (client lambda-list argument-variable macro-name))

(defgeneric unknown-keywords-error
    (client lambda-list argument-variable unknown-keywords macro-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default methods

(defmethod too-many-arguments-error (client lambda-list
                                     argument-variable macro-name)
  (if (cl:null macro-name)
      `(error "Too many elements in~%~2t~a~%to satisfy lambda list~%~2t~a"
              ,argument-variable ',(unparse-lambda-list client lambda-list))
      `(error "Error while parsing arguments to ~a:
~2tToo many elements in~4t~a~%~2tto satisfy lambda list~%~4t~a"
          ',macro-name ,argument-variable
          ',(unparse-lambda-list client lambda-list))))

(defmethod too-few-arguments-error (client lambda-list
                                     argument-variable macro-name)
  (if (cl:null macro-name)
      `(error "Too few elements in~%~2t~a~%to satisfy lambda list~%~2t~a"
              ,argument-variable ',(unparse-lambda-list client lambda-list))
      `(error "Error while parsing arguments to ~a:
~2tToo few elements in~4t~a~%~2tto satisfy lambda list~%~4t~a"
          ',macro-name ,argument-variable
          ',(unparse-lambda-list client lambda-list))))

(defmethod odd-keywords-error (client lambda-list
                               argument-variable macro-name)
  (if (cl:null macro-name)
      `(error "The keyword portion of ~a has an odd length, where keywords were expected:~%~2t~a"
              ,argument-variable
              ',(unparse-lambda-list client lambda-list))
      `(error "Error while parsing arguments to ~a:
~2tThe keyword portion of ~a has an odd length, where keywords were expected:~%~4t~a"
              ',macro-name
              ,argument-variable
              ',(unparse-lambda-list client lambda-list))))

(defmethod unknown-keywords-error (client lambda-list
                                   argument-variable unknowns macro-name)
  (if (cl:null macro-name)
      `(error "Unknown keywords ~a in~%~2t~a~%for lambda list~%~2t~a"
              ,unknowns ,argument-variable
              ',(unparse-lambda-list client lambda-list))
      `(error "Error while parsing arguments to ~a:
~2tUnknown keywords ~a in~%~4t~a~%~2tfor lambda list~%~4t~a"
              ',macro-name ,unknowns ,argument-variable
              ',(unparse-lambda-list client lambda-list))))
