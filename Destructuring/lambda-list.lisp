(cl:in-package #:concrete-syntax-tree)

(defmethod parameter-groups-bindings
    (client (parameter-groups cl:null) argument-variable)
  (values `((,argument-variable
             (if (cl:null ,argument-variable)
                 ,argument-variable
                 ,(too-many-arguments-error client *current-lambda-list*
                                            argument-variable
                                            *current-macro-name*))))
          (cl:list argument-variable)))

(defmethod parameter-groups-bindings
    (client (parameter-groups cl:cons) argument-variable)
  (loop with all-binds = nil
        with all-ignorables = (cl:list argument-variable)
        with too-many-args-bindings
          = (if (some (lambda (pg)
                        (parameter-group-varargs-p client pg))
                      parameter-groups)
                `()
                `((,argument-variable
                   (if (cl:null ,argument-variable)
                       ,argument-variable
                       ,(too-many-arguments-error client *current-lambda-list*
                                                  argument-variable
                                                  *current-macro-name*)))))
        for parameter-group in parameter-groups
        do (multiple-value-bind (binds ignorables)
               (parameter-group-bindings client parameter-group
                                         argument-variable)
             (setf all-binds (append all-binds binds)
                   all-ignorables (append ignorables all-ignorables)))
        finally (return
                  (values (append all-binds too-many-args-bindings)
                          all-ignorables))))

(defmethod destructuring-lambda-list-bindings
    (client (lambda-list macro-lambda-list) argument-variable)
  (let (;; Make sure *current-lambda-list* is bound, but allow callers to
        ;; make it some other lambda list if they want by not overriding.
        (*current-lambda-list* (if (boundp '*current-lambda-list*)
                                   *current-lambda-list*
                                   lambda-list)))
    (parameter-groups-bindings client (children lambda-list)
                               argument-variable)))

(defmethod destructuring-lambda-list-bindings
    (client (lambda-list destructuring-lambda-list) argument-variable)
  (let ((*current-lambda-list* lambda-list))
    (parameter-groups-bindings client (children lambda-list)
                               argument-variable)))
