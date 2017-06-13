(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defparameter *variables*
  '(a b c d e f g h i j k l m n o p q r s u v w x y z))

(defun random-variable ()
  (elt *variables* (random (length *variables*))))

(defun random-form ()
  (list (random-variable) (random-variable)))

(defun random-ordinary-required-parameter-group ()
  (loop repeat (random 5)
        collect (random-variable)))

(defun random-ordinary-optional-parameter ()
  (let ((x (random 1d0)))
    (cond ((< x 0.25d0)
           (random-variable))
          ((< x 0.5d0)
           (list (random-variable)))
          ((< x 0.75d0)
           (list (random-variable) (random-form)))
          (t
           (list (random-variable) (random-form) (random-variable))))))

(defun random-ordinary-optional-parameter-group ()
  (let ((x (random 1d0)))
    (if (< x 0.25d0)
        '()
        (cons '&optional
              (loop repeat (random 5)
                    collect (random-ordinary-optional-parameter))))))

(defun random-ordinary-rest-parameter-group ()
  (let ((x (random 1d0)))
    (if (< x 0.25d0)
        '()
        (list '&rest (random-variable)))))
  
(defun random-key-variable ()
  (let ((x (random 1d0)))
    (if (< x 0.5d0)
        (random-variable)
        (list (random-variable) (random-variable)))))

(defun random-ordinary-key-parameter ()
  (let ((x (random 1d0)))
    (cond ((< x 0.25d0)
           (random-variable))
          ((< x 0.5d0)
           (list (random-key-variable)))
          ((< x 0.75d0)
           (list (random-key-variable) (random-form)))
          (t
           (list (random-key-variable) (random-form) (random-variable))))))

(defun random-ordinary-key-parameter-group ()
  (let ((x (random 1d0))
        (y (random 1d0)))
    (if (< x 0.25d0)
        '()
        (cons '&key
              (append (loop repeat (random 5)
                            collect (random-ordinary-key-parameter))
                      (if (< y 0.5d0)
                          '()
                          '(&allow-other-keys)))))))

(defun random-aux-parameter ()
  (let ((x (random 1d0)))
    (cond ((< x 0.33d0)
           (random-variable))
          ((< x 0.66d0)
           (list (random-variable)))
          (t
           (list (random-variable) (random-form))))))

(defun random-aux-parameter-group ()
  (let ((x (random 1d0)))
    (if (< x 0.25d0)
        '()
        (cons '&aux
              (loop repeat (random 5)
                    collect (random-aux-parameter))))))

(defun random-ordinary-lambda-list ()
  (append (random-ordinary-required-parameter-group)
          (random-ordinary-optional-parameter-group)
          (random-ordinary-rest-parameter-group)
          (random-ordinary-key-parameter-group)
          (random-aux-parameter-group)))

(defun random-generic-function-optional-parameter ()
  (let ((x (random 1d0)))
    (cond ((< x 0.5d0)
           (random-variable))
          (t
           (list (random-variable))))))

(defun random-generic-function-optional-parameter-group ()
  (let ((x (random 1d0)))
    (if (< x 0.25d0)
        '()
        (cons '&optional
              (loop repeat (random 5)
                    collect (random-generic-function-optional-parameter))))))

(defun random-generic-function-key-parameter ()
  (let ((x (random 1d0)))
    (cond ((< x 0.5d0)
           (random-variable))
          (t
           (list (random-key-variable))))))

(defun random-generic-function-key-parameter-group ()
  (let ((x (random 1d0))
        (y (random 1d0)))
    (if (< x 0.25d0)
        '()
        (cons '&key
              (append (loop repeat (random 5)
                            collect (random-generic-function-key-parameter))
                      (if (< y 0.5d0)
                          '()
                          '(&allow-other-keys)))))))

(defun random-generic-function-lambda-list ()
  (append (random-ordinary-required-parameter-group)
          (random-generic-function-optional-parameter-group)
          (random-ordinary-rest-parameter-group)
          (random-generic-function-key-parameter-group)))

(defun random-specialized-required-parameter ()
  (let ((x (random 1d0)))
    (cond ((< x 0.25d0)
           (random-variable))
          ((< x 0.5d0)
           (list (random-variable)))
          ((< x 0.75d0)
           (list (random-variable) (random-variable)))
          (t
           (list (random-variable)
                 (list 'eql (random-form)))))))

(defun random-specialized-required-parameter-group ()
  (loop repeat (random 5)
        collect (random-specialized-required-parameter)))

(defun random-specialized-lambda-list ()
  (append (random-specialized-required-parameter-group)
          (random-ordinary-optional-parameter-group)
          (random-ordinary-rest-parameter-group)
          (random-ordinary-key-parameter-group)
          (random-aux-parameter-group)))

(defun random-environment-parameter-group ()
  (let ((x (random 1d0)))
    (if (< x 0.25d0)
        '()
        (list '&environment (random-variable)))))
  
(defun random-defsetf-lambda-list ()
  (append (random-ordinary-required-parameter-group)
          (random-ordinary-optional-parameter-group)
          (random-ordinary-rest-parameter-group)
          (random-ordinary-key-parameter-group)
          (random-environment-parameter-group)))

(defun random-define-modify-macro-lambda-list ()
  (append (random-ordinary-required-parameter-group)
          (random-ordinary-optional-parameter-group)
          (random-ordinary-rest-parameter-group)))
