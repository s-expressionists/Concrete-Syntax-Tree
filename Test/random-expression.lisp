(cl:in-package #:concrete-syntax-tree-test)

(defun random-expression ()
  (labels ((aux (cons-probability nil-probability)
             (cond ((< (random 1d0) cons-probability)
                    (cons (aux (* cons-probability 0.8) nil-probability)
                          (aux (* cons-probability 0.8) (* 1.2 nil-probability))))
                   ((< (random 1d0) nil-probability)
                    nil)
                   ((< (random 1d0) 0.2)
                    (make-symbol (string (code-char (+ 65 (random 10))))))
                   ((< (random 1d0) 0.3)
                    (code-char (+ 97 (random 10))))
                   ((< (random 1d0) 0.4)
                    (+ 100000000000000000000000 (random 10)))
                   (t
                    (make-array 2)))))
    (aux 0.9 0.2)))

                    
