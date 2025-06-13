(cl:defpackage #:concrete-syntax-tree.tools.write-changelog
  (:use
   #:cl)

  (:local-nicknames
   (#:ti #:concrete-syntax-tree.tools.texinfo)
   (#:rc #:concrete-syntax-tree.tools.read-changes))

  (:export
   #:emit-changelog))

(cl:in-package #:concrete-syntax-tree.tools.write-changelog)

(defmacro define-emitter (name (keyword &rest lambda-list) &body body)
  `(defun ,name (node stream)
     (destructuring-bind (keyword ,@lambda-list) node
       (assert (eq keyword ,keyword))
       ,@body)))

(define-emitter emit-paragraph (:paragraph &rest content)
  (pprint-logical-block (stream content)
    (labels ((write-content (content)
               (loop :for (chunk next) :on content
                     :do (etypecase chunk
                           ((cons (eql :when))
                            (when (equal (second chunk) "manual")
                              (write-content (nthcdr 2 chunk))))
                           ((cons (member :symbol :tt))
                            (ti:write-code (second chunk) stream))
                           ((cons (eql :ref))
                            (destructuring-bind (keyword namespace target) chunk
                              (declare (ignore keyword))
                              (ecase namespace
                                (:figure (format stream "@ref{fig:~A}" target))
                                (:section (format stream "@xref{~A}" target)))))
                           (string
                            (ti:write-escaped chunk stream)))
                     :when (and next
                                (not (eq (rc:punctuationp next) t))
                                (not (eq (rc:punctuationp chunk) :open)))
                       :do (write-string " " stream)
                           (pprint-newline :fill stream))))
      (write-content content)))
  (format stream "~@:_~@:_"))

(define-emitter emit-code (:code language content)
  (ti:code (stream :language language)
    (write-string content stream)))

(define-emitter emit-item (:item &rest children)
  (ti:item (stream) nil
    (mapc (lambda (child)
            (etypecase child
              ((cons (eql :paragraph)) (emit-paragraph child stream))
              ((cons (eql :code)) (emit-code child stream))))
          children)))

(define-emitter emit-release (:release version date &rest items)
  (ti:item (stream)
    (lambda (stream)
      (format stream "Release ~A (~:[not yet released~;~:*~A~])" version date))
    (ti:itemize (stream)
      (mapc (lambda (item) (emit-item item stream)) items))))

(define-emitter emit-changelog (:changes &rest releases)
  (pprint-logical-block (stream node)
    (ti:write-section "Changelog" stream :kind :unnumbered)
    (ti:table (stream)
      (mapc (lambda (release) (emit-release release stream)) releases))))

;;; Entry point

(let ((changes (rc:read-changes (second sb-ext:*posix-argv*))))
  (emit-changelog changes *standard-output*))
