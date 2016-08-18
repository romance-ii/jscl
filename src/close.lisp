(defpackage :close
  (:use :cl :jscl/ffi)
  (:export #:defgeneric #:defmethod #:defclass))

(defun methods-var-name (name)
  (intern (concatenate 'string
                       (symbol-package name) "%"
                       (symbol-name name) "%METHODS")
          (find-package :close)))

(defun dispatch-generic-function (name &rest arguments)
  `(block dispatch
     (dolist (method ,(methods-var-name name))
       (destructuring-bind (specializations function) method
         (when (every #'typep ,arguments specializations)
           (return-from dispatch (apply function arguments)))))
     (error "No method specialization for ~s matches argument types ~s~%~s"
            ',name (mapcar #'type-of arguments) arguments)))

(defmacro defgeneric (name lambda-list)
  (let ((methods (methods-var-name name)))
    `(progn
       (defvar ,methods ())
       (defun ,name ,lambda-list (dispatch-generic-function ,methods ,@lambda-list)))))

(defmacro defmethod (name lambda-list &rest body)
  (let ((specializations (mapcar (lambda (entry)
                                   (if (consp entry) (second entry) t))
                                 lambda-list))
        (bindings (mapcar (lambda (entry)
                            (if (consp entry) (first entry) entry)
                            lambda-list))))
    `(push (list ,specializations
                 (lambda ,bindings
                   ,@body))
           ,(methods-var-name name))))
