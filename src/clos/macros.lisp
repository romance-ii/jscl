;;; -*- mode:lisp; coding:utf-8 -*-

;;;
;;; CLOS macros
;;; Modification for JSCL  @vlad-km, 2019
;;;
;;; JSCL compilation mode :target
;;;

(in-package :jscl/impl)

;;; defclass
;;; from std-object.lisp
;;; from original closette.lisp lines 370-383
(defmacro jscl/cl::defclass (name direct-superclasses direct-slots &rest options)
  `(ensure-class ',name
                 :direct-superclasses ,(canonicalize-direct-superclasses direct-superclasses)
                 :direct-slots ,(canonicalize-direct-slots direct-slots)
                 ,@(canonicalize-defclass-options options)))


;;; defgeneric
;;; from std-generic.lisp
;;; from original closette.lisp lines 825-832
(defmacro jscl/cl::defgeneric (function-name lambda-list &rest options)
  `(!ensure-generic-function ',function-name
                             :lambda-list ,(canonicalize-defgeneric-ll lambda-list)
                             ,@(canonicalize-defgeneric-options options)))


;;; defmethod
;;; from std-method.lisp
;;; from original closette.lisp lines 919-931
;;; @vlad-km. modify :body. added :cmf
(defmacro jscl/cl::defmethod (&rest args)
  (multiple-value-bind (function-name qualifiers lambda-list specializers body)
      (parse-defmethod args)
    `(let ((gf (find-generic-function ',function-name)))
       (ensure-method gf
                      :lambda-list ,(canonicalize-defgeneric-ll lambda-list)
                      :qualifiers ,(canonicalize-defgeneric-ll qualifiers)
                      :specializers ,(canonicalize-specializers specializers)
                      :body ',body
                      :cmf (compile-method-function ,function-name
                                                    ,(kludge-arglist lambda-list)
                                                    ,body)))))

;;; @vlad-km
;;; added standard macro - with-slots
(defmacro jscl/cl::with-slots ((&rest slots) instance-name &body forms)
  (let ((instance (gensym)))
    `(let ((,instance ,instance-name))
       (symbol-macrolet
           ,(loop for slot-name in slots
               collect (if (symbolp slot-name)
                           `(,slot-name (!slot-value ,instance ',slot-name))
                           `(,(first slot-name) (!slot-value ,instance ',(second slot-name)))))
         ,@forms))))


;;; @vlad-km
;;; added standard macro - with-accessors
(defmacro jscl/cl::with-accessors ((&rest Readers) instance-name &body forms)
  (let ((instance (gensym)))
    `(let ((,instance ,instance-name))
       (symbol-macrolet
           ,(loop for (var reader) in Readers
               collect `(,var (,reader ,instance)))
         ,@forms))))




;;; psetf
;;: todo: remove to ?
#+nil
(defmacro psetf (&rest pairs)
  `(!psetf ,@pairs))

;;; rotatef
;;: todo: remove to ?
#+nil
(defmacro rotatef (&rest assigments)
  `(!rotatef ,@assigments))



;;; FSET section
(jscl::fset 'jscl/cl::class-of (fdefinition '!class-of))
(jscl::fset 'jscl/cl::class-name (fdefinition '!class-name))
(jscl::fset 'jscl/cl::find-class (fdefinition '!find-class))
(jscl::fset 'jscl/cl::slot-value (fdefinition '!slot-value))
(jscl::fset 'jscl/cl::slot-boundp (fdefinition '!slot-boundp))
(jscl::fset 'jscl/cl::slot-makunbound  (fdefinition '!slot-makunbound))
(jscl::fset 'jscl/cl::slot-exists-p  (fdefinition '!slot-exists-p))
(jscl::fset 'jscl/cl::ensure-generic-function (fdefinition '!ensure-generic-function))
(jscl::fset 'jscl/cl::find-method (fdefinition '!find-method))
(jscl::fset 'jscl/cl::add-method (fdefinition '!add-method))
(jscl::fset 'jscl/cl::remove-method (fdefinition '!remove-method))
(jscl::fset 'jscl/cl::method-qualifiers (fdefinition '!method-qualifiers))
(jscl::fset 'jscl/cl::method-specializers (fdefinition '!method-specializers))


(push :mop jscl/cl::*features*)

;;; EOF
