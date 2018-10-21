;;;; builtins.lisp — Built-in primitives

;; JSCL is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General  Public License as published by the Free
;; Software Foundation,  either version  3 of the  License, or  (at your
;; option) any later version.
;;
;; JSCL is distributed  in the hope that it will  be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should  have received a  copy of  the GNU General  Public License
;; along with JSCL. If not, see <http://www.gnu.org/licenses/>.

(in-package #-jscl :jscl #+jscl :jscl/impl)

;;; Primitives

(defvar *builtins* (make-hash-table :test 'eql))

(defmacro define-raw-builtin (name args &body body)
  " Creates  a new  primitive function `name'  with parameters  args and
 @body.  The body  can access  to  the local  environment through  the
 variable *ENVIRONMENT*."
  `(setf (gethash ',name *builtins*)
         (lambda ,args
           (block ,name ,@body))))

(defmacro define-builtin (name args &body body)
  `(define-raw-builtin ,name ,args
     (let ,(mapcar (lambda (arg) `(,arg (convert ,arg))) args)
       ,@body)))

(define-raw-builtin + (&rest numbers)
  (if (null numbers)
      0
      (variable-arity numbers
        `(+ ,@numbers))))

(define-raw-builtin - (x &rest others)
  (let ((args (cons x others)))
    (variable-arity args `(- ,@args))))

(define-raw-builtin * (&rest numbers)
  (if (null numbers)
      1
      (variable-arity numbers `(* ,@numbers))))

(define-builtin logior (x y) (list 'logior x y))
(define-builtin logand (x y) (list 'logand x y))
(define-builtin logxor (x y) (list 'logxor x y))

(define-raw-builtin / (x &rest others)
  (let ((args (cons x others)))
    (variable-arity args
      (if (null others)
          `(jscl/js::call-internal |handled_division| 1 ,(car args))
          (reduce (lambda (x y) `(jscl/js::call-internal |handled_division| ,x ,y))
                  args)))))

(define-builtin mod (x y)
  (when (constantp y)
    (assert (not (zerop y))))
  `(jscl/js::selfcall
    (jscl/js::if (jscl/js::== ,y 0)
                 (jscl/js::throw (jscl/js::new |Error| "Division by zero")))
    (jscl/js::return (jscl/js::% ,x ,y))))

(defun comparison-conjuntion (vars op)
  (cond
    ((null (cdr vars))
     'jscl/js::true)
    ((null (cddr vars))
     `(,op ,(car vars) ,(cadr vars)))
    (t
     `(jscl/js::and (,op ,(car vars) ,(cadr vars))
                    ,(comparison-conjuntion (cdr vars) op)))))

(defmacro define-builtin-comparison (op &optional (sym op))
  `(define-raw-builtin ,op (x &rest args)
     (let ((args (cons x args)))
       (variable-arity args
         (convert-to-bool (comparison-conjuntion args ',sym))))))


(define-builtin-comparison >)
(define-builtin-comparison <)
(define-builtin-comparison >=)
(define-builtin-comparison <=)
(define-builtin-comparison = ==)
(define-builtin-comparison /= !=)

(define-builtin numberp (x)
  (convert-to-bool `(jscl/js::== (jscl/js::typeof ,x) "number")))

(define-builtin %floor (x)
  `(jscl/js::method-call |Math| "floor" ,x))  ; Should return two values

(define-builtin %ceiling (x)
  `(jscl/js::method-call |Math| "ceil" ,x))   ; Should return two values

(defmacro define-builtin-math (lisp-fn &optional
                                 (math-fn (string-downcase lisp-fn)))
  `(define-builtin ,lisp-fn (x)
     `(method-call |Math| ,,math-fn ,x)))

(define-builtin-math acos)
(define-builtin-math acosh)
(define-builtin-math asin)
(define-builtin-math atan)
(define-builtin-math atanh)
(define-builtin-math cos)
(define-builtin-math cosh)
(define-builtin-math log)
(define-builtin-math sin)
(define-builtin-math sinh)
(define-builtin-math sqrt)
(define-builtin-math tan)
(define-builtin-math tanh)

(define-builtin expt (base power)
  `(method-call |Math| "pow" ,base ,power))

(define-builtin float-to-string (x)
  `(jscl/js::call-internal |make_lisp_string| (jscl/js::method-call ,x |toString|)))

(define-builtin cons (x y)
  `(new (jscl/js::call-internal |Cons| ,x ,y)))

(define-builtin consp (x)
  (convert-to-bool `(instanceof ,x (internal |Cons|))))

(define-builtin car (x)
  `(jscl/js::call-internal |car| ,x))

(define-builtin cdr (x)
  `(jscl/js::call-internal |cdr| ,x))

(define-builtin rplaca (x new)
  `(jscl/js::selfcall
    (jscl/js::var tmp ,x)
    (jscl/js::= (jscl/js::get tmp "car") ,new)
    (jscl/js::return tmp)))

(define-builtin rplacd (x new)
  `(jscl/js::selfcall
    (jscl/js::var tmp ,x)
    (jscl/js::= (jscl/js::get tmp "cdr") ,new)
    (jscl/js::return tmp)))
(define-builtin symbolp (x)
  (convert-to-bool `(jscl/js::instanceof ,x (internal |Symbol|))))

(define-builtin make-symbol (name)
  `(jscl/js::new (jscl/js::call-internal |Symbol|
                                         (jscl/js::call-internal |lisp_to_js| ,name))))

(define-builtin set (symbol value)
  `(jscl/js::= (jscl/js::get ,symbol "value") ,value))

(define-builtin fset (symbol value)
  `(jscl/js::= (jscl/js::get ,symbol "fvalue")
               ,(if (and (listp value)
                         (= 2 (length value))
                         (eql 'quote (first value))
                         (symbolp (second value)))
                    (fdefinition value)
                    value)))

(define-builtin fset-setf (symbol value)
  `(jscl/js::= (jscl/js::get ,symbol "setfValue")
               ,(if (and (listp value)
                         (= 2 (length value))
                         (eql 'quote (first value))
                         (symbolp (second value)))
                    (fdefinition value)
                    value)))

(defmacro fset-macro (target source)
  (setf (macro-function target) (macro-function source)))

(define-builtin boundp (x)
  (convert-to-bool `(jscl/js::!== (jscl/js::get ,x "value") undefined)))


(define-builtin fboundp (x)
  (convert-to-bool `(jscl/js::!== (jscl/js::get ,x "fvalue") (jscl/js::internal |unboundFunction|))))

(define-builtin %fboundp-setf (x)
  (convert-to-bool `(jscl/js::!== (jscl/js::get ,x "setfValue") (jscl/js::internal |unboundSetFFunction|))))

(define-builtin symbol-value (x)
  `(jscl/js::call-internal |symbolValue| ,x))

(define-builtin %fdefinition-setf (accessor)
  `(jscl/js::get ,accessor "setfValue"))

(define-builtin %setf-fdefinition-setf (accessor function)
  `(jscl/js::= (jscl/js::get ,accessor "setfValue") ,function))


(define-builtin %setf-symbol-function (x fn)
  `(jscl/js::call-internal |setSymbolFunction| ,x ,fn))
(define-builtin symbol-function (x)
  `(jscl/js::call-internal |symbolFunction| ,x))
(define-builtin %fmakunbound (x)
  `(jscl/js::call-internal |fMakUnbound| ,x))
(define-builtin %fmakunbound-setf (x)
  `(jscl/js::call-internal |fMakUnboundSetF| ,x))

(define-builtin lambda-code (x)
  `(jscl/js::call-internal |make_lisp_string| (jscl/js::method-call ,x "toString")))

(define-builtin eq (x y)
  (convert-to-bool `(jscl/js::=== ,x ,y)))


(define-builtin char-code (x)
  `(jscl/js::call-internal |char_to_codepoint| ,x))

(define-builtin code-char (x)
  `(jscl/js::call-internal |char_from_codepoint| ,x))

(define-builtin characterp (x)
  `(jscl/js::selfcall
    (jscl/js::var x ,x)
    (jscl/js::return ,(convert-to-bool
                       `(jscl/js::and (jscl/js::== (jscl/js::typeof x) "string")
                                      (jscl/js::or (jscl/js::== (jscl/js::get x "length") 1)
                                                   (jscl/js::== (jscl/js::get x "length") 2)))))))


(define-builtin char-upcase (x)
  `(jscl/js::call-internal |safe_char_upcase| ,x))

(define-builtin char-downcase (x)
  `(jscl/js::call-internal |safe_char_downcase| ,x))

(define-builtin stringp (x)
  `(jscl/js::selfcall
    (jscl/js::var x ,x)
    (jscl/js::return ,(convert-to-bool
                       `
                       (jscl/js::and
                        (jscl/js::and
                         (jscl/js::=== (jscl/js::typeof x) "object")
                         (jscl/js::in "length" x))
                        (jscl/js::== (jscl/js::get x "stringp") 1))))))


(define-builtin ornate-object-class% (x)
  `(jscl/js::selfcall
    (jscl/js::var x ,x)
    (jscl/js::return (jscl/js::and (jscl/js::=== (jscl/js::typeof x) "object")
                                   (jscl/js::in "🏛" x)
                                   (jscl/js::get x "🏛")))))

(define-builtin set-ornate-object-class% (x class)
  `(jscl/js::selfcall
    (jscl/js::var x ,x)
    (jscl/js::return (jscl/js::and (jscl/js::=== (jscl/js::typeof x) "object")
                                   (jscl/js::in "🏛" x)
                                   (jscl/js::= (jscl/js::get x "🏛") ,(string class))))))


(define-raw-builtin funcall (func &rest args)
  `(jscl/js::selfcall
    (jscl/js::var f ,(convert func))
    (jscl/js::return (jscl/js::call
                      (jscl/js::if (jscl/js::=== (jscl/js::typeof f) "function")
                                   f
                                   (jscl/js::get f "fvalue"))
                      ,@(cons (if *multiple-value-p* '|values| '(internal |pv|))
                              (mapcar #'convert args))))))

(define-raw-builtin apply (func &rest args)
  (if (null args)
      (convert func)
      (let ((args (butlast args))
            (last (car (last args))))
        `(jscl/js::selfcall
          (jscl/js::var f ,(convert func))
          (jscl/js::var args ,(list-to-vector
                               (cons (if *multiple-value-p* '|values| '(internal |pv|))
                                     (mapcar #'convert args))))
          (jscl/js::var tail ,(convert last))
          (jscl/js::while (jscl/js::!= tail ,(convert nil))
            (jscl/js::method-call args "push" (jscl/js::get tail "car"))
            (jscl/js::= tail (jscl/js::get tail "cdr")))
          (jscl/js::return (jscl/js::method-call (if (jscl/js::=== (jscl/js::typeof f) "function")
                                                     f
                                                     (jscl/js::get f "fvalue"))
                                                 "apply"
                                                 this
                                                 args))))))

(define-builtin js-eval (string)
  (if *multiple-value-p*
      `(selfcall
        (jscl/js::var v (jscl/js::call-internal |globalEval|
                                                (jscl/js::call-internal |xstring|
                                                                        ,string)))
        (jscl/js::return (jscl/js::method-call |values| "apply"
                                               this (jscl/js::call-internal |forcemv| v))))
      `(jscl/js::call-internal |globalEval|
                               (jscl/js::call-internal |xstring| ,string))))

(define-builtin %throw (string)
  `(jscl/js::selfcall (throw ,string)))

(define-builtin functionp (x)
  (convert-to-bool `(jscl/js::=== (jscl/js::typeof ,x) "function")))



(define-builtin /debug (x)
  `(jscl/js::method-call |console| "log" (jscl/js::call-internal |xstring| ,x)))

(define-builtin /log (x)
  `(jscl/js::method-call |console| "log" ,x))


;;; Storage vectors. They are used to implement arrays and (in the
;;; future) structures. (work-in-progress, kinda.)

(define-builtin storage-vector-p (x)
  `(jscl/js::selfcall
    (jscl/js::var x ,x)
    (jscl/js::return ,(convert-to-bool
                       `(jscl/js::and (jscl/js::=== (jscl/js::typeof x) "object")
                                      (in "length" x)
                                      (in "svKind" x))))))

(define-builtin make-storage-vector (size kind)
  `(jscl/js::selfcall
    (jscl/js::var r #())
    (jscl/js::= (jscl/js::get r "length") ,size)
    (jscl/js::= (jscl/js::get r "svKind") ,kind)
    (jscl/js::return r)))
(define-builtin storage-vector-size (x)
  `(jscl/js::get ,x "length"))

(define-builtin resize-storage-vector (vector new-size)
  `(jscl/js::= (jscl/js::get ,vector "length") ,new-size))

(define-builtin storage-vector-ref (vector n)
  (when (constantp n)
    (check-type n (integer 0 *)))
  `(jscl/js::selfcall
    (jscl/js::var x (jscl/js::property ,vector ,n))
    (jscl/js::if (jscl/js::=== x jscl/js::undefined)
                 (jscl/js::throw
                     (jscl/js::new
                      (jscl/js::call
                       |Error|
                       ,(concatenate 'string "AREF "
                                     (princ-to-string n)
                                     "out of range for vector "
                                     (string vector))))))
    (jscl/js::return x)))



(define-builtin storage-vector-set (vector n value)
  `(jscl/js::selfcall
    (jscl/js::var x ,vector)
    (jscl/js::var i ,n)
    (jscl/js::if (jscl/js::or (< i 0)
                              (>= i (jscl/js::get x "length")))
                 (jscl/js::throw
                     (jscl/js::new
                      (jscl/js::call |Error|
                                     ,(concatenate
                                       'string
                                       "SETF AREF out of range for vector "
                                       (string vector))))))
    (jscl/js::return (jscl/js::= (jscl/js::property x i) ,value))))


(define-builtin concatenate-storage-vector (sv1 sv2)
  ;; TODO check kind?
  `(jscl/js::selfcall
    (jscl/js::var sv1 ,sv1)
    (jscl/js::var r (jscl/js::method-call sv1 "concat" ,sv2))
    (jscl/js::= (jscl/js::get r "svKind") (jscl/js::get sv1 "svKind"))
    (jscl/js::= (jscl/js::get r "stringp") (jscl/js::get sv1 "stringp"))
    (jscl/js::return r)))



(define-builtin get-internal-real-time ()
  `(jscl/js::method-call (jscl/js::new (jscl/js::call |Date|)) "getTime"))



(define-builtin values-array (array)
  (if *multiple-value-p*
      `(jscl/js::method-call |values| "apply" this ,array)
      `(jscl/js::method-call (internals |pv|) "apply" this ,array)))

(define-raw-builtin values (&rest args)
  (if *multiple-value-p*
      `(jscl/js::call |values| ,@(mapcar #'convert args))
      `(jscl/js::call-internal |pv| ,@(mapcar #'convert args))))


;;; Javascript FFI

(define-builtin new ()
  '(object))

(define-raw-builtin jscl/ffi::oget* (object key &rest keys)
  `(jscl/js::selfcall
    (jscl/js::progn
      (jscl/js::var tmp (jscl/js::property ,(convert object)
                                           (jscl/js::call-internal |xstring|
                                                                   ,(convert key))))
      ,@(mapcar (lambda (key)
                  `(jscl/js::progn
                     (jscl/js::if (jscl/js::=== tmp jscl/ffi::undefined)
                                  (jscl/js::return ,(convert nil)))
                     (jscl/js::= tmp (jscl/js::property tmp
                                                        (jscl/js::call-internal |xstring|
                                                                                ,(convert key))))))
                keys))
    (jscl/js::return (jscl/js::if (jscl/js::=== tmp undefined)
                                  ,(convert nil)
                                  tmp))))



(define-raw-builtin jscl/ffi::oset* (value object key &rest keys)
  (let ((keys (cons key keys)))
    `(jscl/js::selfcall
      (jscl/js::progn
        (jscl/js::var obj ,(convert object))
        ,@(mapcar (lambda (key)
                    `(jscl/js::progn
                       (jscl/js::= obj (jscl/js::property obj
                                                          (jscl/js::call-internal |xstring|
                                                                                  ,(convert key))))
                       (jscl/js::if (jscl/js::=== obj jscl/ffi::undefined)
                                    (jscl/js::throw "Impossible to set object property."))))
                  (butlast keys))
        (jscl/js::var tmp
                      (jscl/js::= (jscl/js::property obj
                                                     (jscl/js::call-internal |xstring|
                                                                             ,(convert (car (last keys)))))
                                  ,(convert value)))
        (jscl/js::return (jscl/js::if (jscl/js::=== tmp jscl/ffi::undefined)
                                      ,(convert nil)
                                      tmp))))))


(define-raw-builtin jscl/ffi::oget (object key &rest keys)
  `(jscl/js::call-internal |js_to_lisp| ,(convert `(jscl/ffi:oget* ,object ,key ,@keys))))

(define-raw-builtin jscl/ffi::oset (value object key &rest keys)
  (convert `(jscl/ffi::oset* (lisp-to-js ,value) ,object ,key ,@keys)))

(define-builtin js-null-p (x)
  (convert-to-bool `(jscl/js::=== ,x jscl/ffi::null)))

(define-builtin objectp (x)
  (convert-to-bool `(jscl/js::=== (jscl/js::typeof ,x) "object")))



(define-builtin %%nlx-p (x)
  (convert-to-bool `(jscl/js::call-internal |isNLX| ,x)))

(define-builtin %%throw (x)
  `(jscl/js::selfcall (jscl/js::throw ,x)))

(define-builtin lisp-to-js (x) `(jscl/js::call-internal |lisp_to_js| ,x))
(define-builtin js-to-lisp (x) `(jscl/js::call-internal |js_to_lisp| ,x))

(define-builtin in (key object)
  (convert-to-bool `(in (jscl/js::call-internal |xstring| ,key) ,object)))



(define-builtin delete-property (key object)
  `(selfcall
    (delete (jscl/js::property ,object (jscl/js::call-internal |xstring| ,key)))))

(define-builtin map-for-in (function object)
  `(selfcall
    (jscl/js::var f ,function
                  (g (if (jscl/js::=== (jscl/js::typeof f) "function") f (jscl/js::get f "fvalue")))
                  (o ,object)
                  key)
    (for-in (key o)
            (jscl/js::callg ,(if *multiple-value-p* '|values| '(internal |pv|))
                            (jscl/js::property o key)))
    (return ,(convert nil))))



;;; JavaScript primitive mapping of function calls

(define-js-macro selfcall (&body body)
  `(jscl/js::call (jscl/js::function () ,@body)))

(define-js-macro method-call (x method &rest args)
  `(jscl/js::call (jscl/js::get ,x ,method) ,@args))

(define-js-macro nargs ()
  `(- (jscl/js::get |arguments| |length|) 1))

(define-js-macro arg (n)
  `(jscl/js::property |arguments| (+ ,n 1)))


;;; Runtime

(define-js-macro internal (x)
  `(jscl/js::get |internals| ,x))

(define-js-macro call-internal (name &rest args)
  `(jscl/js::method-call |internals| ,name ,@args))

