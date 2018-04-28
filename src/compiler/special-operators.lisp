;;;; special-operators.lisp — Compilation forms

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

(define-compilation jscl/cl::if (condition true &optional false)
  `(jscl/js::if (jscl/js::!== ,(convert condition) ,(convert nil))
                ,(convert true *multiple-value-p*)
                ,(convert false *multiple-value-p*)))

(define-compilation jscl/cl::setq (&rest pairs)
  (when (null pairs)
    (return-from jscl/cl::setq (convert nil)))
  (with-collector (result)
    (loop
       (cond
         ((null pairs)
          (return))
         ((null (cdr pairs))
          (error "Odd pairs in SETQ; dangling ~s" pairs))
         (t
          (collect-result (setq-pair (car pairs) (cadr pairs)))
          (setq pairs (cddr pairs)))))
    `(progn ,@result)))

(define-compilation jscl/cl::quote (sexp)
  (literal sexp))

(define-compilation %while (pred &rest body)
  `(jscl/js::selfcall
    (jscl/js::while (jscl/js::!== ,(convert pred) ,(convert nil))
      ,(convert-block body))
    (jscl/js::return ,(convert nil))))

(defun compile-named-lambda-form (form)
  (destructuring-bind (name ll &rest body) form
    (compile-lambda ll body
                    :name (function-namestring name)
                    :block (function-block-name name))))

(defun coerce-list-to-function (list)
  (case (car list)
    (lambda
        (compile-lambda (cadr list) (cddr list)))
    (jscl/impl::named-lambda
     (compile-named-lambda-form (cdr list)))
    (setf
     (let ((b (lookup-in-lexenv list *environment* 'function)))
       (if b
           (binding-value b)
           (error "No SETF function ~s" list))))
    (otherwise
     (error "Can't compile #'~s" list))))

(define-compilation jscl/cl::function (x)
  (cond
    ((listp x)
     (coerce-list-to-function x))
    ((symbolp x)
     (let ((b (lookup-in-lexenv x *environment* 'function)))
       (if b
           (binding-value b)
           (convert `(symbol-function ',x)))))
    (t (error "~s is not a function designator" x))))

(define-compilation jscl/cl::flet (definitions &rest body)
  (let* ((flet-fun-names (mapcar #'car definitions))
         (flet-compiled-funs (mapcar #'compiled-function-code definitions))
         (*environment* (environment+new-functions flet-fun-names)))
    `(jscl/js::call(function ,(mapcar #'translate-function flet-fun-names)
                             ,(convert-block body t))
                   ,@flet-compiled-funs)))

(define-compilation jscl/cl::labels (definitions &rest body)
  (let* ((label-fun-names (mapcar #'car definitions))
         (*environment* (environment+new-functions label-fun-names)))
    `(jscl/js::selfcall
      ,@(mapcar #'labels/compiled-label-function definitions)
      ,(convert-block body t))))

(define-compilation jscl/cl::eval-when (situations &rest body)
  "NOTE: It  is probably wrong  in many cases but  we will not  use this
 heavily. Please, do not rely on wrong cases of this implementation."
  ;; TODO: Error checking
  (assert (every (lambda (situation)
                   (find situation '(:compile-toplevel :load-toplevel :execute)))
                 situations)
          (situations)
          "Eval-When situations must be (MEMBER :COMPILE-TOPLEVEL~
 :LOAD-TOPLEVEL :EXECUTE) only; not ~s" situations)
  (cond
    ;; Toplevel form compiled by compile-file.
    ((and *compiling-file* (zerop *convert-level*))
     ;; If  the  situation  `compile-toplevel'  is given.  The  form  is
     ;; evaluated  at compilation-time.  This  probably  means it'll  be
     ;; evaluated  in the  host compiler,  which  is maybe  not what  we
     ;; usually want.
     (when (find :compile-toplevel situations)
       (warn "Eval-When Compile-Toplevel: OK, evaluating in compiler (~a) ~a…"
             (lisp-implementation-type)
             (truncate-string
              (substitute #\space #\newline (princ-to-string body))
              120))
       (map nil #'eval body))
     ;; `load-toplevel'  is  given,  then   just  compile  the  subforms
     ;; as usual.
     (when (find :load-toplevel situations)
       (warn "Eval-When Load-Toplevel: OK, pushing into code")
       (convert-toplevel (cons 'progn body) *multiple-value-p*))
     (unless (or (find :compile-toplevel situations)
                 (find :load-toplevel situations))
       (warn "Eval-When: During compilation, ignoring ~s" situations)))
    ((find :execute situations)
     (convert `(progn ,@body) *multiple-value-p*))
    ((find :compile-toplevel situations)
     (warn "Skipping EVAL-WHEN: ~@[not compiling~] ~@[not toplevel~] "
           (not *compiling-file*) (not (zerop *convert-level*))))
    (t
     (warn "EVAL-WHEN has no valie situation ~s~%(unreachable code ~s)"
           situations body)
     (convert nil))))

(define-compilation jscl/cl::progn (&rest body)
  (if (null (cdr body))
      (convert (car body) *multiple-value-p*)
      `(jscl/js::progn
         ,@(append (mapcar #'convert (butlast body))
                   (list (convert (car (last body)) t))))))

(define-compilation jscl/cl::macrolet (definitions &rest body)
  (let ((*environment* (copy-lexenv *environment*)))
    (dolist (def definitions)
      (destructuring-bind (name lambda-list &body body) def
        (let ((binding (make-binding :name name :type 'macro
                                     :value (macrolet-value lambda-list body))))
          (push-to-lexenv binding  *environment* 'function))))
    (convert (cons 'progn body) *multiple-value-p*)))

(define-compilation jscl/cl::let (bindings &rest body)
  (multiple-value-bind (lexical-variables values special-bindings)
      (process-bindings bindings)
    (let ((compiled-values (mapcar #'convert values))
          (*environment* (extend-local-env lexical-variables)))
      `(jscl/js::call(function ,(mapcar #'translate-variable lexical-variables)
                               ,(let-bind-dynamic-vars special-bindings body))
                     ,@compiled-values))))

(define-compilation jscl/cl::lambda (lambda-list &rest body)
  (compile-lambda lambda-list body))

(define-compilation jscl/cl::let* (bindings &rest body)
  (let ((bindings (mapcar #'ensure-list bindings))
        (*environment* (copy-lexenv *environment*)))
    (let ((specials (remove-if-not #'special-variable-p (mapcar #'first bindings)))
          (body `(jscl/js::progn
                   ,@(mapcar #'let*-initialize-value bindings)
                   ,(convert-block body t t))))
      `(jscl/js::selfcall ,(let*-binding-wrapper specials body)))))

(define-compilation jscl/cl::block (name &rest body)
  "  We  use  Javascript  exceptions  to  implement  non  local  control
 transfer.  Exceptions  has  dynamic  scoping, so  we  use  a  uniquely
 generated object to identify the block.  The instance of a empty array
 is used  to distinguish between nested  dynamic Javascript exceptions.
 See         https://github.com/jscl-project/jscl/issues/64         for
 futher details."
  (let* ((idvar (gvarname name))
         (b (make-binding :name name :type 'block :value idvar)))
    (when *multiple-value-p*
      (push 'multiple-value (binding-declarations b)))
    (let* ((*environment* (extend-lexenv (list b) *environment* 'block))
           (cbody (convert-block body t)))
      (if (member 'used (binding-declarations b))
          (block/build-nlx-catcher idvar cbody)
          `(jscl/js::selfcall ,cbody)))))



(define-compilation jscl/cl::return-from (name &optional value)
  (let* ((binding (or (lookup-in-lexenv name *environment* 'block)
                      (error "Return from unknown block `~S'." name)))
         (multiple-value-p (member 'multiple-value
                                   (binding-declarations binding))))
    (push 'used (binding-declarations binding))
    ;; The binding value  is the name of a variable,  whose value is the
    ;; unique identifier  of the  block as exception.  We can't  use the
    ;; variable  name itself,  because it  might  not be  unique, so  we
    ;; capture it in a closure.
    `(jscl/js::selfcall
      ,(when multiple-value-p `(jscl/js::var |values| (internal |mv|)))
      (jscl/js::throw (jscl/js::new (jscl/js::call-internal |BlockNLX|
                                                            ,(binding-value binding)
                                                            ,(convert value multiple-value-p)
                                                            ,(symbol-name name)))))))



(define-compilation jscl/cl::catch (id &rest body)
  (let ((values (if *multiple-value-p* '|values| '(internal |pv|))))
    `(jscl/js::selfcall
      (jscl/js::var id ,(convert id))
      (jscl/js::try
       ,(convert-block body t))
      (jscl/js::catch (cf)
        (jscl/js::if (jscl/js::and (jscl/js::instanceof cf (jscl/js::internal |CatchNLX|))
                                   (jscl/js::== (jscl/js::get cf "id") id))
                     (jscl/js::return (jscl/js::method-call
                                       ,values "apply" this
                                       (jscl/js::call-internal
                                        |forcemv|
                                        (jscl/js::get cf "values"))))
                     (jscl/js::throw cf))))))

(define-compilation jscl/cl::throw (id value)
  `(jscl/js::selfcall
    (jscl/js::var |values| (internal |mv|))
    (jscl/js::throw
        (jscl/js::new
         (jscl/js::call-internal |CatchNLX|
                                 ,(convert id)
                                 ,(convert value t))))))

(define-compilation jscl/cl::tagbody (&rest body)
  ;; Ignore the  tagbody if it does  not contain any go-tag.  We do this
  ;; because  1)  it is  easy  and  2)  many  built-in forms  expand  to
  ;; a implicit tagbody, so we save some space.
  (unless (some #'go-tag-p body)
    (return-from jscl/cl::tagbody (convert `(progn ,@body nil))))
  ;; The translation assumes the first form in BODY is a label
  (unless (go-tag-p (car body))
    (push (gensym "START") body))
  ;; Tagbody compilation
  (let ((branch (gvarname 'branch))
        (tbidx (gvarname 'tbidx)))
    (let ((*environment* (declare-tagbody-tags tbidx body))
          initag)
      (let ((b (lookup-in-lexenv (first body) *environment* 'gotag)))
        (setq initag (second (binding-value b))))
      `(jscl/js::selfcall
        ;; TAGBODY branch to take
        (jscl/js::var ,branch ,initag)
        (jscl/js::var ,tbidx #())
        (jscl/js::label tbloop
                        (jscl/js::while jscl/js::true
                          (jscl/js::try
                           (jscl/js::switch ,branch
                             ,@(with-collect
                                 (collect `(case ,initag))
                                 (dolist (form (cdr body))
                                   (if (go-tag-p form)
                                       (let ((b (lookup-in-lexenv form *environment* 'gotag)))
                                         (collect `(jscl/js::case ,(second (binding-value b)))))
                                       (collect (convert form)))))
                             default
                             (jscl/js::break tbloop)))
                          (jscl/js::catch (jump)
                            (jscl/js::if (jscl/js::and (jscl/js::instanceof jump
                                                                            (internal |TagNLX|))
                                                       (jscl/js::== (jscl/js::get jump "id") ,tbidx))
                                         (jscl/js::= ,branch (jscl/js::get jump "label"))
                                         (jscl/js::throw jump)))))
        (jscl/js::return ,(convert nil))))))

(define-compilation jscl/cl::go (label)
  (let ((b (lookup-in-lexenv label *environment* 'gotag)))
    (when (null b)
      (error "Unknown tag `~S'" label))
    `(jscl/js::selfcall
      (jscl/js::throw (jscl/js::new (jscl/js::call-internal |TagNLX|
                                                            ,(first (binding-value b))
                                                            ,(second (binding-value b))))))))

(define-compilation jscl/cl::unwind-protect (form &rest clean-up)
  `(jscl/js::selfcall
    (jscl/js::var ret ,(convert nil))
    (jscl/js::try
     (jscl/js::= ret ,(convert form)))
    (jscl/js::finally
     ,(convert-block clean-up))
    (jscl/js::return ret)))

(define-compilation jscl/cl::multiple-value-call (func-form &rest forms)
  `(jscl/js::selfcall
    (jscl/js::var func ,(convert func-form))
    (jscl/js::var args ,(vector (if *multiple-value-p* '|values| '(internal |pv|))))
    (jscl/js::return
      (jscl/js::selfcall
       (jscl/js::var |values| (internal |mv|))
       (jscl/js::var vs)
       (jscl/js::progn
         ,@(with-collect
             (dolist (form forms)
               (collect `(jscl/js::= vs ,(convert form t)))
               (collect `(if (jscl/js::and (jscl/js::=== (jscl/js::typeof vs) "object")
                                           (in "multiple-value" vs))
                             (jscl/js::= args (jscl/js::method-call args "concat" vs))
                             (jscl/js::method-call args "push" vs))))))
       (jscl/js::return (jscl/js::method-call func "apply" null args))))))

(define-compilation jscl/cl::multiple-value-prog1 (first-form &rest forms)
  `(jscl/js::selfcall
    (jscl/js::var args ,(convert first-form *multiple-value-p*))
    (jscl/js::progn ,@(mapcar #'convert forms))
    (jscl/js::return args)))

(define-compilation jscl/cl::the (value-type form)
  (warn "discarding THE ~a" value-type) ; XXX perhaps one day
  (convert form *multiple-value-p*))

(define-compilation jscl/cl::symbol-name (x)
  (convert `(jscl/ffi:oget ,x "name")))

(define-compilation %js-vref (var &optional raw)
  (if raw
      (make-symbol var)
      `(jscl/js::call-internal |js_to_lisp| ,(make-symbol var))))

(define-compilation %js-vset (var val)
  `(jscl/js::= ,(make-symbol var) (jscl/js::call-internal |lisp_to_js| ,(convert val))))

(define-compilation %js-typeof (x)
  `(jscl/js::call-internal |js_to_lisp| (jscl/js::typeof ,x)))

;;; Access a function defined in the internals runtime object.
(define-compilation %js-internal (name)
  `(internal ,name))


;; Catch any Javascript exception. Note  that because all non-local exit
;; are based  on try-catch-finally,  it will also  catch them.  We could
;; provide  a JS  function  to  detect it,  so  the  user could  rethrow
;; the error.
;;
;; (%js-try (progn …) (catch (err) …) (finally …))
(define-compilation %js-try (form &optional catch-form finally-form)
  (let ((catch-compilation
         (and catch-form
              (destructuring-bind (catch (var) &body body) catch-form
                (unless (eq catch 'catch)
                  (error "Bad CATCH clausule `~S'." catch-form))
                (let* ((*environment* (extend-local-env (list var)))
                       (tvar (translate-variable var)))
                  `(jscl/js::catch (,tvar)
                     (jscl/js::= ,tvar (jscl/js::call-internal |js_to_lisp| ,tvar))
                     ,(convert-block body t))))))
        
        (finally-compilation
         (and finally-form
              (destructuring-bind (finally &body body) finally-form
                (unless (eq finally 'finally)
                  (error "Bad FINALLY clausule `~S'." finally-form))
                `(finally
                  ,(convert-block body))))))
    
    `(selfcall
      (try (return ,(convert form)))
      ,catch-compilation
      ,finally-compilation)))

(define-compilation jscl/cl::symbol-macrolet (macrobindings &rest body)
  (let ((new (copy-lexenv *environment*)))
    (dolist (macrobinding macrobindings)
      (destructuring-bind (symbol expansion) macrobinding
        (let ((b (make-binding :name symbol :type 'macro :value expansion)))
          (push-to-lexenv b new 'variable))))
    (let ((*environment* new))
      (convert-block body nil t))))

(define-compilation jscl/cl::defmacro (name args &rest body)
  (format *trace-output* "~& Defining a macro-function ~s ~s"
          name args) ; ☠ debugging bootstrap
  (let* ((body (parse-body body :declarations t :docstring t))
         (ll (parse-destructuring-lambda-list args))
         (whole (or (lambda-list-wholevar ll)
                    (gensym "WHOLE-")))
         (environment (or (lambda-list-environment ll)
                          (gensym "ENVIRONMENT-")))
         (expander (make-macro-expander-body 
                    name args whole environment body)))
    
    ;; If we are  boostrapping JSCL, we need  to quote the
    ;; macroexpander, because the  macroexpander will need
    ;; to be dumped in the final environment somehow.
    (when (find :jscl-xc *features*)
      (setq expander (list 'quote expander)))
    (%compile-defmacro name expander)
    `(jscl/cl::eval-when (:compile-toplevel :execute)
       (%compile-defmacro ',name ,expander))))

(defmacro define-transformation (name args form)
  `(define-compilation ,name ,args
     (convert ,form)))

;; from backquote.lisp
(declaim (ftype (function (t) t) bq-completely-process))

(define-transformation backquote (form)
  (bq-completely-process form))


