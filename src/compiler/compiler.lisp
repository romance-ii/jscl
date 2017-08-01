;;;; compiler.lisp — the heart of the JavaScript compiler.

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

;;;; Compiler

(in-package #-jscl :jscl #+jscl :jscl/impl)

;; Translates the Lisp code to Javascript. This will compile the special
;; forms. Some  primitive functions are  compiled as special  forms too.
;; The respective  real functions are defined  in the target as  well as
;; some primitive functions.

(defun convert-to-bool (expr)
  `(if ,expr ,(convert t) ,(convert nil)))

(defvar *multiple-value-p* nil
  "A Form  can return  a multiple values  object calling  `values', like
 values(arg1,  arg2, ...).  It  will work  in any  context,  as well  as
 returning  an  individual  object.  However, if  the  special  variable
 `*multiple-value-p*' is  NIL, it  is guaranteed  that only  the primary
 value  will  be   used,  so  we  can  optimize  to   avoid  the  VALUES
 function call.
")

(defvar *convert-level* -1
  "Bound  dynamically  to  the  number of  nested  calls  to  `convert'.
 Therefore, a form is being compiled as toplevel if it is zero.")


;;; Environment

(defstruct binding
    name
  type
  value
  declarations)

(defstruct lexenv
    variable
  function
  setf-function
  block
  gotag
  type
  class)

(defvar *global-environment* (make-lexenv))
(defvar *environment* *global-environment*)

(defun lookup-in-lexenv (name lexenv namespace)
  (or (and (eql namespace 'function)
           (listp name)
           (= 2 (length name))
           (eql 'setf (car name))
           (or (find name (lexenv-setf-function lexenv)
                     :key #'binding-name :test #'eql)
               (unless (eq lexenv *global-environment*)
                 (find name (lexenv-setf-function *global-environment*)
                       :key #'binding-name :test #'eql))))
  (find name (ecase namespace
               (variable (lexenv-variable lexenv))
               (function (lexenv-function lexenv))
               (block    (lexenv-block    lexenv))
                   (gotag	(lexenv-gotag	lexenv))
                   (type	(lexenv-type	lexenv))
                   (class	(lexenv-class	lexenv)))
            :key #'binding-name
            :test #'eql)
      (unless (eq lexenv *global-environment*)
        (lookup-in-lexenv name *global-environment* namespace))))

(defun push-to-lexenv (binding lexenv namespace)
  (ecase namespace
    (variable (push binding (lexenv-variable lexenv)))
    (function (push binding (lexenv-function lexenv)))
    (block    (push binding (lexenv-block    lexenv)))
    (gotag	(push binding (lexenv-gotag	lexenv)))
    (type  	(push binding (lexenv-type  	lexenv)))
    (class	(push binding (lexenv-class	lexenv)))))

(defun extend-lexenv (bindings lexenv namespace)
  (let ((env (copy-lexenv lexenv)))
    (dolist (binding (reverse bindings) env)
      (push-to-lexenv binding env namespace))))

(defvar *global-environment* (make-lexenv))
(defvar *environment* *global-environment*)
(defvar *variable-counter*)

(defun gvarname (symbol)
  (incf *variable-counter*)
  (safe-js-var-name (limit-string-length symbol 32)
                    (integer-to-string *variable-counter*)))

(defun translate-variable (symbol)
  (awhen (lookup-in-lexenv symbol *environment* 'variable)
    (binding-value it)))

(defun extend-local-env (args)
  (let ((new (copy-lexenv *environment*)))
    (dolist (symbol args new)
      (let ((b (make-binding :name symbol :type 'variable
                             :value (gvarname symbol))))
        (push-to-lexenv b new 'variable)))))


;;; Toplevel compilations
(defvar *toplevel-compilations*)

(defun toplevel-compilation (string)
  (push string *toplevel-compilations*))

(defun get-toplevel-compilations ()
  (reverse *toplevel-compilations*))

(defun %compile-defmacro (name lambda)
  (let ((binding (make-binding :name name :type 'macro :value lambda)))
    (warn "Binding global macro ~s" name)
    (push-to-lexenv binding *global-environment* 'function))
  name)

(defun global-binding (name type namespace)
  (or (lookup-in-lexenv name *global-environment* namespace)
      (let ((b (make-binding :name name :type type :value nil)))
        (push-to-lexenv b *global-environment* namespace)
        b)))

(defun claimp (symbol namespace claim)
  (let ((b (lookup-in-lexenv symbol *environment* namespace)))
    (and b (member claim (binding-declarations b)))))

(defun !proclamation (decl type)
     (dolist (name (cdr decl))
    (let ((b (global-binding name type type)))
      (push (car decl) (binding-declarations b)))))

(defun jscl/cl::proclaim (decl)
  (case (car decl)
    (jscl::pure (!proclamation decl 'function))
    (special (!proclamation decl 'variable))
    (notinline (!proclamation decl 'function))
    (constant (!proclamation decl 'variable))))

(defun jscl/cltl2::declaration-information (name
                                            &optional
                                              (env *global-environment*))
  "Return information about declarations named by DECLARATION-NAME.

Defined in CLtL2.

If DECLARATION-NAME is  OPTIMIZE return a list whose entries  are of the
form (QUALITY VALUE).

If DECLARATION-NAME  is DECLARATION return  a list of  declaration names
that have been proclaimed as valid.

If DECLARATION-NAME  is a name  that has defined  via DEFINE-DECLARATION
return a user defined value.

In SBCL,  if DECLARATION-NAME is SB-EXT:MUFFLE-CONDITIONS  return a type
specifier for the condition types that have been muffled.
"
  (declare (ignore env))
  (case name
    (optimize '((speed 1) (debug 1) (space 1)
                (safety 2) (compilation-speed 1)))
    (otherwise nil)))

(defun jscl/cl::define-symbol-macro (name expansion)
  (let ((b (make-binding :name name :type 'macro :value expansion)))
    (push-to-lexenv b *environment* 'variable)
    name))


;;; Report functions which are called but not defined

(defvar *fn-info* '())

(defstruct fn-info
  name
  defined
  called)

(defun function-name-p (name)
  (or (symbolp name)
      (and (listp name)
           (= 2 (length name))
           (find (car name) '(setf jscl/ffi:oget)))))

(defun find-fn-info (name)
  (assert (function-name-p name))
  (let ((entry (find name *fn-info*
                     :key #'fn-info-name
                     :test 'equalp)))
    (unless entry
      (setq entry (make-fn-info :name name))
      (push entry *fn-info*))
    entry))

(defun fn-info (name &key defined called)
  (let ((info (find-fn-info name)))
    (when defined
      (setf (fn-info-defined info) defined))
    (when called
      (setf (fn-info-called info) called))))

(defun report-undefined-functions ()
  (dolist (info *fn-info*)
    (let ((name (fn-info-name info)))
      (when (and (fn-info-called info)
                 (not (fn-info-defined info)))
        (warn "The function `~s' is undefined." name))))
  (setq *fn-info* nil))


;;; Special forms

(defvar *special-operators* (make-hash-table :test 'eql)
  "Special forms that have direct compilations rather than typical macros")

(defun lambda-list-keyword-p (symbol)
  (member symbol lambda-list-keywords))

(defmacro define-compilation (name args &body body)
  "Creates a new primitive named NAME with parameters ARGS and
 BODY. The body can access to the local environment through the
 variable *ENVIRONMENT*."
  `(let ((fn (lambda ,args (block ,name ,@body))))
     ,(when (member (symbol-package name) (list (find-package "COMMON-LISP")
                                                (find-package "JSCL/COMMON-LISP")))
        (format *trace-output* 
                "~& Defined special form ~a" name)
        `(let ((binding (make-binding :name ',name :type 'macro 
                                      :value '(lambda (&rest args)
                                               (compile-special-form ',name args)))))
           (push-to-lexenv binding *global-environment* 'function)))
     (setf (gethash ',name *special-operators*) fn)))

(defvar *ll-keywords* '(&optional &rest &key))

(defun list-until-keyword (list)
  (if (or (null list) (member (car list) *ll-keywords*))
      nil
      (cons (car list) (list-until-keyword (cdr list)))))

(defun ll-section (keyword ll)
  (list-until-keyword (cdr (member keyword ll))))

(defun ll-required-arguments (ll)
  (list-until-keyword ll))

(defun ll-optional-arguments-canonical (ll)
  (mapcar #'ensure-list (ll-section '&optional ll)))

(defun ll-optional-arguments (ll)
  (mapcar #'car (ll-optional-arguments-canonical ll)))

(defun ll-rest-argument (ll)
  (let ((rest (ll-section '&rest ll)))
    (when (cdr rest)
      (error "Bad lambda-list `~S'." ll))
    (car rest)))

(defun ll-keyword-arguments-canonical (ll)
  (flet ((canonicalize (keyarg)
           ;; Build a canonical keyword argument descriptor, filling the
           ;; optional  fields.  The  result  is  a  list  of  the  form
           ;; ((keyword-name var) init-form svar).
           (let ((arg (ensure-list keyarg)))
             (cons (if (listp (car arg))
                       (car arg)
                       (list (intern (symbol-name (car arg))
                                     "KEYWORD")
                             (car arg)))
                   (cdr arg)))))
    (mapcar #'canonicalize (ll-section '&key ll))))

(defun ll-keyword-arguments (ll)
  (mapcar (lambda (keyarg) (second (first keyarg)))
          (ll-keyword-arguments-canonical ll)))

(defun ll-svars (lambda-list)
  (let ((args
         (append
          (ll-keyword-arguments-canonical lambda-list)
          (ll-optional-arguments-canonical lambda-list))))
    (remove nil (mapcar #'third args))))

(defun js-identifier-char-p (char)
  (or (char= #\_ char)
      (char= #\$ char)
      (alphanumericp char)))

(defun js-name-part (name)
  (substitute-if #\$ (complement #'js-identifier-char-p)
                 (substitute #\_ #\- (string name))))

(defun safe-js-name (&rest name-parts)
  (intern (join (mapcar #'js-name-part name-parts) "_")))

(defun safe-js-fun-name (&rest name-parts)
  (apply #'safe-js-name "fun" name-parts))

(defun safe-js-var-name (&rest name-parts)
  (apply #'safe-js-name "var" name-parts))

(defun safe-js-lit-name (&rest name-parts)
  (apply #'safe-js-name "lit" name-parts))

(defun lambda-name/docstring-wrapper (name docstring code)
  (let ((func (safe-js-fun-name name)))
    (if (or name docstring)
        `(jscl/js::selfcall
          (jscl/js::var ,func ,code)
          ,(when name `(jscl/js::= (jscl/js::get ,func "fname") ,name))
          ,(when docstring `(jscl/js::= (jscl/js::get ,func "docstring") ,docstring))
          (jscl/js::return ,func))
        code)))

(defun lambda-check-argument-count
    (n-required-arguments n-optional-arguments rest-p)
  ;; Note:  Remember that  we assume  that  the number  of arguments  of
  ;; a call is at least 1 (the values argument).
  (let ((min n-required-arguments)
        (max (if rest-p 'n/a (+ n-required-arguments n-optional-arguments))))
    (block nil
      ;; Special case: a positive exact number of arguments.
      (when (and (< 0 min) (eql min max))
        (return `(jscl/js::call-internal |checkArgs| (nargs) ,min)))
      ;; General case:
      `(jscl/js::progn
         ,(when (< 0 min)     `(jscl/js::call-internal |checkArgsAtLeast| (nargs) ,min))
         ,(when (numberp max) `(jscl/js::call-internal |checkArgsAtMost|  (nargs) ,max))))))

(defun compile-lambda-optional (ll)
  (let* ((optional-arguments (ll-optional-arguments-canonical ll))
         (n-required-arguments (length (ll-required-arguments ll)))
         (n-optional-arguments (length optional-arguments))
         (svars (remove nil (mapcar #'third optional-arguments))))
    (when optional-arguments
      `(jscl/js::progn
         ,(when svars
            `(jscl/js::var ,@(mapcar (lambda (svar)
                              (list (translate-variable svar)
                                    (convert t)))
                            svars)))
         (jscl/js::switch (nargs)
           ,@(with-collect
                 (dotimes (idx n-optional-arguments)
                   (let ((arg (nth idx optional-arguments)))
                     (collect `(case ,(+ idx n-required-arguments)))
                   (collect `(jscl/js::= ,(translate-variable (car arg))
                                  ,(convert (cadr arg))))
                     (collect (when (third arg)
                              `(jscl/js::= ,(translate-variable (third arg))
                                    ,(convert nil))))))
               (collect 'default)
               (collect '(break))))))))

(defun compile-lambda-rest (ll)
  (let ((n-required-arguments (length (ll-required-arguments ll)))
        (n-optional-arguments (length (ll-optional-arguments ll)))
        (rest-argument (ll-rest-argument ll)))
    (when rest-argument
      (let ((js!rest (translate-variable rest-argument)))
        `(jscl/js::progn
           (jscl/js::var ,js!rest ,(convert nil))
           (jscl/js::var i)
           (jscl/js::for ((jscl/js::= i (- (nargs) 1))
                          (jscl/js::>= i ,(+ n-required-arguments n-optional-arguments))
                          (jscl/js::post-- i))
                         (jscl/js::= ,js!rest (new (jscl/js::call-internal |Cons| (arg i) ,js!rest)))))))))

(defun compile-lambda-parse-keywords (ll)
  (let ((n-required-arguments
         (length (ll-required-arguments ll)))
        (n-optional-arguments
         (length (ll-optional-arguments ll)))
        (keyword-arguments
         (ll-keyword-arguments-canonical ll)))
    `(jscl/js::progn
       ;; Declare variables
       ,@(with-collect
             (dolist (keyword-argument keyword-arguments)
               (destructuring-bind ((keyword-name var) &optional initform svar)
                   keyword-argument
                 (declare (ignore keyword-name initform))
               (collect `(jscl/js::var ,(translate-variable var)))
                 (when svar
                   (collect
                     `(jscl/js::var ,(translate-variable svar)
                                    ,(convert nil)))))))

       ;; Parse keywords
       ,(flet ((parse-keyword (keyarg)
                 (destructuring-bind ((keyword-name var) &optional initform svar) keyarg
                   ;; ((keyword-name var) init-form svar)
                   `(jscl/js::progn
                      (jscl/js::for ((jscl/js::= i ,(+ n-required-arguments n-optional-arguments))
                                     (jscl/js::< i (nargs))
                                     (jscl/js::+= i 2))
                           ;; ....
                                    (jscl/js::if (jscl/js::=== (arg i) ,(convert keyword-name))
                                                 (jscl/js::progn
                                                   (jscl/js::= ,(translate-variable var) (arg (+ i 1)))
                                                   ,(when svar `(jscl/js::= ,(translate-variable svar)
                                                 ,(convert t)))
                                                   (jscl/js::break))))
                      (jscl/js::if (jscl/js::== i (nargs))
                                   (jscl/js::= ,(translate-variable var) ,(convert initform)))))))
          (when keyword-arguments
            `(jscl/js::progn
               (jscl/js::var i)
               ,@(mapcar #'parse-keyword keyword-arguments))))

       ;; Check for unknown keywords
       ,(when keyword-arguments
          `(jscl/js::progn
             (jscl/js::var start ,(+ n-required-arguments n-optional-arguments))
             (jscl/js::if (jscl/js::== (jscl/js::% (jscl/js::- (nargs) start) 2) 1)
                          (jscl/js::throw "Odd number of keyword arguments."))
             (jscl/js::for ((jscl/js::= i start) (jscl/js::< i (nargs)) (jscl/js::+= i 2))
                           (jscl/js::if (jscl/js::and
                                         ,@(mapcar (lambda (keyword-argument)
                                       (destructuring-bind ((keyword-name var) &optional initform svar)
                                           keyword-argument
                                         (declare (ignore var initform svar))
                                                       `(jscl/js::!== (arg i) ,(convert keyword-name))))
                                     keyword-arguments))
                                        (jscl/js::throw (jscl/js::+ "Unknown keyword argument "
                                                                    (jscl/js::property (arg i) "name"))))))))))

(defun parse-lambda-list (ll)
  (values (ll-required-arguments ll)
          (ll-optional-arguments ll)
          (ll-keyword-arguments  ll)
          (ll-rest-argument      ll)))

(defun parse-body (body &key declarations docstring)
  "Process BODY for declarations and/or docstrings. Return as
 multiple values the BODY without docstrings or declarations, the
 list of declaration forms and the docstring."
  (let ((value-declarations)
        (value-docstring))
    ;; Parse declarations
    (when declarations
      (do* ((rest body (cdr rest))
            (form (car rest) (car rest)))
           ((or (atom form) (not (eql (car form) 'declare)))
            (setf body rest))
        (push form value-declarations)))
    ;; Parse docstring
    (when (and docstring
               (stringp (car body))
               (not (null (cdr body))))
      (setq value-docstring (car body))
      (setq body (cdr body)))
    (values body value-declarations value-docstring)))

(defun bind-this ()
  (let* ((gvar (gvarname 'this))
         (binding (make-binding :name 'this :type 'variable :value gvar)))
    (push-to-lexenv binding *environment* 'variable)
    `(jscl/js::var ,gvar |this|)))

(defun compile-lambda (ll body &key name block)
  "Compile a lambda function with lambda list LL and body BODY. If NAME
is given, it should be a constant string and it will become the name
of  the function.  If BLOCK  is non-NIL,  a named  block is  created
around the body. NOTE: No block (even anonymous) is created if BLOCK
is NIL."
  (multiple-value-bind (required-arguments
                        optional-arguments
                        keyword-arguments
                        rest-argument)
      (parse-lambda-list ll)
    (multiple-value-bind (body decls documentation)
        (parse-body body :declarations t :docstring t)
      (declare (ignore decls))
      (let ((n-required-arguments (length required-arguments))
            (n-optional-arguments (length optional-arguments))
            (*environment* (extend-local-env
                            (append (ensure-list rest-argument)
                                    required-arguments
                                    optional-arguments
                                    keyword-arguments
                                    (ll-svars ll)))))

        (lambda-name/docstring-wrapper
         name documentation
                                       `(named-function ,(safe-js-fun-name name)
                          (|values|
                           ,@(mapcar (lambda (x)
                                                                              (translate-variable x))
                                     (append required-arguments
                                             optional-arguments)))
                                                        ;; Check number of arguments
                          ,(lambda-check-argument-count
                            n-required-arguments
                                                                                      n-optional-arguments
                                                                                      (or rest-argument keyword-arguments))
                                                        ,(compile-lambda-optional ll)
                                                        ,(compile-lambda-rest ll)
                                                        ,(compile-lambda-parse-keywords ll)
                                                        ,(bind-this)
                                                        ,(let ((*multiple-value-p* t))
                                                           (if block
                                                               (convert-block `((block ,block ,@body)) t)
                                                               (convert-block body t)))))))))

(defun setq-pair (var val)
  (unless (symbolp var)
    (error "~a is not a symbol" var))
  (let ((b (lookup-in-lexenv var *environment* 'variable)))
    (cond
      ((and b
            (eq (binding-type b) 'variable)
            (not (member 'special (binding-declarations b)))
            (not (member 'constant (binding-declarations b))))
       `(jscl/js::= ,(binding-value b) ,(convert val)))
      ((and b (eq (binding-type b) 'macro))
       (convert `(setf ,var ,val)))
      (t
       (convert `(set ',var ,val))))))

;;; Compilation of literals and object dumping

;; BOOTSTRAP MAGIC: We record the  macro definitions as lists during the
;; bootstrap. Once  everything is  compiled, we want  to dump  the whole
;; global  environment  to  the  output  file to  reproduce  it  in  the
;; run-time. However,  the environment  must contain  expander functions
;; rather  than lists.  We  do not  know how  to  dump function  objects
;; itself, so  we mark  the list  definitions with  this object  and the
;; compiler  will  be  called  when   this  object  has  to  be  dumped.
;; Backquote/unquote does a similar magic, but this use is exclusive.
;;
;; Indeed,  perhaps  to compile  the  object  other  macros need  to  be
;; evaluated.  For this  reason  we define  a  valid macro-function  for
;; this symbol.
(defvar *magic-unquote-marker* (gensym "MAGIC-UNQUOTE-"))

#-jscl-xc
(setf (macro-function *magic-unquote-marker*)
      (lambda (form &optional environment)
        (declare (ignore environment))
        (second form)))

(defvar *literal-table*)
(defvar *literal-counter*)

(defun limit-string-length (string length)
  (and string
       (let ((string (princ-to-string string)))
         (if (> (length string) length)
             (subseq string 0 length)
             string))))

(defun genlit (&optional name)
  (incf *literal-counter*)
  (safe-js-lit-name (or (limit-string-length name 32) "")
                    (integer-to-string *literal-counter*)))

(defun dump-symbol (symbol)
  (let ((package (symbol-package symbol)))
    (cond
      ;; Uninterned symbol
      ((null package)
       `(new (jscl/js::call-internal |Symbol| ,(symbol-name symbol))))
      ;; Special case for bootstrap. For now,  we just load all the code
      ;; with  JSCL as  the current  package. We  will compile  the JSCL
      ;; package as CL in the target. ☠ FIXME
      #+ not-needed-any-more #- jscl
      ((eq package (find-package "JSCL"))
       `(jscl/js::call-internal |intern| ,(symbol-name symbol)))
      ;; Interned symbol
      (t
       `(jscl/js::call-internal |intern| ,(symbol-name symbol)
                                ,(package-name package))))))

(defun dump-cons (cons)
  (if (eql (car cons)
           'sb-int::quasiquote)
      (progn
        (warn "Quasi-quote leakage: ~s" cons)
        (dump-cons (sb-impl::expand-quasiquote cons nil)))
  (let ((head (butlast cons))
        (tail (last cons)))
        `(jscl/js::call-internal |QIList|
                    ,@(mapcar (lambda (x) (literal x t)) head)
                    ,(literal (car tail) t)
                                 ,(literal (cdr tail) t)))))

(defun dump-array (array)
  (let ((elements (vector-to-list array)))
    (list-to-vector (mapcar #'literal elements))))

(defun dump-string (string)
  `(jscl/js::call-internal |make_lisp_string| ,string))

                                        ; from ALEXANDRIA
(declaim (inline safe-endp))
(defun safe-endp (x)
  (declare (optimize (safety 3) (speed 0) (debug 3)))
  (endp x))

(defun alist-plist (alist)
  "Returns a property list containing the same keys and values as the
association list ALIST in the same order."
  (let (plist)
    (dolist (pair alist)
      (push (car pair) plist)
      (push (cdr pair) plist))
    (nreverse plist)))

                                        ; end ALEXANDRIA

#+struct-ctor-FIXME-remove
(defun constructor<-structure (object) ;; FIXME ☠☠☠
  (let* ((class (class-of object))
         (class-name (class-name class))
         (constructor (intern (concatenate 'string
                                           (string :make-)
                                           (symbol-name class-name))
                              (symbol-package class-name)))
         (slot-names
          #+sbcl
           (mapcar #'sb-mop:slot-definition-name
                   (sb-mop:class-slots (find-class class)))
           #+jscl (mapcar #'jscl/mop:slot-definition-name
                          (jscl/mop:class-slots (fnd-class class))))
         (slot-values
          (mapcar (lambda (slot-name)
                    (literal (slot-value object slot-name)))
                  slot-names)))
    (convert-1 (cons constructor
                     (alist-plist
                      (mapcar #'cons slot-names slot-values))))))

(defun dump-complex-literal (sexp &optional recursivep)
     (or (cdr (assoc sexp *literal-table* :test #'eql))
         (let ((dumped (typecase sexp
                         (symbol (dump-symbol sexp))
                         (string (dump-string sexp))
                         (cons
                       ;; BOOTSTRAP MAGIC:  See the root  file jscl.lisp
                       ;; and the function `dump-global-environment' for
                       ;; further information.
                          (if (eq (car sexp) *magic-unquote-marker*)
                              (convert (second sexp))
                              (dump-cons sexp)))
                         (array (dump-array sexp)))))
        (if (and recursivep (not (symbolp sexp)))
               dumped
               (let ((jsvar (genlit (typecase sexp
                                      (cons "expr")
                                      (array "array")
                                      (t (string sexp))))))
                 (push (cons sexp jsvar) *literal-table*)
              (toplevel-compilation `(jscl/js::var ,jsvar ,dumped))
                 (when (keywordp sexp)
                (toplevel-compilation `(jscl/js::= (jscl/js::get ,jsvar "value") ,jsvar)))
              jsvar)))))

(defun literal-sv (sv)
  (let* ((kind (storage-vector-kind sv))
         (jsvar (genlit (string kind)))
         (vec (storage-vector-underlying-vector sv)))
    (push (cons sv jsvar) *literal-table*)
    `(jscl/js::var ,jsvar
                   (jscl/js::selfcall
                    (jscl/js::var r ,vec)
                    (jscl/js::= (jscl/js::get r "length") ,(length vec))
                    (jscl/js::= (jscl/js::get r "svKind") ,kind)
                    (jscl/js::return r)))))

(defun literal-symbol (sexp)
  (let ((jsvar (genlit)))
    (toplevel-compilation `(var (,jsvar ,(dump-symbol sexp))))
    jsvar))

(defun literal-bignum (bignum)
  (cerror (format nil
                  "Use the ~:[smallest~;biggest~] possible number instead"
                  (plusp bignum))
          "Cannot pass BigNum ~:d yet" bignum)
  (if (plusp bignum)
      (literal +most-positive-fixnum+)
      (literal +most-negative-fixnum+)))

(defun literal-rational (number)
  (cerror "Round it off as a Double-Float"
          "Cannot pass BigNum ~:d exactly" number)
  (literal (coerce number 'double-float)))

(defun literal (sexp &optional recursivep)
  (cond
    ((typep sexp 'sb-impl::comma)
     (error "Quasi-quoted expression leakage: ~s" sexp))
    ((and (integerp sexp)
          (not (jscl/cl::fixnump sexp)))
     (literal-bignum sexp))
    ((and (rationalp sexp)
          (not (= 1 (denominator sexp)))
          (not (rational-float-p sexp)))
     (literal-rational sexp))
    ((complexp sexp)
     (error "Cannot pass complex numbers like ~d" sexp))
    (t
     (typecase sexp
       (symbol (literal-symbol sexp))
       (pathname (namestring sexp))
       (fixnum sexp)
       (rational
        ;; already made sure it was OK, above
        (coerce sexp 'double-float))
       (number sexp)
       (structure-object (literal-sv sexp))
       (string sexp)
       (array (literal-sv sexp))
       (standard-object (literal-sv sexp))
       (function ;; FIXME?
        (list 'function (list 'quote (nth-value 2 (function-lambda-expression sexp)))))
       (character (string sexp))       ; is this really the right thing?
       (t (dump-complex-literal sexp recursivep))))))

(defun function-namestring (name)
  (cond
    ((symbolp name) (symbol-name name))
    ((and (listp name)
          (eq 'setf (car name)))
     (concatenate 'string
                  "@Set-Field@«"
                  (symbol-name (second name))
                  "»"))))

(defun function-block-name (name)
  (etypecase name
    (symbol name)
    (list (second name))))

(defun make-function-binding (fname)
  (make-binding :name fname :type 'function :value (gvarname fname)))

(defun compile-function-definition (list)
  (compile-lambda (car list) (cdr list)))

(defun translate-function (name)
  (let ((b (lookup-in-lexenv name *environment* 'function)))
    (and b (binding-value b))))

(defun compiled-function-code (def)
                           (compile-lambda (cadr def)
                                           `((block ,(car def)
                                               ,@(cddr def)))))

(defun environment+new-functions (fun-names)
  (extend-lexenv (mapcar #'make-function-binding fun-names)
                         *environment*
                 'function))

(defun labels/compiled-label-function (func)
  `(jscl/js::var ,(translate-function (car func))
                 ,(compiled-function-code func)))

(defvar *compiling-file* nil
  "Was the compiler invoked from `compile-file'?")

(defun macrolet-value (lambda-list body)
  (let ((g!form (gensym "FORM-")))
                                       `(lambda (,g!form)
                                          (destructuring-bind ,lambda-list ,g!form
         ,@body))))

(defun special-variable-p (x)
  (and (claimp x 'variable 'special) t))

(defun normalize-bindings (arg)
  (destructuring-bind (name &optional value)
      (ensure-list arg)
    (list name value)))

(defun process-bindings (bindings)
  "Given a LET-like description of bindings, return:

1. A list of lexical variable names

2. A list of values to bind to the lexical variables

3. A alist of (special-variable . lexical-variable) to bind."
  (let ((bindings (mapcar #'normalize-bindings bindings))
        (special-bindings nil))
    (values
     ;; Lexical Variables
     (mapcar (lambda (var)
               (if (special-variable-p var)
                   (let ((lexvar (gensym "LEXICAL-VARIABLE-")))
                     (push (cons var lexvar) special-bindings)
                     lexvar)
                   var))
             (mapcar #'car bindings))
     ;; Values
     (mapcar #'cadr bindings)
     ;; Binding special variables to lexical variables
     special-bindings)))

(defun convert-block-with-special-bindings (body special-bindings)
  (let ((special-variables (mapcar #'car special-bindings))
        (lexical-variables (mapcar #'cdr special-bindings)))
    `(jscl/js::return (jscl/js::call-internal
                       |bindSpecialBindings|
                       ,(map 'vector #'literal special-variables)
                       ,(map 'vector #'translate-variable lexical-variables)
                       (function () ,(convert-block body t t))))))

(defun let-bind-dynamic-vars (special-bindings body)
  "Wrap  CODE to  restore the  symbol  values of  the dynamic  bindings.
BINDINGS is a list of pairs of the form (SYMBOL . PLACE), where PLACE is
a Javascript variable  name to initialize the symbol value  and where to
stored the old value."
  (if (null special-bindings)
      (convert-block body t t)
      (convert-block-with-special-bindings body special-bindings)))

(defun add-let*-var-to-environment (var value)
  (let* ((v (gvarname var))
         (b (make-binding :name var :type 'variable :value v)))
    (prog1 `(jscl/js::var ,v ,(convert value))
      (push-to-lexenv b *environment* 'variable))))

(defun let*-initialize-value (binding)
  " Return  the code to  initialize BINDING,  and push it  extending the
current lexical environment if the variable is not special."
  (let ((var (first binding))
        (value (second binding)))
    (if (special-variable-p var)
        (convert `(setq ,var ,value))
        (add-let*-var-to-environment var value))))

(defun let*-wrapper-set-value (b)
  (let ((s (convert `(quote ,(car b)))))
    `(jscl/js::var ,(cdr b) (jscl/js::get ,s "value"))))

(defun let*-wrapper-reset-value (b)
  (let ((s (convert `(quote ,(car b)))))
    `(jscl/js::= (jscl/js::get ,s "value") ,(cdr b))))

(defun let*-binding-wrapper (symbols body)
  "  Wrap BODY  to  restore the  symbol values  of  SYMBOLS after  body.
It DOES NOT generate code to initialize the value of the symbols, unlike
let-binding-wrapper."
  (unless symbols
    (return-from let*-binding-wrapper body))
  (let ((store (mapcar (lambda (s) (cons s (gvarname s)))
                       (remove-if-not #'special-variable-p symbols))))
    `(jscl/js::progn
       (jscl/js::try
        ,@(mapcar #'let*-wrapper-set-value store)
        ,body)
       (jscl/js::finally
        ,@(mapcar #'let*-wrapper-reset-value store)))))
          
(defun block-return-multiple-values ()
  `(jscl/js::return (jscl/js::method-call |values| "apply" this
                                          (jscl/js::call-internal |forcemv| (jscl/js::get cf "values")))))

(defun block-return-single-value ()
  `(jscl/js::return `(jscl/js::return (jscl/js::get cf "values"))))

(defun block/build-nlx-catcher (idvar cbody)
  `(jscl/js::selfcall
    (jscl/js::try
     (jscl/js::var ,idvar #())
             ,cbody)
    (jscl/js::catch (cf)
      (jscl/js::if (jscl/js::and (jscl/js::instanceof cf (jscl/js::internal |BlockNLX|))
                                 (jscl/js::== (jscl/js::get cf "id") ,idvar))
                  ,(if *multiple-value-p*
                        (block-return-multiple-values)
                        (block-return-single-value))
                   (jscl/js::throw cf)))))

(defun go-tag-p (x)
  (or (integerp x) (symbolp x)))

(defun declare-tagbody-tags (tbidx body)
  (let* ((go-tag-counter 0)
         (bindings
          (mapcar (lambda (label)
                    (let ((tagidx (incf go-tag-counter)))
                      (make-binding :name label :type 'gotag :value (list tbidx tagidx))))
                  (remove-if-not #'go-tag-p body))))
    (extend-lexenv bindings *environment* 'gotag)))

(defun variable-arity/check-numeric-arg (v x function args)
  `(jscl/js::if (jscl/js::!= (jscl/js::typeof ,v) "number")
                (jscl/js::throw
                    (jscl/js::new
                     (jscl/js::call
                      |Error|
                      (+ "" (jscl/js::typeof ,v)
                         " is not a number: " ,v " "
                         ,(princ-to-string (convert x)) " in "
                         ,(princ-to-string function)
                         ,@(mapcar (lambda (s)
                                     (concatenate 'string " "
                                                  (princ-to-string s)))
                                   args)))))))

(defun variable-arity-call (args function)
  "VARIABLE-ARITY-CALL  compiles variable  arity  operations on  numeric
arguments.  ARGS stands  for a  variable which  holds a  list of  forms.
It will compile them and store  the result in some Javascript variables.
BODY is  evaluated with  ARGS bound  to the list  of these  variables to
generate the code which performs the transformation on these variables."
  (unless (consp args)
    (error "ARGS must be a non-empty list"))
  (let ((counter 0))
    ;; XXX: Add macro with-collectors
    (with-collector (fargs)
      (with-collector (prelude)
        (dolist (x args)
          (if (numberp x)
              (collect-fargs x)
              (let ((v (make-symbol (concat "arg" (integer-to-string (incf counter))))))
                (collect-prelude `(jscl/js::var ,v ,(convert x)))
                (collect-prelude (variable-arity/check-numeric-arg v x
                                                                   function args))
                (collect-fargs v))))
        `(jscl/js::selfcall
          (jscl/js::progn
            ,@prelude)
          ,(funcall function fargs))))))

(defmacro variable-arity (args &body body)
  (unless (symbolp args)
    (error "`~S' is not a symbol." args))
  `(variable-arity-call ,args (lambda (,args) `(jscl/js::return  ,,@body))))

(define-setf-expander %js-vref (var)
  (let ((new-value (gensym "JS-VREF-NEW-VALUE-")))
    (unless (stringp var)
      (error "`~S' is not a string." var))
    (values nil
            (list var)
            (list new-value)
            `(%js-vset ,var ,new-value)
            `(%js-vref ,var))))

#-jscl
(defparameter *macroexpander-cache*
  (make-hash-table :test #'eq))

(defun jscl/cl::macro-function (symbol &optional (environment *environment*))
  #- (or ecl sbcl jscl)
  (warn "Your Implementation's quasiquote may not be handled properly.")
  (cond
    ((not (symbolp symbol))
    (error "`~S' is not a symbol." symbol))
    #+ecl
    ((eql symbol 'si:quasiquote)
     (warn "ECL quasiquote is probably not handled properly yet")
     (lambda (form) (ext::macroexpand-1 form)))
    #+sbcl
    ((eql symbol 'sb-int:quasiquote)
     (lambda (form environment)
       (declare (ignore environment))
       (sb-impl::expand-quasiquote form nil)))
    (t 
     (let ((b (lookup-in-lexenv symbol (or environment *global-environment*)
                                'function)))
       (if (and b (eql (binding-type b) 'macro))
        (let ((expander (binding-value b)))
          (cond
            #-jscl
               ((gethash b *macroexpander-cache*))
            ((listp expander)
             (let ((compiled (eval expander)))
               ;; The list representation are useful while
               ;; bootstrapping, as we can dump the definition of the
               ;; macros easily, but they are slow because we have to
               ;; evaluate them and compile them now and again. So, let
               ;; us replace the list representation version of the
               ;; function with the compiled one.
               #+jscl (setf (binding-value b) compiled)
                  #-jscl (setf (gethash b *macroexpander-cache*) compiled)))
               (expander)))
           nil)))))

(defun macroexpand-1/symbol (symbol &optional (env *environment*))
  (let ((b (lookup-in-lexenv symbol (or env *global-environment*)
                             'variable)))
         (if (and b (eq (binding-type b) 'macro))
             (values (binding-value b) t)
        (values symbol nil))))

(defun macroexpand-1/cons (form &optional (environment *environment*))
  (let ((macrofun (jscl/cl::macro-function (car form) environment)))
    (cond ((special-operator-p (car form))
           (values form nil))
          (macrofun
           (values (funcall macrofun (cdr form) environment) t))
          ((macro-function (car form) nil)
           (break "MACROEXPAND-1 has no macro binding for ~a::~a, ~
but one exists in the global environment of the host compiler."
                  (package-name (symbol-package (car form)))
                  (car form)))
          (t (values form nil)))))

(defun jscl/cl::macroexpand-1 (form &optional (environment *environment*))
  "If FORM is a macro form or symbol in ENVIRONMENT, expand it.

Returns the expanded form  of FORM (if any), or FORM  itself (if FORM is
not  a  macro)   as  primary  value.  As  a   secondary  value,  returns
a generalized boolean indicating whether FORM were changed."
  (let ((*environment* (or environment *global-environment*)))
    (cond
      ((symbolp form)
       (macroexpand-1/symbol form environment))
      ((not (consp form)) (values form nil))
      ((and (consp (car form))
            (eql 'jscl/cl:setf (caar form)))
       (error "Should expand SETF form ~a" form))
      ((symbolp (car form))
       (macroexpand-1/cons form environment))
      (t (values form nil)))))

(defun jscl/cl::macroexpand (form &optional environment)
  "Fully expand all macro forms in FORM (in context of any given ENVIRONMENT)

See also: `MACROEXPAND-1'"
  (let ((continue t))
    (while continue
      (multiple-value-setq (form continue) 
        (jscl/cl::macroexpand-1 form environment))))
  form)

(defun compile-funcall/function (function arglist)
  (when (and (symbolp function)
             (or (jscl/cl::macro-function function)
                 (jscl/cl::special-operator-p function)))
    (error "Compiler error: Macro function was not expanded: ~s"
           function))
  (fn-info function :called t)
  ;; This code will  work even if the symbol-function is  unbound, as it
  ;; is represented by a function that throws the expected error.
  `(jscl/js::method-call ,(convert `',function) "fvalue" ,@arglist))

(defun compile-funcall/translate-function (function arglist)
  `(jscl/js::call,(translate-function function) ,@arglist))

(defun compile-funcall/lambda (function arglist)
  `(jscl/js::call,(convert `(function ,function)) ,@arglist))

(defun compile-funcall/oget (function args)
  `(jscl/js::call-internal
    |js_to_lisp|
    (jscl/js::call,(reduce (lambda (obj p)
                             `(jscl/js::property ,obj (jscl/js::call-internal |xstring| ,p)))
                           (mapcar #'convert (cdr function)))
                  ,@(mapcar (lambda (s)
                              `(jscl/js::call-internal |lisp_to_js| ,(convert s)))
                            args))))

(defun compile-funcall/error (function)
  (error "Function designator ~s is not a lambda form nor an oget; car is ~a::~a"
         function
         (let ((p (symbol-package (car function))))
           (if p (package-name p) "#"))
         (symbol-name (car function))))

(defun compile-funcall/args-list (args)
  (cons (if *multiple-value-p*
            '|values|
            '(internal |pv|))
        (mapcar #'convert args)))

(defun compile-funcall/function-special (fun args)
  (compile-funcall fun args))

(defun compile-funcall (function args)
  (let* ((arglist (compile-funcall/args-list args)))
    (cond
      ((eql 'setf function)
       (compile-funcall (list 'setf (first args))
                        (rest args)))
      ((and (symbolp function)
            (jscl/cl:special-operator-p function))
       (error "Special operator ~s treated as function call"
              function))
      ((and (symbolp function) 
            (jscl/cl::macro-function function))
       (error "Macro function was not expanded: ~s" function))
      ((and (symbolp function)
            (special-operator-p function))
       (error "CL special operator ~s ~
treated as function call in JSCL"
              function))
      ((and (symbolp function) 
            (macro-function function))
       (error "CL macro function was not expanded in JSCL: ~s" function))
      ((translate-function function)
       (compile-funcall/translate-function function arglist))
      ((function-name-p function)
       (compile-funcall/function function arglist))
      ((not (consp function))
       (error "Bad function designator `~S'" function))
      ((eql (car function) 'function)
       (compile-funcall/function-special (rest function) arglist))
      ((eql (car function) 'lambda)
       (compile-funcall/lambda function arglist))
      ((eql (car function) 'jscl/ffi:oget)
       (compile-funcall/oget function args))
      (t
       (compile-funcall/error function)))))

(defun convert-block (sexps &optional return-last-p decls-allowed-p)
  (multiple-value-bind (sexps decls)
      (parse-body sexps :declarations decls-allowed-p)
    (declare (ignore decls))
    (if return-last-p
        `(jscl/js::progn
           ,@(mapcar #'convert (butlast sexps))
           (jscl/js::return ,(convert (car (last sexps)) *multiple-value-p*)))
        `(jscl/js::progn ,@(mapcar #'convert sexps)))))

(defun inline-builtin-p (name)
  (and (gethash name *builtins*)
       (not (claimp name 'function 'notinline))))

(defun jscl/cl::special-operator-p (name)
  (or
   (and (eql (symbol-package name) (find-package "COMMON-LISP"))
        (gethash (intern (symbol-name name)
                         (find-package "JSCL/COMMON-LISP"))
                 *special-operators*))
   (and (eql (symbol-package name) (find-package "JSCL/COMMON-LISP"))
        (gethash name *special-operators*))))

(defun compile-special-form (name args)
  (let ((comp (gethash name *special-operators*)))
    (cond
      (comp
       (apply comp args))
      ((eql (symbol-package name) 
            (find-package "COMMON-LISP"))
       (compile-special-form (intern (symbol-name name)
                                     (find-package "JSCL/COMMON-LISP"))
                             args))
      (t (assert comp () "~S must name a special form" name)))))

(defun compile-builtin-function (name args)
  (let ((built-in (gethash name *builtins*)))
    (assert built-in (name)
            "~a does not name a built-in" name)
    (apply built-in args)))

#+jscl
(dolist (fn (+ - / * mod sqrt expt log
               round floor
               elt nth aref
               first rest last lastcar
               car cdr
               caar cadr cdar cddr
               caaar caadr cadar caddr cdaar cdadr cddar cdddr
               logand logior logxor
               and or xor not))
  (proclaim (list 'jscl::pure fn)))

(defun compile-sexp (sexp)
  (let ((name (car sexp))
        (args (cdr sexp)))
    (cond
      ((jscl/cl:special-operator-p name)
       (compile-special-form name args))
      ((inline-builtin-p name)
       (compile-builtin-function name args))
      ((jscl/cl:macro-function name)
       (cerror "Macroexpand now and continue"
               "Macro ~a encountered in funcall position; ~
It should be macroexpanded before reaching COMPILE-SEXP"
               name)
       (compile-sexp (jscl/cl:macroexpand sexp)))
      ((and (claimp name 'function 'jscl::pure)
            (every #'constantp args))
       (apply name args))
      (t (compile-funcall name args)))))

(defun convert-1/symbol (sexp)
  (let ((b (lookup-in-lexenv sexp *environment* 'variable)))
    (cond
      ((and b (not (member 'special (binding-declarations b))))
       (binding-value b))
      ((keywordp sexp)
       sexp)
      ((and b (member 'constant (binding-declarations b)))
       `(jscl/js::get ,(convert `',sexp) "value"))
      (t
       (convert `(symbol-value ',sexp))))))

(defun emit-uncompilable-form (sexp err-format &rest err-args)
  (restart-case
      (apply #'error err-format err-args)
    (replace-with-warning ()
      :report "Replace failed form with a warning"
      (warn "Error ~? replaced with a warning" err-format err-args)
      (list 'warn (format nil "Failed compilation of ~s"
                          sexp)))
    (replace-with-cerror ()
      :report "Replace failed form with a continuable error"
      (warn "Error ~? replaced with a continuable error"
            err-format err-args)
      (list 'cerror "Continue"
            (format nil "Failed compilation of ~s"
                    sexp)))
    (replace-with-error ()
      :report "Replace failed form with an error"
      (warn "Error ~? replaced with a runtime error"
            err-format err-args)
      (list 'error (format nil "Failed compilation of ~s"
                           sexp)))))

(defun should-be-macroexpanded-in-cl-p (sexp)
  (and (consp sexp)
       (symbolp (car sexp))
       ;; DESTRUCTURING-BIND is  defined via !DESTRUCTURING-BIND  and is
       ;; almost not a macro and almost,  but not quite, a special form,
       ;; so we have to work around that here.
       (not (eql 'destructuring-bind (car sexp)))
       (eql (find-package :common-lisp)
            (symbol-package (car sexp)))
       (macro-function (car sexp))))

(defun prefix-! (symbol)
  (intern (concatenate 'string "!"
                       (symbol-name symbol))
          :jscl))

(defun complain-failed-macroexpansion (sexp)
  (emit-uncompilable-form
   sexp
   "Failed to macroexpand ~s ~% in ~s~2%~a"
   (car sexp) sexp
   (format nil
           "No macro-function ~s is defined in JSCL"
           (car sexp))))

(defun check-for-failed-macroexpansion (sexp)
  "When a symbol is defined as a  macro in the host compiler, and exists
in the CL package,  it is usually a safe bet that  we should be treating
it as a macro within JSCL  as well. The notable exception (inversion) is
`DESTRUCTURING-BIND'.  This checks  for  missing  macros, which  usually
means either that  we have a compile-time dependency  ordering issue, or
just haven't gotten  around to defining that macro at  all, yet. It also
jumps out and  shouts when macro-expansion is  broken completely, rather
than baffling errors because of macro-forms being treated as functions.

FIXME redocument with !

Notably,  a  macro defined  as  !NAME  will be  used  for  NAME to  make
cross-compilation less  sticky about symbol  names in the  JSCL package.
If the  JSCL/CL package were separate  from the JSCL/INT package  or so,
this might go  away, but that will  take a good bit  of rewriting symbol
names throughout the tree. "
  (cond ((not (should-be-macroexpanded-in-cl-p sexp)) sexp)
        ((jscl/cl::macro-function (prefix-! (car sexp)))
         (warn "Substituting !~s for ~:*~s" (car sexp))
         (jscl/cl::macroexpand (cons (prefix-! (car sexp))
                                     (cdr sexp))))
        (t (complain-failed-macroexpansion sexp))))

(defun object-evaluates-to-itself-p (object)
  ;; TODO:  Things  that  “should”  evaluate  to  themselves  but  won't
  ;; actually (yet) include bignums, rationals, and complex numbers
  (or (jscl/cl::fixnump object)
      (floatp object)
      (characterp object)
      (stringp object)
      (arrayp object)
      (pathnamep object)
      (keywordp object)
      (typep object 'structure-object)))

(defun convert-1 (sexp &optional multiple-value-p)
  "Translate  SEXP  (which  can  be  a  single  symbol  or  value)  into
JavaScript  AST  form  for  the   code  generator.

Expands  all  macros.

If MULTIPLE-VALUE-P,  then it's possible  that SEXP may  return multiple
values, and  the appropriate  JavaScript wrappers must  be in  place; if
not, the simplified forms that accept  only a primary value returned can
be used."
  (multiple-value-bind (sexp expandedp) (jscl/cl::macroexpand-1 sexp)
    (when expandedp
      (return-from convert-1 (convert sexp multiple-value-p)))
    ;; The expression has been macroexpanded. Now compile it!
    (let ((sexp (check-for-failed-macroexpansion sexp)))
    (let ((*multiple-value-p* multiple-value-p)
          (*convert-level* (1+ *convert-level*)))
      (cond
          ((null sexp)	(literal nil))
          ((listp sexp)	(compile-sexp sexp))
          ((symbolp sexp)	(convert-1/symbol sexp))
          ((rational-float-p sexp)
           (literal (rational-float-p sexp)))
          ((object-evaluates-to-itself-p sexp)
         (literal sexp))
          (t (error "How should I compile ~s `~S'?"
                    (type-of sexp) sexp)))))))

(defvar *convert-recursion-guard* 100)

(defun convert (sexp &optional multiple-value-p)
  (let ((*convert-recursion-guard* (1- *convert-recursion-guard*)))
    (when (minusp *convert-recursion-guard*)
      (cerror "Continue, expecting doom"
              "CONVERT recursed very deeply (~:d past the guard limit);~
 this may be out of control."
              (- *convert-recursion-guard*)))
    (convert-1 sexp multiple-value-p)))

(defvar *compile-print-toplevels* nil)

(defun truncate-string (string &optional (width 60))
  "Truncate   a  STRING   to  no   more  than   WIDTH  characters,   and
remove newlines."
  (let ((n (or (position #\newline string)
               (min width (length string)))))
    (subseq string 0 n)))

(defun convert-toplevel-progn (sexp &optional multiple-value-p return-p)
  (warn "Top-level PROGN same as as ~r top-level form~:p"
        (length (cdr sexp)))
  `(jscl/js::progn
     ;; Discard all except the last value
     ,@(mapcar (lambda (form)
                 (convert-toplevel form nil nil))
               (butlast (cdr sexp)))
     ;; Return the last value(s)
     ,(convert-toplevel
       (first (last (cdr sexp))) multiple-value-p return-p)))

(defun convert-toplevel-defpackage (sexp)
  (warn
   "DEFPACKAGE ~a will probably not be effective within the current
file.  See  https://github.com/romance-ii/jscl/issues/30  — If  you  put
DEFPACKAGE in  a separate file  from IN-PACKAGE, everything seems  to be
just fine."
   (second sexp))
  (apply #'defpackage-real% (rest sexp))
  (convert-toplevel `(apply #'defpackage-real% (rest sexp))))

;; Based upon the one from Alexandria:
(defun hash-table-values (table)
  (let ((values nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v values))
             table)
    values))

(defun hosted-copy-package (name)
  #-jscl
  (let* ((package (jscl::find-package-or-fail name))
         (used (gethash "use" package))
         (nicknames (gethash "nicknames" package))
         #+nil (exports (gethash "exports" package)))
    (make-package (gethash "packageName" package)
                  :use (and used (hash-table-values used))
                  :nicknames (and nicknames (hash-table-values nicknames)))))

(defun convert-toplevel-in-package (sexp)
  (assert (= 2 (length sexp)))
  (let ((name (second sexp)))
    (setf *package* (or (cl:find-package name)
                        #-jscl (hosted-copy-package name)
                        #+jscl (find-package-or-fail name)))
    (convert-toplevel
     `(setq *package* (find-package-or-fail ,name)))))

(defun convert-toplevel-normal (sexp multiple-value-p return-p)
  (when *compile-print-toplevels*
    (let ((form-string (prin1-to-string sexp)))
      (format t "~&;; 𝓙𝓢ℂ𝕃 is Compiling ~a…" (truncate-string
                                              (substitute #\space #\newline
                                                          form-string)
                                              120))))

  (let ((code (convert sexp multiple-value-p)))
    (if return-p
        `(return ,code)
        code)))

(defun convert-toplevel (sexp &optional multiple-value-p return-p)
  "Macroexpand SEXP as  much as possible, and process it  as a top-level
form.
 
If MULTIPLE-VALUE-P, then SEXP may return multiple values (and they will
be   bound);    otherwise,   use   a   simpler    form   that   discards
non-primary values.

If RETURN-P, emit a JavaScript “return” operator on the value."
  (multiple-value-bind (expansion expandedp) (jscl/cl::macroexpand-1 sexp)
    (when expandedp
      (warn "Macro-expansion done on top level (~s …)…" (car sexp))
      (return-from convert-toplevel
        (convert-toplevel expansion multiple-value-p return-p))))
  ;; Process as toplevel
  (let ((*convert-level* -1))
    (cond
      ;; HACK work-around for DEFPACKAGE not working
;;; TODO  after   confirming  that  this   is  not  a   problem,  remove
;;; the warning.
      ((and (consp sexp)
            (eql (car sexp) 'defpackage))
       (convert-toplevel-defpackage sexp))
      ((and (consp sexp)
            (eql (car sexp) 'in-package)
            (= 2 (length sexp)))
       (convert-toplevel-in-package sexp))
      ;; Non-empty toplevel progn
      ((and (consp sexp)
            (eq (car sexp) 'progn)
            (cdr sexp))
       (convert-toplevel-progn sexp multiple-value-p return-p))
      (t
       (convert-toplevel-normal sexp multiple-value-p return-p)))))

(defvar *toplevel-recursion-depth* 10)

(defun process-toplevel (sexp &optional multiple-value-p return-p)
  (let ((*toplevel-compilations* nil)
        (*toplevel-recursion-depth* (1- *toplevel-recursion-depth*)))
    (when (zerop *toplevel-recursion-depth*)
      (cerror "Ignore and continue, expecting doom"
              "PROCESS-TOPLEVEL recursion guard hit. ~
More than ~:d levels of recursion were encountered."
              (eval *toplevel-recursion-depth*)))
    (let ((code (convert-toplevel sexp multiple-value-p return-p)))
      `(jscl/js::progn
         ,@(get-toplevel-compilations)
         ,code))))

(defun compile-toplevel (sexp &optional multiple-value-p return-p)
  #-jscl
  (progn
    (eval (process-toplevel sexp multiple-value-p return-p))
    (format *trace-output* "~&; compiling form ~a"
            (if (consp sexp) (car sexp) sexp))
    (finish-output *trace-output*)
    (js-format "/* Toplevel form evaluated in ~a */" (lisp-implementation-type)))
  #+jscl
  (with-output-to-string (*js-output*)
    (js (process-toplevel sexp multiple-value-p return-p))))

(defmacro with-compilation-environment (&body body)
  `(let ((*literal-table* nil)
         (*variable-counter* 0)
         (*gensym-counter* 0)
         (*literal-counter* 0)
         (*features* (list :jscl *features*)))
     (with-sharp-j
       ,@body)))

#+ (or)
(unwind-protect
     (progn
       (rename-package (find-package "JSCL/HOSTED")
                       "JSCL/HOSTED*")
       (unwind-protect
            (progn
              (rename-package (find-package "JSCL/XC") "JSCL")
              ,@body)
         (ignore-errors
           (rename-package (find-package "JSCL")
                           "JSCL/INTERMEDIATE-CROSS-COMPILATION"))))
  (ignore-errors
    (rename-package (find-package "JSCL/HOSTED*")
                    "JSCL/HOSTED")))

(defun make-macro-expander-body (name args whole environment body)
  `(function
    (lambda (,whole ,environment)
     (let ((*environment* ,environment))
       (block ,name
         (destructuring-bind ,args ,whole
           ,@body))))))

(defmacro jscl/cl::with-compilation-unit (options &body body)
  (warn "WITH-COMPILATION-UNIT currently has no effect.~@[
Ignoring options ~s~]" options)
  body)
