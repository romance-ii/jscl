;;; boot.lisp --- First forms to be cross compiled

;; Copyright (C) 2012, 2013 David Vazquez
;; Copyright (C) 2012 Raimon Grau

;; JSCL is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; JSCL is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with JSCL.  If not, see <http://www.gnu.org/licenses/>.

;;; This code is executed when JSCL compiles this file itself. The
;;; compiler provides compilation of some special forms, as well as
;;; funcalls and macroexpansion, but no functions. So, we define the
;;; Lisp world from scratch. This code has to define enough language
;;; to the compiler to be able to run.

(/debug "loading boot.lisp!")

(eval-when (:compile-toplevel)
  (let ((defmacro-macroexpander
         '#'(lambda (form)
              (destructuring-bind (name args &body body)
                  form
                (let* ((whole (gensym))
                       (expander `(function
                                   (lambda (,whole)
                                    (block ,name
                                      (destructuring-bind ,args ,whole
                                        ,@body))))))

                  ;; If we are boostrapping JSCL, we need to quote the
                  ;; macroexpander, because the macroexpander will
                  ;; need to be dumped in the final environment
                  ;; somehow.
                  (when (find :jscl-xc *features*)
                    (setq expander `(quote ,expander)))
                  
                  `(eval-when (:compile-toplevel :execute)
                     (%compile-defmacro ',name ,expander))

                  )))))
    
    (%compile-defmacro 'defmacro defmacro-macroexpander)))

(defmacro declaim (&rest decls)
  `(eval-when (:compile-toplevel :execute)
     ,@(mapcar (lambda (decl) `(!proclaim ',decl)) decls)))

(defmacro defconstant (name value &optional docstring)
  `(progn
     (declaim (special ,name))
     (declaim (constant ,name))
     (setq ,name ,value)
     ,@(when (stringp docstring) `((oset ,docstring ',name "vardoc")))
     ',name))

(defconstant t 't)
(defconstant nil 'nil)
(%js-vset "nil" nil)
(%js-vset "t" t)

(defmacro lambda (args &body body)
  `(function (lambda ,args ,@body)))

(defmacro when (condition &body body)
  `(if ,condition (progn ,@body) nil))

(defmacro unless (condition &body body)
  `(if ,condition nil (progn ,@body)))

(defmacro defvar (name &optional (value nil value-p) docstring)
  `(progn
     (declaim (special ,name))
     ,@(when value-p `((unless (boundp ',name) (setq ,name ,value))))
     ,@(when (stringp docstring) `((oset ,docstring ',name "vardoc")))
     ',name))

(defmacro defparameter (name value &optional docstring)
  `(progn
     (setq ,name ,value)
     ,@(when (stringp docstring) `((oset ,docstring ',name "vardoc")))
     ',name))

(defmacro defun (name args &rest body)
  `(progn
     (eval-when (:compile-toplevel)
       (fn-info ',name :defined t))
     (fset ',name #'(named-lambda ,name ,args ,@body))
     ',name))

(defmacro return (&optional value)
  `(return-from nil ,value))

(defmacro while (condition &body body)
  `(block nil (%while ,condition ,@body)))

(defvar *gensym-counter* 0)
(defun gensym (&optional (prefix "G"))
  (setq *gensym-counter* (+ *gensym-counter* 1))
  (make-symbol (concat prefix (integer-to-string *gensym-counter*))))

(defun boundp (x)
  (boundp x))

(defun fboundp (x)
  (fboundp x))

(defun eq (x y) (eq x y))
(defun eql (x y) (eq x y))

(defun not (x) (if x nil t))

(defun funcall (function &rest args)
  (apply function args))

(defun apply (function arg &rest args)
  (apply function (apply #'list* arg args)))

;; Basic macros

(defmacro dolist ((var list &optional result) &body body)
  (let ((g!list (gensym)))
    (unless (symbolp var) (error "`~S' is not a symbol." var))
    `(block nil
       (let ((,g!list ,list)
             (,var nil))
         (%while ,g!list
                 (setq ,var (car ,g!list))
                 (tagbody ,@body)
                 (setq ,g!list (cdr ,g!list)))
         ,result))))

(defmacro dotimes ((var count &optional result) &body body)
  (let ((g!count (gensym)))
    (unless (symbolp var) (error "`~S' is not a symbol." var))
    `(block nil
       (let ((,var 0)
             (,g!count ,count))
         (%while (< ,var ,g!count)
                 (tagbody ,@body)
                 (incf ,var))
         ,result))))

(defmacro cond (&rest clausules)
  (unless (null clausules)
    (destructuring-bind (condition &body body)
        (first clausules)
      (cond
        ((eq condition t)
         `(progn ,@body))
        ((null body)
         (let ((test-symbol (gensym)))
           `(let ((,test-symbol ,condition))
              (if ,test-symbol
                  ,test-symbol
                  (cond ,@(rest clausules))))))
        (t
         `(if ,condition
              (progn ,@body)
              (cond ,@(rest clausules))))))))

(defmacro case (form &rest clausules)
  (let ((!form (gensym)))
    `(let ((,!form ,form))
       (cond
         ,@(mapcar (lambda (clausule)
                     (destructuring-bind (keys &body body)
                         clausule
                       (if (or (eq keys 't) (eq keys 'otherwise))
                           `(t nil ,@body)
                           (let ((keys (if (listp keys) keys (list keys))))
                             `((or ,@(mapcar (lambda (key) `(eql ,!form ',key)) keys))
                               nil ,@body)))))
                   clausules)))))

(defmacro ecase (form &rest clausules)
  (let ((g!form (gensym)))
    `(let ((,g!form ,form))
       (case ,g!form
         ,@(append
            clausules
            `((t
               (error "ECASE expression failed for the object `~S'." ,g!form))))))))

(defmacro and (&rest forms)
  (cond
    ((null forms)
     t)
    ((null (cdr forms))
     (car forms))
    (t
     `(if ,(car forms)
          (and ,@(cdr forms))
          nil))))

(defmacro or (&rest forms)
  (cond
    ((null forms)
     nil)
    ((null (cdr forms))
     (car forms))
    (t
     (let ((g (gensym)))
       `(let ((,g ,(car forms)))
          (if ,g ,g (or ,@(cdr forms))))))))

(defmacro prog1 (form &body body)
  (let ((value (gensym)))
    `(let ((,value ,form))
       ,@body
       ,value)))

(defmacro prog2 (form1 result &body body)
  `(prog1 (progn ,form1 ,result) ,@body))

(defmacro prog (inits &rest body )
  (multiple-value-bind (forms decls docstring) (parse-body body)
    `(block nil
       (let ,inits
         ,@decls
         (tagbody ,@forms)))))

(defmacro psetq (&rest pairs)
  (let (;; For each pair, we store here a list of the form
        ;; (VARIABLE GENSYM VALUE).
        (assignments '()))
    (while t
      (cond
        ((null pairs) (return))
        ((null (cdr pairs))
         (error "Odd paris in PSETQ"))
        (t
         (let ((variable (car pairs))
               (value (cadr pairs)))
           (push `(,variable ,(gensym) ,value)  assignments)
           (setq pairs (cddr pairs))))))
    (setq assignments (reverse assignments))
    ;;
    `(let ,(mapcar #'cdr assignments)
       (setq ,@(!reduce #'append (mapcar #'butlast assignments) nil)))))

(defmacro do (varlist endlist &body body)
  `(block nil
     (let ,(mapcar (lambda (x) (if (symbolp x)
                                   (list x nil)
                                 (list (first x) (second x)))) varlist)
       (while t
         (when ,(car endlist)
           (return (progn ,@(cdr endlist))))
         (tagbody ,@body)
         (psetq
          ,@(apply #'append
                   (mapcar (lambda (v)
                             (and (listp v)
                                  (consp (cddr v))
                                  (list (first v) (third v))))
                           varlist)))))))

(defmacro do* (varlist endlist &body body)
  `(block nil
     (let* ,(mapcar (lambda (x1) (if (symbolp x1)
                                     (list x1 nil)
                                   (list (first x1) (second x1)))) varlist)
       (while t
         (when ,(car endlist)
           (return (progn ,@(cdr endlist))))
         (tagbody ,@body)
         (setq
          ,@(apply #'append
                   (mapcar (lambda (v)
                             (and (listp v)
                                  (consp (cddr v))
                                  (list (first v) (third v))))
                           varlist)))))))

(defmacro loop (&body body)
  `(while t ,@body))

(defun identity (x) x)

(defun complement (x)
  (lambda (&rest args)
    (not (apply x args))))

(defun constantly (x)
  (lambda (&rest args)
    x))

(defun code-char (x)
  (code-char x))

(defun char-code (x)
  (char-code x))

(defun char= (x y)
  (eql x y))

(defun char< (x y)
  (< (char-code x) (char-code y)))

(defun atom (x)
  (not (consp x)))

(defun alpha-char-p (x)
  (or (<= (char-code #\a) (char-code x) (char-code #\z))
      (<= (char-code #\A) (char-code x) (char-code #\Z))))

;; I made this list by running DIGIT-CHAR-P in SBCL on every codepoint up to CHAR-CODE-LIMIT,
;; filtering on only those with SB-IMPL::UCD-GENERAL-CATEGORY 12 (Nd), and then grouping
;; consecutive sets.  There's 37 spans of 10, plus 1 extra digit (6618).
(defconstant +unicode-zeroes+
  '(48 1632 1776 1984 2406 2534 2662 2790 2918 3046 3174 3302 3430 3664
    3792 3872 4160 4240 6112 6160 6470 6608 6784 6800 6992 7088 7232 7248
    42528 43216 43264 43472 43600 44016 65296 66720 120782)
  "Unicode codepoints which have Digit value 0, followed by 1, 2, ..., 9, as of Unicode 6.2")

;; The "Digit value" of a (Unicode) character, or NIL, if it doesn't have one.
(defun unicode-digit-value (char)
  (let ((code (char-code char)))
    (if (= code 6618)
        1  ;; it's special!
        (dolist (z +unicode-zeroes+)
          (when (<= z code (+ z 9))
            (return-from unicode-digit-value (- code z)))))))

;; From comment #4 on <https://bugs.launchpad.net/sbcl/+bug/1177986>:
(defun digit-char-p (char &optional (radix 10))
  "Includes ASCII 0-9 a-z A-Z, plus Unicode HexDigit characters (fullwidth variants of 0-9 and A-F)."
  (let* ((number (unicode-digit-value char))
         (code (char-code char))
         (upper (char-upcase char))
         (code-upper (char-code upper))
         (potential (cond (number number) 
                          ((char<= #\A upper #\Z)
                           (+ 10 (- code-upper (char-code #\A))))
                          ((<= 65313 (char-code upper) 65318)  ;; FULLWIDTH_LATIN_CAPITAL_LETTER_A, FULLWIDTH_LATIN_CAPITAL_LETTER_F
                           (+ 10 (- code-upper 65313)))
                          (t nil))))
    (if (and potential (< potential radix))
        potential
        nil)))

(defun digit-char (weight &optional (radix 10))
  "All arguments must be integers. Returns a character object that represents
a digit of the given weight in the specified radix. Returns NIL if no such
character exists."
  
  (and (>= weight 0) (< weight radix) (< weight 36)
       (or (and (<= 0 weight 9)
                (code-char (+ (char-code #\0) weight)))
           (and (<= 10 weight 36)
                (code-char (+ (char-code #\A) weight))))))

(defun equal (x y)
  (cond
    ((eql x y) t)
    ((numberp x) (and (numberp y) (= x y)))
    ((consp x)
     (and (consp y)
          (equal (car x) (car y))
          (equal (cdr x) (cdr y))))
    ((stringp x)
     (and (stringp y) (string= x y)))
    (t nil)))

(defun equalp (x y)
  "This is a marginally correct implementation of EQUALP"
  (cond
    ((eql x y) t)
    ((numberp x) (and (numberp y) (= x y)))
    ((consp x)
     (and (consp y)
          (equalp (car x) (car y))
          (equalp (cdr x) (cdr y))))
    ((characterp x)
     (and (characterp y)
          (char-equal x y)))
    ((vectorp x)
     (and (vectorp y)
          (= (length x) (length y))
          (every #'equalp x y)))
    (t nil)))

(defun fdefinition (x)
  (cond
    ((functionp x)
     x)
    ((symbolp x)
     (symbol-function x))
    (t
     (error "Invalid function `~S'." x))))

(defun disassemble (function)
  (write-line (lambda-code (fdefinition function)))
  nil)

(defmacro multiple-value-bind (variables value-from &body body)
  `(multiple-value-call (lambda (&optional ,@variables &rest ,(gensym))
                          ,@body)
     ,value-from))

(defmacro multiple-value-list (value-from)
  `(multiple-value-call #'list ,value-from))


;; Incorrect typecase, but used in NCONC.
(defmacro typecase (x &rest clausules)
  (let ((value (gensym)))
    `(let ((,value ,x))
       (cond
         ,@(mapcar (lambda (c)
                     (if (find (car c) '(t otherwise))
                         `(t ,@(rest c))
                         `((,(ecase (car c)
                                    (integer 'integerp)
                                    (cons 'consp)
                                    (list 'listp)
                                    (vector 'vectorp)
                                    (character 'characterp)
                                    (sequence 'sequencep)
                                    (symbol 'symbolp)
                                    (keyword 'keywordp)
                                    (function 'functionp)
                                    (float 'floatp)
                                    (array 'arrayp)
                                    (string 'stringp)
                                    (atom 'atom)
                                    (null 'null)
                                    (package 'packagep))
                             ,value)
                           ,@(or (rest c)
                                 (list nil)))))
                   clausules)))))

(defmacro etypecase (x &rest clausules)
  (let ((g!x (gensym)))
    `(let ((,g!x ,x))
       (typecase ,g!x
         ,@clausules
         (t (error "~S fell through etypecase expression." ,g!x))))))

(defun notany (fn seq)
  (not (some fn seq)))

(defconstant internal-time-units-per-second 1000)

(defun get-internal-real-time ()
  (get-internal-real-time))

(defun get-unix-time ()
  (truncate (/ (get-internal-real-time) 1000)))

(defun get-universal-time ()
  (+ (get-unix-time) 2208988800))

(defun values-list (list)
  (values-array (list-to-vector list)))

(defun values (&rest args)
  (values-list args))

;;; Early error definition.
(defun error (fmt &rest args)
  (%throw (apply #'format nil fmt args)))

(defmacro nth-value (n form)
  `(multiple-value-call (lambda (&rest values)
                          (nth ,n values))
     ,form))



(defvar *print-escape* t)
(defvar *print-readably* t)
(defvar *print-circle* nil)
(defvar *print-radix* nil)
(defvar *print-base* 10)
(defvar *read-base* 10)
