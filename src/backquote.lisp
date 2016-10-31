;;; backquote.lisp ---

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

(/debug "loading backquote.lisp!")

;;; Backquote implementation.
;;;
;;;    Author: Guy  L. Steele  Jr. Date: 27  December 1985  Tested under
;;;    Symbolics Common Lisp and Lucid  Common Lisp. This software is in
;;;    the public domain.

;;;    The following are unique tokens used during processing. They need
;;;    not be symbols; they need not even be atoms.
(defvar *comma* 'unquote)
(defvar *comma-atsign* 'unquote-splicing)

(defvar *bq-list* (make-symbol "BQ-LIST"))
(defvar *bq-append* (make-symbol "BQ-APPEND"))
(defvar *bq-list** (make-symbol "BQ-LIST*"))
(defvar *bq-nconc* (make-symbol "BQ-NCONC"))
(defvar *bq-clobberable* (make-symbol "BQ-CLOBBERABLE"))
(defvar *bq-quote* (make-symbol "BQ-QUOTE"))
(defvar *bq-quote-nil* (list *bq-quote* nil))

;;; BACKQUOTE is an ordinary macro (not a read-macro) that processes the
;;; expression foo, looking for  occurrences of #:COMMA, #:COMMA-ATSIGN,
;;; and #:COMMA-DOT.  It constructs code  in strict accordance  with the
;;; rules on pages  349-350 of the first edition (pages  528-529 of this
;;; second edition). It then optionally applies a code simplifier.

;;; If the value of *BQ-SIMPLIFY* is non-NIL, then BACKQUOTE processing applies the code simplifier.
;;; If the  value is NIL, then  the code resulting from  BACKQUOTE is exactly that  specified by the
;;; official rules.
(defparameter *bq-simplify* t)

(defmacro backquote (x)
  (bq-completely-process x))

;;; The BACKQUOTE macro should remove all occurences of UNQUOTE and UNQUOTE-SPLICING from the source
;;; before we reach them. If  we ever reach one, it must have occurred  outside a BACKQUOTE form, so
;;; we signal the appropriate error.
(defmacro unquote (x) (error "Comma not inside a backquote: ,~S" x))
(defmacro unquote-splicing (x) (error "Comma-atsign not inside a backquote: ,@~S" x))

;;; Backquote processing proceeds in three stages:
;;;
;;; (1)  BQ-PROCESS  applies  the  rules  to remove  occurrences  of  #:COMMA,  #:COMMA-ATSIGN,  and
;;; #:COMMA-DOT  corresponding  to this  level  of  BACKQUOTE. (It  also  causes  embedded calls  to
;;; BACKQUOTE  to be  expanded  so that  nesting  is properly  handled.) Code  is  produced that  is
;;; expressed in  terms of functions #:BQ-LIST,  #:BQ-APPEND, and #:BQ-CLOBBERABLE. This  is done so
;;; that  the simplifier  will  simplify  only list  construction  functions  actually generated  by
;;; BACKQUOTE  and will  not involve  any user  code in  the simplification.  #:BQ-LIST means  LIST,
;;; #:BQ-APPEND means  APPEND, and #:BQ-CLOBBERABLE means  IDENTITY but indicates places  where "%."
;;; was used and where NCONC may therefore be introduced by the simplifier for efficiency.
;;;
;;; (2) BQ-SIMPLIFY, if used, rewrites the code produced by
;;; BQ-PROCESS to produce equivalent but faster code.  The
;;; additional functions #:BQ-LIST* and #:BQ-NCONC may be
;;; introduced into the code.
;;;
;;; (3) BQ-REMOVE-TOKENS goes through the code and replaces
;;; #:BQ-LIST with LIST, #:BQ-APPEND with APPEND, and so on.
;;; #:BQ-CLOBBERABLE is simply eliminated (a call to it being
;;; replaced by its argument).  #:BQ-LIST* is replaced by either
;;; LIST* or CONS (the latter is used in the two-argument case,
;;; purely to make the resulting code a tad more readable).

(defun bq-completely-process (x)
  (let ((raw-result (bq-process x)))
    (bq-remove-tokens (if *bq-simplify*
                          (bq-simplify raw-result)
                          raw-result))))

(defun bq-process (x)
  (cond ((atom x)
         (list *bq-quote* x))
        ((eq (car x) 'backquote)
         (bq-process (bq-completely-process (cadr x))))
        ((eq (car x) *comma*) (cadr x))
        ((eq (car x) *comma-atsign*)
         (error ",@~S after `" (cadr x)))
        ;; ((eq (car x) *comma-dot*)
        ;;  ;; (error ",.~S after `" (cadr x))
        ;;  (error "ill-formed"))
        (t (do ((p x (cdr p))
                (q '() (cons (bracket (car p)) q)))
               ((atom p)
                (cons *bq-append*
                      (nreconc q (list (list *bq-quote* p)))))
             (when (eq (car p) *comma*)
               (unless (null (cddr p))
                 (error "Malformed ,~S" p))
               (return (cons *bq-append*
                             (nreconc q (list (cadr p))))))
             (when (eq (car p) *comma-atsign*)
               (error "Dotted ,@~S" p))
             ;; (when (eq (car p) *comma-dot*)
             ;;   ;; (error "Dotted ,.~S" p)
             ;;   (error "Dotted"))
             ))))

;;; This implements the bracket operator of the formal rules.
(defun bracket (x)
  (cond ((atom x)
         (list *bq-list* (bq-process x)))
        ((eq (car x) *comma*)
         (list *bq-list* (cadr x)))
        ((eq (car x) *comma-atsign*)
         (cadr x))
        ;; ((eq (car x) *comma-dot*)
        ;;  (list *bq-clobberable* (cadr x)))
        (t (list *bq-list* (bq-process x)))))

;;; This auxiliary function is like MAPCAR but has two extra
;;; purposes: (1) it handles dotted lists; (2) it tries to make
;;; the result share with the argument x as much as possible.
(defun maptree (fn x)
  (if (atom x)
      (funcall fn x)
      (let ((a (funcall fn (car x)))
            (d (maptree fn (cdr x))))
        (if (and (eql a (car x)) (eql d (cdr x)))
            x
            (cons a d)))))

;;; This predicate is true of a form that when read looked
;;; like %@foo or %.foo.
(defun bq-splicing-frob (x)
  (and (consp x)
       (or (eq (car x) *comma-atsign*)
           ;; (eq (car x) *comma-dot*)
           )))

;;; This predicate is true of a form that when read
;;; looked like %@foo or %.foo or just plain %foo.
(defun bq-frob (x)
  (and (consp x)
       (or (eq (car x) *comma*)
           (eq (car x) *comma-atsign*)
           ;; (eq (car x) *comma-dot*)
           )))

;;; The simplifier essentially looks for calls to #:BQ-APPEND and
;;; tries to simplify them.  The arguments to #:BQ-APPEND are
;;; processed from right to left, building up a replacement form.
;;; At each step a number of special cases are handled that,
;;; loosely speaking, look like this:
;;;
;;;  (APPEND (LIST a b c) foo) => (LIST* a b c foo)
;;;       provided a, b, c are not splicing frobs
;;;  (APPEND (LIST* a b c) foo) => (LIST* a b (APPEND c foo))
;;;       provided a, b, c are not splicing frobs
;;;  (APPEND (QUOTE (x)) foo) => (LIST* (QUOTE x) foo)
;;;  (APPEND (CLOBBERABLE x) foo) => (NCONC x foo)
(defun bq-simplify (x)
  (if (atom x)
      x
      (let ((x (if (eq (car x) *bq-quote*)
                   x
                   (maptree #'bq-simplify x))))
        (if (not (eq (car x) *bq-append*))
            x
            (bq-simplify-args x)))))

(defun bq-simplify-args (x)
  (do ((args (reverse (cdr x)) (cdr args))
       (result
        nil
        (cond ((atom (car args))
               (bq-attach-append *bq-append* (car args) result))
              ((and (eq (caar args) *bq-list*)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses (cdar args) result))
              ((and (eq (caar args) *bq-list**)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses
                (reverse (cdr (reverse (cdar args))))
                (bq-attach-append *bq-append*
                                  (car (last (car args)))
                                  result)))
              ((and (eq (caar args) *bq-quote*)
                    (consp (cadar args))
                    (not (bq-frob (cadar args)))
                    (null (cddar args)))
               (bq-attach-conses (list (list *bq-quote*
                                             (caadar args)))
                                 result))
              ((eq (caar args) *bq-clobberable*)
               (bq-attach-append *bq-nconc* (cadar args) result))
              (t (bq-attach-append *bq-append*
                                   (car args)
                                   result)))))
      ((null args) result)))

(defun null-or-quoted (x)
  (or (null x) (and (consp x) (eq (car x) *bq-quote*))))

;;; When BQ-ATTACH-APPEND is called, the OP should be #:BQ-APPEND
;;; or #:BQ-NCONC.  This produces a form (op item result) but
;;; some simplifications are done on the fly:
;;;
;;;  (op '(a b c) '(d e f g)) => '(a b c d e f g)
;;;  (op item 'nil) => item, provided item is not a splicable frob
;;;  (op item 'nil) => (op item), if item is a splicable frob
;;;  (op item (op a b c)) => (op item a b c)
(defun bq-attach-append (op item result)
  (cond ((and (null-or-quoted item) (null-or-quoted result))
         (list *bq-quote* (append (cadr item) (cadr result))))
        ((or (null result) (equal result *bq-quote-nil*))
         (if (bq-splicing-frob item) (list op item) item))
        ((and (consp result) (eq (car result) op))
         (list* (car result) item (cdr result)))
        (t (list op item result))))

;;; The effect of BQ-ATTACH-CONSES is to produce a form as if by
;;; `(LIST* ,@items ,result) but some simplifications are done
;;; on the fly.
;;;
;;;  (LIST* 'a 'b 'c 'd) => '(a b c . d)
;;;  (LIST* a b c 'nil) => (LIST a b c)
;;;  (LIST* a b c (LIST* d e f g)) => (LIST* a b c d e f g)
;;;  (LIST* a b c (LIST d e f g)) => (LIST a b c d e f g)
(defun bq-attach-conses (items result)
  (cond ((and (every #'null-or-quoted items)
              (null-or-quoted result))
         (list *bq-quote*
               (append (mapcar #'cadr items) (cadr result))))
        ((or (null result) (equal result *bq-quote-nil*))
         (cons *bq-list* items))
        ((and (consp result)
              (or (eq (car result) *bq-list*)
                  (eq (car result) *bq-list**)))
         (cons (car result) (append items (cdr result))))
        (t (cons *bq-list** (append items (list result))))))

;;; Removes funny tokens and changes (#:BQ-LIST* a b) into
;;; (CONS a b) instead of (LIST* a b), purely for readability.
(defun bq-remove-tokens (x)
  (cond ((eq x *bq-list*) 'list)
        ((eq x *bq-append*) 'append)
        ((eq x *bq-nconc*) 'nconc)
        ((eq x *bq-list**) 'list*)
        ((eq x *bq-quote*) 'quote)
        ((atom x) x)
        ((eq (car x) *bq-clobberable*)
         (bq-remove-tokens (cadr x)))
        ((and (eq (car x) *bq-list**)
              (consp (cddr x))
              (null (cdddr x)))
         (cons 'cons (maptree #'bq-remove-tokens (cdr x))))
        (t (maptree #'bq-remove-tokens x))))
