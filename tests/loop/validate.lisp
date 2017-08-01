;;; -*-      Mode:     LISP;      Syntax:     Common-lisp;      Package:
;;; EXTENDED-LOOP-TEST-PACKAGE; Base: 10; Lowercase:T -*-

;;; Portions of LOOP are Copyright © 1986 by the Massachusetts Institute
;;; of Technology. All Rights Reserved.

;;; Permission to use, copy, modify and distribute this software and its
;;; documentation for  any purpose  and without  fee is  hereby granted,
;;; provided that the  M.I.T. copyright notice appear in  all copies and
;;; that both that copyright notice and this permission notice appear in
;;; supporting  documentation.  The  names "M.I.T."  and  "Massachusetts
;;; Institute of Technology" may not be used in advertising or publicity
;;; pertaining to distribution of the software without specific, written
;;; prior permission.  Notice must be given  in supporting documentation
;;; that copying distribution is by permission of M.I.T. M.I.T. makes no
;;; representations  about  the suitability  of  this  software for  any
;;; purpose. It is provided "as is" without express or implied warranty.

;;; Massachusetts Institute of Technology
;; 77 Massachusetts Avenue
;;; Cambridge, Massachusetts 02139
;; United States of America
;;; +1-617-253-1000

;;; Portions  of  LOOP  are  Copyright  ©  1989,  1990,  1991,  1992  by
;;; Symbolics, Inc. All Rights Reserved.

;;; Permission to use, copy, modify and distribute this software and its
;;; documentation for  any purpose  and without  fee is  hereby granted,
;;; provided that  the Symbolics copyright  notice appear in  all copies
;;; and  that both  that  copyright notice  and  this permission  notice
;;; appear in supporting documentation. The  name "Symbolics" may not be
;;; used in advertising  or publicity pertaining to  distribution of the
;;; software without specific, written  prior permission. Notice must be
;;; given in  supporting documentation  that copying distribution  is by
;;; permission of  Symbolics. Symbolics  makes no  representations about
;;; the suitability of this software for any purpose. It is provided "as
;;; is" without express or implied warranty.

;;; Symbolics,  CLOE  Runtime,  and  Minima are  trademarks,  and  CLOE,
;;; Genera, and Zetalisp are registered trademarks of Symbolics, Inc.

;; Symbolics, Inc.
;;; 8 New England Executive Park, East
;; Burlington, Massachusetts 01803
;;; United States of America
;; +1-617-221-1000


(in-package :jscl/loop)


(defvar *slow-test*
  nil)


;; (defvar  *loop-lisp-package* (let  ((p  (car (last  (package-use-list
;; 	(find-package  'jscl/loop))))))  (format   t  "~&assuming  the
;; 	``lisp'' package used by loop is ~s.~@ if not, you must preset
;; 	jscl/loop::*loop-lisp-package*.~%" p) p))


(defmacro test (short-desc lambda-list form &body params-and-answers)
  `(test1 ,short-desc ',form ',lambda-list
          #'(lambda ,lambda-list ,form)
          ',params-and-answers))


(defun test1 (short-desc form lambda-list interpreted params-and-answers)
  (declare (ignore short-desc))
  (dolist (pair params-and-answers)
    (let ((params (first pair)) (answers (rest pair)) yow)
      ;; fixme:
      (jscl::test
       (cond
         ((equal (setq yow (multiple-value-list (apply interpreted params))) answers))
         (t
          (format t "interpreted loop form gave incorrect answer. ~%
 bindings: ~s~%
 form:     ~s ~%
 right:	  ~s ~%
 wrong:	  ~s ~%"
                  (and params (mapcar #'list lambda-list params))
                  form answers yow)
          nil))))))



#+nil
(unless (find-package 'extended-loop-test-package)
  (let ((p (make-package 'extended-loop-test-package :use (list jscl/loop::*loop-lisp-package*))))
    (shadowing-import 'symbolics-loop:loop p)
    (use-package (find-package 'symbolics-loop) p)
    p))


;;; FIXME: *package* should be bound for the file being compiled.
(in-package :cl)
