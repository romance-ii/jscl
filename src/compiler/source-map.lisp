;;;; compiler/source-map.lisp — support for JavaScript source-maps

;;; This  module collects  source  mapping data  and  attempts to  write
;;; a valid  JSON source map  file for the generated  JavaScript output.
;;; Its intention is to be accurate to the top-level form, at least.

;; © 2017 Bruce-Robert Fenn Pocock <brfennpocock@star-hope.org>

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

(defpackage :jscl/source-map
  (:use :common-lisp)
  (:export #:print-source-map
           #:with-source-map-output-file
           #:annotate))

(in-package :jscl/source-map)

(defvar *source-map* nil)

(defstruct source-mapping 
  symbol-name
  output-line
  output-column
  source-file
  source-line
  source-column)

(defun numberize (list &key (test 'eql))
  "Create a hash-table to quickly find the index (0-based) of an item in LIST"
  (loop with table = (make-hash-table :test test)
     for i from 0
     for item in list
     do (setf (gethash item table) i)
     finally (return table)))

(defun zig-zag-signed (number)
  (check-type number integer)
  (logior (ash (abs number) 1) (if (minusp number) 1 0)))

(defun encode-5-bit (number)
  (check-type number (integer 0 *))
  (if (zerop number)
      (list 0)
      (loop for byte-index below (ceiling (log number 2) 5)
         collecting (ldb (byte 5 (* 5 byte-index)) number))))

(eval-when (:load-toplevel :compile-toplevel)
  (defun make-char-seq (begin end)
    (coerce (loop for ch = begin then (code-char (1+ (char-code ch)))
               until (char> ch end) 
               collecting ch)
            'string)))

(defun base-64 (byte)
  (position byte #. (concatenate 'string 
                                 (make-char-seq #\A #\Z)
                                 (make-char-seq #\a #\z)
                                 (make-char-seq #\0 #\9)
                                 "+/")))

(defun base-64-vlq (number)
  (let ((bytes (encode-5-bit (zig-zag-signed number)))) 
    (concatenate 'string
                 (coerce (loop for byte in (subseq bytes 0 (1- (length bytes)))
                            collecting (base-64 (+ byte #b100000)))
                         'string)
                 (string (base-64 (last bytes))))))

(defmacro relative-number (absolute reference)
  `(prog1
       (- ,absolute ,reference)
     (setf ,reference ,absolute)))

(defvar *last-output-column* 0)
(defvar *last-source-index* 0)
(defvar *last-source-line* 0)
(defvar *last-source-column* 0)
(defvar *last-name-index* 0)

(defun source-map-encode-segment (mapping &key name-indices file-indices)
  (apply 
   #'concatenate 'string
   (mapcar 
    #'base-64-vlq 
    (list (relative-number (source-mapping-output-column mapping)
                           *last-output-column*)
          (relative-number (gethash (source-mapping-source-file mapping) 
                                    file-indices)
                           *last-source-index*)
          (relative-number (source-mapping-source-line mapping)
                           *last-source-line*)
          (relative-number (source-mapping-source-column mapping)
                           *last-source-column*)
          (relative-number (gethash (source-mapping-symbol-name mapping) 
                                    name-indices)
                           *last-name-index*)))))

(defun annotate (&key (source-map *source-map*)
                      symbol-name
                      output-line
                      output-column
                      source-file
                      source-line
                      source-column)
  (push (make-source-mapping 
         :symbol-name	symbol-name
         :output-line	output-line
         :output-column	output-column
         :source-file	source-file
         :source-line	source-line
         :source-column	source-column)
        source-map)
  nil)

(defun print-source-map (&key 
                           stream
                           file-name
                           source-root 
                           (source-map *source-map*))
  (format stream "{ \"version\": 3,
~@[\"file\": \"~a\",~]
~@[\"sourceRoot\": \"~a\",~]"
          file-name source-root)
  (let* ((names (remove-duplicates (mapcar #'source-mapping-symbol-name
                                           source-map)))
         (name-indices (numberize names))
         (files (remove-duplicates (mapcar #'source-mapping-source-file 
                                           source-map)
                                   :test #'string=))
         (file-indices (numberize files :test #'string=))
         (max-output-line (loop for line 
                             in (mapcar #'source-mapping-output-line 
                                        source-map)
                             maximizing line))
         (*last-source-index* 0)
         (*last-source-line* 0)
         (*last-source-column* 0)
         (*last-name-index* 0))
    (format stream "\"sources\": [~{\"~a\"~^, ~}],~%" files)
    (format stream "\"names\": [~{\"~a\"~^, ~}],~%" names)
    (princ "\"mappings\": \"" stream)
    (dotimes (output-line (1+ max-output-line))
      ;; XXX this is not very efficient.
      (setf *last-output-column* 0)
      (format stream
              "~{~a~^,~}"
              (mapcar
               (lambda (mapping)
                 (source-map-encode-segment mapping
                                            :name-indices name-indices
                                            :file-indices file-indices))
               (stable-sort (copy-list 
                             (remove-if-not 
                              (lambda (mapping)
                                (= output-line
                                   (source-mapping-output-line mapping))) 
                              source-map))
                            #'<
                            :key #'source-mapping-output-column)))
      (unless (= output-line max-output-line)
        (princ ";" stream))
      ))
  (princ "\"}" stream)
  (terpri stream)
  (finish-output stream) 
  t)

(defmacro with-source-map-output-file ((source-map-file-name) &body body)
  `(let ((*source-map* nil)) 
     (prog1 
         (progn ,@body)
       (with-open-file (source-map ,source-map-file-name
                                   :direction :output
                                   :if-exists
                                   #+jscl :new-version
                                   #+unix :supersede
                                   #- (or jscl unix) (error "IF-EXISTS … ?"))
         (print-source-map :stream source-map :file-name ,source-map-file-name)))))

