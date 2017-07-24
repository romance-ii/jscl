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
           #:annotate)
  (:documentation "Generate source maps for the output JavaScript. 

Source  maps allow  the  in-browser  debugger in  Firefox  or Chrome  to
identify the  actual Common Lisp  forms and source code  when debugging.
When correctly processed, these can survive minification through Closure
or Uglify as well.

The source map format itself is defined at
 https://docs.google.com/document/d/1U1RGAehQwRypUTovF1KRlpiOFze0b-_2gc6fAH0KY0k"))

(in-package :jscl/source-map)

(defvar *source-map* nil
  "The source map  data collected for the current output  file is stored
  here in a simple list.")

(defstruct source-mapping 
  symbol-name
  output-line
  output-column
  source-file
  source-line
  source-column)

(defun numberize (list &key (test 'eql))
  "Create a hash-table to quickly find the index (0-based) of an item in LIST.

The source file  and names are referenced using  relative indices within
the list, and numbering the list makes that process less onerous."
  (loop with table = (make-hash-table :test test)
     for i from 0
     for item in list
     do (setf (gethash item table) i)
     finally (return table)))

(defun zig-zag-signed (number)
  "Convert signed NUMBER (an integer) into a zig-zag signed value.

Essentially, this is the absolute  value with a low-order bit indicating
the  sign.  It's  used  in   source  mapping  to  allow  variable-length
quantities (VLQ) since 2's-complement requires a known bit-width."
  (check-type number integer)
  (logior (ash (abs number) 1) (if (minusp number) 1 0)))

(defun encode-5-bit (number)
  "Given a positive integer NUMBER, return it as a list of 5-bit bytes."
  (check-type number (integer 0 *))
  (if (zerop number)
      (list 0)
      (loop for byte-index below (ceiling (log number 2) 5)
         collecting (ldb (byte 5 (* 5 byte-index)) number))))

(eval-when (:load-toplevel :compile-toplevel)
  (defun make-char-seq (begin end)
    "Create a string of characters from BEGIN through END (inclusive)."
    (coerce (loop for ch = begin then (code-char (1+ (char-code ch)))
               until (char> ch end) 
               collecting ch)
            'string)))

(defun base-64 (byte)
  "Convert a six-bit BYTE to a base64-encoded character."
  (check-type byte (integer 0 (#x80)) "a six-bit byte")
  (position byte #. (concatenate 'string 
                                 (make-char-seq #\A #\Z)
                                 (make-char-seq #\a #\z)
                                 (make-char-seq #\0 #\9)
                                 "+/")))

(defun base-64-vlq (number)
  "Convert NUMBER  into a  base64-encoded Variable Length  Quantity with
zig-zag sign."
  (let ((bytes (encode-5-bit (zig-zag-signed number)))) 
    (concatenate 'string
                 (coerce (loop for byte in (subseq bytes 0 (1- (length bytes)))
                            collecting (base-64 (+ byte #b100000)))
                         'string)
                 (string (base-64 (last bytes))))))

(defmacro relative-number (absolute reference)
  "Return the distance  from REFERENCE to ABSOLUTE  and update REFERENCE
to be ABSOLUTE now."
  `(prog1
       (- ,absolute ,reference)
     (setf ,reference ,absolute)))

(defvar *last-output-column* 0
  "The last output-column recorded into the current source-map line.
NB. this is the only field that resets per output line (the others reset
per output file)")
(defvar *last-source-index* 0
  "The last source-file index recorded into the current source-map file.")
(defvar *last-source-line* 0
  "The last source line number recorded into the current source-map file.")
(defvar *last-source-column* 0
  "The last source column number recorded into the current source-map file.")
(defvar *last-name-index* 0
  "The last symbol-name index recorded into the current source-map file.")

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

(defun boring-character-p (char)
  "True  if CHAR  is not  something  likely to  blow up  in the  default
readtable in a symbol-name.

ie, not  a macro-character in  the reader,  not a control  or whitespace
character,  and not  a lower-case  character (assuming  *READTABLE-CASE*
will upcase it)"
  (and (not (find char #(#\# #\" #\' #\| #\\ #\, #\. #\( #\) #\[ #\] #\{ #\} #\: #\;)))
       (not (lower-case-p char))
       (let ((code (char-code char)))
         (and (> code #x20)
              (not (<= #x7f code #xa0))))))

(defun backslash-escape (string &optional other-char)
  (let ((output-string (make-array (length string) :element-type 'character
                                   :fill-pointer 0 :adjustable t)))
    (dotimes (i (length string))
      (let ((char (char string i)))
        (cond 
          ((char= char other-char)
           (vector-push-extend #\\ output-string)
           (vector-push-extend other-char output-string))
          ((char= char #\\)
           (vector-push-extend #\\ output-string)
           (vector-push-extend #\\ output-string))
          (t (vector-push-extend char output-string)))))
    output-string))

(defun doubly-quoted (string)
  (concatenate 'string "\"" (backslash-escape string #\") "\""))

(defun symbol-maybe-escaped (name)
  "Return NAME with | | delimiters if necessary."
  (if (every #'boring-character-p name)
      (string name)
      (concatenate 'string "|" (backslash-escape name #\|) "|")))

(defun symbol-name-string (symbol)
  "Returns a string  form of SYMBOL that is  always fully-qualified, for
use in source maps.

Does  not  respect/care about  exported  symbols  or *PACKAGE*;  so  all
symbols  will  be fully-qualified  with  ::,  except uninterned  symbols
and keywords.

Package or  symbol names with  any “unusual” characters will  be escaped
with #\| characters.

   #:UNINTERNED
   :KEYWORD
   PACKAGE-NAME::SYMBOL-NAME
   |Funny package name|::|Funny symbol name|"
  (cond 
    ((null (symbol-package symbol))
     (concatenate 'string "#:" (symbol-maybe-escaped (symbol-name symbol))))
    ((equal "KEYWORD" (package-name (symbol-package symbol)))
     (concatenate 'string ":" (symbol-maybe-escaped (symbol-name symbol))))
    (t (concatenate 
        'string 
        (symbol-maybe-escaped (package-name (symbol-package symbol))) 
        "::"
        (symbol-maybe-escaped (symbol-name symbol))))))

(defun annotate (&key (source-map *source-map*)
                      symbol
                      output-line
                      output-column
                      source-file
                      source-line
                      source-column)
  "Record a source→output mapping for later generating a source map file."
  (push (make-source-mapping 
         :symbol-name	(symbol-name-string symbol)
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
  "Writes the SOURCE-MAP as a JSON object to STREAM."
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
    (format stream "\"sources\": [~{~a~^, ~}],~%" 
            (mapcar #'doubly-quoted files))
    (format stream "\"names\": [~{~a~^, ~}],~%" 
            (mapcar #'doubly-quoted names))
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
  "Process source files that will  be concatenated into one output file.
When  BODY  completes,  write  the   source-map  (created  by  calls  to
`ANNOTATE' from  within BODY's dynamic context)  to SOURCE-MAP-FILE-NAME
in JSON form."
  `(let ((*source-map* nil)) 
     (prog1 
         (progn ,@body)
       (with-open-file (source-map ,source-map-file-name
                                   :direction :output
                                   :if-exists
                                   #+jscl :new-version
                                   #+unix :supersede
                                   #- (or jscl unix) (error "IF-EXISTS … ?"))
         (print-source-map
          :stream source-map
          :file-name ,source-map-file-name)))))

