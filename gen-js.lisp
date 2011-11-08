(defun ignorable-builtin (name)
  (member name '("SPARE1" "SPARE2" "LBL2") :test #'string=))

(defun aliases-for (name)
  (or (when (cl-ppcre:scan ".+\\!\\=.+" name)
        (list (cl-ppcre:regex-replace "(.+)\\!\\=(.+)" name "\\1 NE \\2")))
      (when (cl-ppcre:scan "\\}" name)
        (nconc (list (cl-ppcre:regex-replace "\\}" name "->"))
               (list (cl-ppcre:regex-replace "\\}" name "-"))))
      (when (string= name "ENTER^") (list "ENTER"))
      (when (string= name "R^") (list "RUP"))
      (when (string= name "LBL") (list "*LBL"))))

(defun get-builtins (infile &optional
                            (one-byte-ht (make-hash-table :test #'equal))
                            (two-byte-ht (make-hash-table :test #'equal)))
  (flet ((set-entry (table name value)
           (assert (null (gethash name table)))
           (setf (gethash name table) value)))
    (with-open-file (f infile  :direction :input)
      (loop for line = (read-line f nil)
            while line
            do (cl-ppcre:register-groups-bind (one-byte two-byte name (#'parse-integer value))
                   ("\\$(?:(single)|(twobyte))\\{\"(.*)\"\\}\\s*=\\s*\"(.*)\";\\s*" line)
                 (declare (ignore two-byte))
                 (unless (ignorable-builtin name)
                   (set-entry (if one-byte one-byte-ht two-byte-ht) name value)
                   (loop for alias in (aliases-for name)
                         do (set-entry (if one-byte one-byte-ht two-byte-ht) alias value)))))))
  (values one-byte-ht two-byte-ht))

(defun get-xrom (infile &optional (ht (make-hash-table :test #'equal)))
  (with-open-file (f infile :direction :input)
    (loop for line = (read-line f nil)
          while line
          do (cl-ppcre:register-groups-bind (name (#'parse-integer byte1 byte2))
                 ("\\$xrom\\{\"(.*)\"\\}\\s*=\\s*\"(\\d+)\\s+(\\d+)\";\\s*" line)
               (let* ((val (+ (* 256 (- byte1 160)) byte2))
                      (xrom-id (truncate val 64))
                      (function-id (mod val 64)))
                 (unless (or (null (gethash name ht))
                             (and (= xrom-id (first (gethash name ht)))
                                  (= function-id (second (gethash name ht)))))
                   (format *error-output* "~&Conflict: ~a~%" name)
                   (format *error-output* "Old: XROM ~d,~d~%" (first (gethash name ht)) (second (gethash name ht)))
                   (format *error-output* "New: XROM ~d,~d~%" xrom-id function-id))
                 (setf (gethash name ht) (list xrom-id function-id))))))
  ht)

(defun extract-xrom-from-buffer (buffer &optional (ht (make-hash-table :test #'equal)))
  (let ((rom-name))
    (flet ((get-10 (offset)
             (+ (ash (aref buffer (* offset 2)) 8)
                (aref buffer (+ 1 (* offset 2)))))
           (get-8 (offset)
             (aref buffer (+ 1 (* offset 2))))
           (get-16 (offset)
             (+ (ash (aref buffer (+ 1 (* offset 2))) 8)
                (aref buffer (+ 3 (* offset 2)))))
           (write-encoded-char (ccode stream)
             (cond ((<= ccode 32)
                    (write-char (code-char (+ ccode 64)) stream))
                   ((<= #.(char-code #\A) ccode #.(char-code #\E))
                    (write-char (code-char (+ ccode 32)) stream))
                   ((= ccode #.(char-code #\M)) ; not-equal
                    (write-string "!=" stream))
                   ((= ccode #.(char-code #\N)) ; sigma
                    (write-string "~" stream))
                   (t
                    (code-char ccode)))))
      (let ((rom-id (get-10 0))
            (num-elements (get-10 1)))
        (loop for i below num-elements
              do (let* ((entry-pt (get-16 (+ 2 (* i 2))))
                        (name (with-output-to-string (s)
                                (loop for addr from (- entry-pt 1) by -1
                                      for ccode = (get-8 addr)
                                      for ccode-masked = (logand ccode #x7f)
                                      do (progn
                                           (format t "~&ccode=~2,'0X (~2,'0X)~%" ccode-masked ccode)
                                           (write-encoded-char ccode-masked s))
                                      while (= ccode ccode-masked)))))
                   (terpri)
                   (format t "~&Found id ~s~%" name)
                   (if (zerop i)
                     (setf rom-name name)
                     (setf (gethash name ht) (list rom-id i)))))))
    (values rom-name ht)))

(defun extract-xrom (infile &optional (ht (make-hash-table :test #'equal)))
  (with-open-file (f infile :direction :input :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (list (file-length f)) :element-type '(unsigned-byte 8))))
      (read-sequence buffer f)
      (extract-xrom-from-buffer buffer ht))))

(defparameter *module-file-header*
  '((file-format 5)
    (title 50)
    (version 10)
    (part-number 20)
    (author 50)
    (copyright 100)
    (license 200)
    (comments 255)
    (category 1)
    (hardware 1)
    (mem-modules 1)
    (xmem-modules 1)
    (original 1)
    (app-auto-update 1)
    (num-pages 1)
    (header-custom 32)))

(defparameter *module-file-page*
  '((name 20)
    (id 9)
    (page 1)
    (page-group 1)
    (bank 1)
    (bank-group 1)
    (ram 1)
    (write-protect 1)
    (fat 1)
    (image 5120)
    (page-custom 32)))

(defun get-offset-and-length (descr item)
  (let ((offset 0))
    (loop for (sym length) in descr
          do (if (eq item sym)
               (return-from get-offset-and-length (values offset length))
               (incf offset length)))
    (values offset 0)))

(defun get-offset-and-length/cached (descr item cache)
  (multiple-value-bind (data found-p)
      (gethash item cache)
    (if found-p
      (values-list data)
      (multiple-value-bind (offset length)
          (get-offset-and-length descr item)
        (setf (gethash item cache) (list offset length))
        (values offset length)))))

(defmacro def-lookup/offset-length (name descr)
  (with-unique-names (g-descr g-cache g-item)
    `(let ((,g-descr ,descr)
           (,g-cache (make-hash-table :test #'eq)))
       (defun ,name (,g-item)
         (get-offset-and-length/cached ,g-descr ,g-item ,g-cache)))))

(def-lookup/offset-length get-module-header-offset-and-length *module-file-header*)

(def-lookup/offset-length get-module-page-offset-and-length *module-file-page*)

#||

(get-module-header-offset-and-length 'file-format)
(get-module-header-offset-and-length 'title)
(get-module-header-offset-and-length 'version)
(get-module-header-offset-and-length 'nil)
(get-module-page-offset-and-length 'nil)
||#

#||
(defun extract-xroms-from-module (infile &optional (ht (make-hash-table :test #'equal)))
  (with-open-file (f infile :direction :input :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (list (file-length f)) :element-type '(unsigned-byte 8))))
      (read-sequence buffer f)
      (labels ((get-string (octets)
                 (map 'string #'code-char (subseq octets 0 (position 0 octets))))
               (get-header-octet (item)
                 (aref buffer (get-module-header-offset-and-length item)))
               (get-header-octets (item)
                 (multiple-value-bind (offset length)
                     (get-module-header-offset-and-length item)
                   (subseq buffer offset (+ offset length))))
               (get-header-string (item)
                 (get-string (get-header-octets item)))
               (get-page-octet (item)
                 (aref buffer (get-module-page-offset-and-length item)))
               (get-page-octets (item)
                 (multiple-value-bind (offset length)
                     (get-module-page-offset-and-length item)
                   (subseq buffer offset (+ offset length))))
               (get-page-string (item)
                 (get-string (get-page-octets item))))
        (loop for item in '(title version author license copyright)
              do (format t "~&~a: ~a~%" item (get-header-string item)))
        (format t "~&Num pages: ~d~%" (get-header-octet 'num-pages))))))


(extract-xroms-from-module #p"/Users/raw/devel/hp41barcode-extras/SANDMATH-II.MOD")
(untrace)
(trace get-offset-and-length)
               
                         

      
      ||#   
    

#||
(extract-xrom #p"/Users/raw/Desktop/HP41Barcode/yfnz-1e.rom")
||#

(defun get-yfns (infile xrom-id &optional (ht (make-hash-table :test #'equal)))
  (with-open-file (f infile :direction :input)
    (loop for name = (read-line f nil)
          for function-id from 1
          while name
          do (setf (gethash name ht) (list xrom-id function-id))))
  ht)

(defun print-table (table-name table stream)
  (loop for first = t then nil
        for k being the hash-key of table
        using (hash-value v)
        do (progn
             (if first
               (format stream "~&~a = {~%" table-name)
               (format stream ",~%"))
             (if (listp v)
               (format stream "  '~a': [~{~d~^, ~}]" k v)
               (format stream "  '~a': ~d" k v)))
        finally (format stream "~%}; ~%")))

(defun main (basedir output-file)
  (with-open-file (out output-file :direction :output :if-exists :supersede)
    (multiple-value-bind (one-byte-builtins-table two-byte-builtins-table)
        (get-builtins (merge-pathnames #p"builtins" basedir))
      (print-table "var one_byte_builtins" one-byte-builtins-table out)
      (print-table "var two_byte_builtins" two-byte-builtins-table out)
      (format out "~&var xroms = {};~%");
      (loop for xrom in (directory (merge-pathnames "*.rom" basedir))
            for table-name = (pathname-name xrom)
            for table = (get-xrom xrom)
            do (print-table (format nil "xroms['~a']" table-name)
                            table out))
      (loop for xrom in (directory (merge-pathnames "bin/*.rom" basedir))
            do (multiple-value-bind (table-name table)
                   (extract-xrom xrom)
                 (print-table (format nil "xroms['~a']" #+nil table-name (pathname-name xrom))
                              table out)))
      (print-table "xroms['yfns']" (get-yfns (merge-pathnames #p"yfns.txt" basedir) 15) out)
      (print-table "xroms['yfnz']" (get-yfns (merge-pathnames #p"yfns.txt" basedir) 15) out))))

#||
(extract-xrom #p"/Users/raw/devel/hp41/rom/bin/41Z.rom")

(main #p"/Users/raw/devel/hp41/rom/" #p"/Users/raw/devel/hp41barcode/functions.js")
||#
