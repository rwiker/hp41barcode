(defun ignorable-builtin (name)
  (member name '("SPARE1" "SPARE2" "LBL2") :test #'string=))

(defun aliases-for (name)
  (or (when (cl-ppcre:scan ".+\\!\\=.+" name)
        (list (cl-ppcre:regex-replace "(.+)\\!\\=(.+)" name "\\1 NE \\2")))
      (when (cl-ppcre:scan "}" name)
        (list (cl-ppcre:regex-replace "\\}" name "->")))      
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
                   (format *error-output* "~&Conflict: ~a~%" name))
                 (setf (gethash name ht) (list xrom-id function-id))))))
  ht)

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
      (print-table "xroms['yfns']" (get-yfns (merge-pathnames #p"yfns.txt" basedir) 15) out)
      (print-table "xroms['yfnz']" (get-yfns (merge-pathnames #p"yfns.txt" basedir) 15) out))))

#||
(main #p"/Users/raw/devel/hp41/rom/" #p"/Users/raw/devel/hp41barcode/functions.js")
||#
