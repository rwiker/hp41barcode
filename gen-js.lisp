(defun get-builtins (infile &optional
                            (one-byte-ht (make-hash-table :test #'equal))
                            (two-byte-ht (make-hash-table :test #'equal)))
  (with-open-file (f infile  :direction :input)
    (loop for line = (read-line f nil)
          while line
          do (cl-ppcre:register-groups-bind (one-byte two-byte name (#'parse-integer value))
                 ("\\$(?:(single)|(twobyte))\\{\"(.*)\"\\}\\s*=\\s*\"(.*)\";\\s*" line)
               (setf (gethash name (if one-byte one-byte-ht two-byte-ht)) value))))
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

(defun print-table (table-name table stream)
  (loop for first = t then nil
        for k being the hash-key of table
        using (hash-value v)
        do (progn
             (if first
               (format stream "~&var ~a = {~%" table-name)
               (format stream ",~%"))
             (if (listp v)
               (format stream "  '~a': [~{~d~^, ~}]" k v)
               (format stream "  '~a': ~d" k v)))
        finally (format stream "~%}; ~%")))

(defun main (basedir output-file)
  (let ((one-byte-builtins-table (make-hash-table :test #'equal))
        (two-byte-builtins-table (make-hash-table :test #'equal))
        (xroms-table (make-hash-table :test #'equal)))
    (get-builtins (merge-pathnames #p"builtins" basedir) one-byte-builtins-table two-byte-builtins-table)
    (loop for xrom in (directory (merge-pathnames "*.rom" basedir))
          do (get-xrom xrom xroms-table))
    (with-open-file (out output-file :direction :output :if-exists :supersede)
      (print-table "one_byte_builtins" one-byte-builtins-table out)
      (print-table "two_byte_builtins" two-byte-builtins-table out)
      (print-table "xroms" xroms-table out))))

#||
(main #p"/Users/raw/devel/hp41/rom/" #p"/Users/raw/devel/hp41barcode/functions.js")
||#
