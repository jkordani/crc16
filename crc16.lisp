;;;; crc16.lisp

(in-package #:crc16)

;;; "crc16" goes here. Hacks and glory await!

(defparameter width 16)

(defparameter poly #x1021)

(defparameter byte-bits 8)

(defun lshift (value places)
  (* value (expt 2 places)))

(defun rshift (value places)
  (floor value (expt 2 places)))

(defparameter table-size (lshift 1 byte-bits))

(defparameter msb-mask (lshift 1 (- width 1)))

(defun crc-init ()
  (let ((val nil))
    (loop
       for i from 0 to (- table-size 1)
       do (setf val (lshift i (- width byte-bits)))
       collect (loop
		  for j from 0 to (- byte-bits 1)
		  do (setf val  (logxor (lshift val 1) (if (/= 0 (logand val msb-mask))
							   poly
							   0)))
		  return val))))

(defparameter crc16-table (crc-init))


(defun crc16 (byte-vector &optional (initcrc 0))
  (let ((crc initcrc))
    (loop
       for i across byte-vector
       do (setf crc (ldb (byte width 0)
			 (logxor (lshift crc byte-bits)
				 (nth (rshift crc (- width byte-bits)) crc16-table)
				 i))))
    crc))

(defun crc-file (file)
  (coerce 
  (with-open-file (f file
		     :element-type '(unsigned-byte 8))
    (loop
       for i = (read-byte f nil)
       until (null i)
       collect i))
  'vector))
