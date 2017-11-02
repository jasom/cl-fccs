(in-package :cl-fccs)
(declaim (optimize (speed 3) (safety 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cache-size+ (* 1024 1024)))

(defconstant +key-size+ 4)

(defvar *byte-cache* (make-array +cache-size+ :element-type '(unsigned-byte 64)))

(defvar *cache-position* +cache-size+)
(declaim (type (integer 0 #.+cache-size+) *cache-position*))
(declaim (type (simple-array (unsigned-byte 64) (#.+cache-size+)) *byte-cache*))

(defvar *cache-lock* (bt:make-lock))

(declaim (inline get-some-of-key)
	 (ftype (function () (unsigned-byte 64)) get-some-of-key))

(defun get-some-of-key ()
  "Must be called with lock held"
  (incf *cache-position*)
  (when (>= *cache-position* +cache-size+)
    (with-open-file (f "/dev/urandom" :element-type '(unsigned-byte 64))
      (read-sequence *byte-cache* f)
      (setf *cache-position* 0)))
  (aref *byte-cache* *cache-position*))

(declaim (type (simple-array simple-base-string (65536)) +print-table+))
(defparameter +print-table+
  #.(make-array 65536 :element-type 'simple-base-string
		:initial-contents
		(loop for i from 0 to 65535
		   collect (coerce (format nil "~4,'0X" i) 'simple-base-string))))

(declaim (inline fast-print-unsigned-byte-64))

(defun fast-print-unsigned-byte-64 (x string pos)
  (declare (type (unsigned-byte 64) x)
	   (type simple-base-string string)
	   (type fixnum pos))

  (setf
   (subseq string pos (incf pos 4))
   (aref +print-table+ (ldb (byte 16 48) x))
   (subseq string pos (incf pos 4))
   (aref +print-table+ (ldb (byte 16 32) x))
   (subseq string pos (incf pos 4))
   (aref +print-table+ (ldb (byte 16 16) x))
   (subseq string pos (incf pos 4))
   (aref +print-table+ (ldb (byte 16  0) x))))

  
(defun get-key ()
  (bt:with-lock-held (*cache-lock*)
    (let ((returnme (make-array #.(* +key-size+ 16) :element-type 'base-char)))
      (loop repeat +key-size+
	   for pos fixnum = 0 then (+ pos 16)
	   do (fast-print-unsigned-byte-64 (get-some-of-key) returnme pos))
      returnme)))

(defvar *closure-hash* (make-hash-table :test #'equal))

(declaim (optimize (speed 1) (debug 1) (safety 1)))

(defun reaper-task (timeout)
  (let ((now (get-universal-time)))
    (maphash (lambda (key value)
	       (destructuring-bind (&key timestamp &allow-other-keys) value
		 (when (> (- now timestamp) timeout)
		   (remhash key *closure-hash*))))
	     *closure-hash*)))

(defun make-action (fn &key (base-uri "/"))
  (let ((id (get-key)))
    (setf (gethash id *closure-hash*)
	  (list :timestamp (get-universal-time)
		:function fn))
    (format nil "~a?~A" base-uri id)))

(defun invoke-action (id env)
  (destructuring-bind (&key function &allow-other-keys)
      (gethash id *closure-hash* '(:function (lambda (env)
					       (declare (ignore env))
					       '(500
						 (:content-type "text/plain")
						 ("Internal Server Error")))))
    (funcall function env)))
