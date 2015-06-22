(in-package :cl-fccs)

(defun read-bytes (f count)
  (let ((buffer (make-array count :element-type '(unsigned-byte 8))))
    (loop for bytes-read = (read-sequence buffer f)
	 then (read-sequence buffer f :start bytes-read)
       while (< bytes-read count))
    buffer))

(defun random-bytes (count)
  (with-open-file (f "/dev/urandom" :element-type '(unsigned-byte 8))
    (read-bytes f count)))

(defun random-base64 (nbytes)
  (crypto-shortcuts:to-base64 (random-bytes nbytes)))

(defun slurp-body (env &optional max-length)
  (let ((content-length (getf env :content-length))
	(body (getf env :raw-body)))
    (when (or (null max-length)
	      (<= content-length max-length))
      (and body
	   (read-bytes body content-length)))))
