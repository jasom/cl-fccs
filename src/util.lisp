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
