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

(defun parse-json-body (body)
  (let*
      ((obj-state)
       (json:*beginning-of-object-handler*
	(lambda ()
	  (push (make-hash-table) obj-state)))
       (json:*end-of-object-handler*
	(lambda ()
	  (pop obj-state)))
       (json:*object-key-handler*
	(lambda (key)
	  (push (to-keyword key) obj-state)))
       (json:*object-value-handler*
	(lambda (val)
	  (let ((key (pop obj-state)))
	    (setf (gethash key (car obj-state)) val))))
       (json:*json-array-type* 'vector))
    (json:decode-json-from-string (babel:octets-to-string body :encoding :utf-8))))

(defun encode-classish (obj stream)
  (let ((json:*lisp-identifier-name-to-json*
	 (lambda (id)
	   (string-downcase (string id)))))
    (json:encode-json obj stream)))
