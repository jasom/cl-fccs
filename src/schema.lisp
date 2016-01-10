(in-package :cl-fccs)

(defvar *current-schema-verison* 1)
 
(defun get-schema-version ()
  (let ((ver (red:get "schema-version")))
    (parse-integer
     (if ver
	 ver
	 (progn
	   (red:setnx "schema-version" *current-schema-verison*)
	   (red:get "schema-version"))))))


(defun %convert-list-to-vector (item)
  (loop for (key . value) in (alexandria:hash-table-alist item)
     when (and (listp value)
	       (not (ends-with-subseq "-ORIGIN" (string key))))
     do (setf (gethash key item) (coerce value 'vector))
     when (hash-table-p value)
     do (%convert-list-to-vector value)))

(defgeneric upgrade-schema-once (from-version))

(defmethod upgrade-schema-once ((from-version (eql 0)))
  (let ((chars (get-all-character-ids)))
    (loop for id in chars
	 for char = (get-character id)
	 do (print id)
	 (%convert-list-to-vector char)
	 (save-character id char)))
    (red:set "schema-version" 1))

(defun upgrade-schema ()
  (with-db-connection (t)
    (loop for ver =  (get-schema-version)
       while (< ver *current-schema-verison*)
       do (upgrade-schema-once ver))))
