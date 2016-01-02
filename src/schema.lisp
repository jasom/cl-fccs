(in-package :cl-fccs)
  
(defun get-schema-version ()
  (let ((ver (red:get "schema-version")))
    (parse-integer
     (if ver
	 ver
	 (progn
	   (red:setnx "schema-version" *current-schema-verison*)
	   (red:get "schema-version"))))))

(defmethod upgrade-schema-once (from-version))

(defun %convert-list-to-vector (item)
  (loop for (key . value) in (alexandria:hash-table-alist item)
     when (and (listp value)
	       (not (ends-with-subseq "-ORIGIN" (string key))))
     do (setf (gethash key item) (coerce value 'vector))
     when (hash-table-p value)
     do (%convert-list-to-vector value)))

(defmethod upgrade-schema-once ((from-version (eql 0)))
  (let ((chars (get-all-character-ids)))
    (loop for item in chars
	 do (%convert-list-to-vector item))))

(defun upgrade-schema ()
  (with-db-connection
    (loop
       for version = (get-schema-version)
       while (< version *current-schema-verison*)
	 do (upgrade-schema-once version))))
