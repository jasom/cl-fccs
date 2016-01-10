(in-package :cl-fccs)

(defapprule autocomplete (property :path-info (ppcre "^/complete/(.*)" thing))
  (let ((partial (parse-json-body body)))
    `(200
      (:content-type "application/json")
      (,(with-output-to-string
	 (s)
	 (encode-classish
	  (cond
	    ((equalp thing "spell-info")
	     (assoc partial fccg::+spells+ :test #'equalp))
	    ((equalp thing "gear")
	     (complete-from-list partial +all-gear-names+))
	    ((equalp thing "gear-info")
	     (lookup-gear partial))
	    ((equalp thing "spell")
	     (complete-from-list partial (mapcar
					  (compose #'better-capitalize #'car)
					  fccg::+spells+)))
	    ((equalp thing "species")
	     (complete-from-list partial
				 (hash-table-keys +species-hash+)))
	    ((equalp thing "specialty")
	     (complete-from-list partial
				 (hash-table-keys +specialty-hash+)))
	    ((equalp thing "class")
	     (complete-from-list
	      partial
	      (hash-table-keys +class-hash+)))
	    ((equalp thing "feat")
	     (complete-from-list
	      partial
	      (mapcar #'better-capitalize
		      (hash-table-keys +feat-hash+))))
	    ((equalp thing "talent")
	     (complete-from-list partial
				 (hash-table-keys +talent-hash+)))
	    (t nil))
	  s))))))

(defun complete-from-list (partial list)
  (when (>= (length partial) 1)
    (loop for item in list
       when (alexandria:starts-with-subseq (string-downcase partial) (string-downcase item))
       collect item)))

(defun lookup-gear (name)
  (loop for item in fccg::+gear+
     when (member name (mapcar #'car (cddr item)) :test #'equalp)
     do
       (let ((info (find name (cddr item) :key #'car :test #'equalp))
	     (effect-idx (position "Effect" (cadr item) :test #'equal))
	     (size-idx (position "SZ/Hand" (cadr item) :test #'equal))
	     (weight-idx (position "Weight" (cadr item) :test #'equal)))
	 (return
	   `(,name ,(if effect-idx
			(elt info effect-idx)
			"")
		   ,@(if size-idx
			 (destructuring-bind (size hand)
			     (split-sequence #\/ (elt info size-idx))
			   (list
			    (if (> (length size) 1)
				"m"
				(string-downcase size))
			    hand))
			 '("" ""))
		   ,(if weight-idx
			(let ((weight-str (elt info weight-idx)))
			  (parse-number (subseq weight-str 0
						(position #\Space weight-str))))
			""))))))
