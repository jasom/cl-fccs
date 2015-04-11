(in-package #:cl-fccs)

(defparameter +skills+ (mapcar
			(lambda (x)
			  (alexandria:make-keyword
			   (string-upcase (first x)))) fccg::+skill-table+))

(defparameter +class-names+ (mapcar (lambda (x) (car x)) fccg::+class+))
(defparameter +species-names+ (mapcar (lambda (x) (car x)) fccg::+species+))
(defparameter +talent-names+ (mapcar (lambda (x) (car x)) fccg::+talent+))
(defparameter +specialty-names+ (mapcar (lambda (x) (car x)) fccg::+specialty+))
(defun recursive-alist-hash-table (x &key (test #'equalp) (depth -1)
				       (munge-value #'identity))
  (let ((result (make-hash-table :test test)))
    (loop for ((key . value)) on x
	 do (setf (gethash key result)
		  (if (and
		       (not (zerop depth))
		       (consp value)
		       (consp (car value))
		       (symbolp (caar value)))
		      (recursive-alist-hash-table value :test test :depth (1- depth)
						  :munge-value munge-value)
		      (funcall munge-value value))))
    result))
       
(defun undot-list (x)
  (let ((fixed (if (and (consp x)
		       (not (null (cdr (last x)))))
		  (append (butlast x)
			  (list (car (last x))
				(cdr (last x))))
		  x)))
    (if (consp fixed)
	(mapcar #'undot-list fixed)
	fixed)))

(defparameter +species-hash+
  (recursive-alist-hash-table fccg::+species+
			      :depth 1
			      :munge-value #'undot-list))

(defparameter +specialty-hash+
  (recursive-alist-hash-table fccg::+specialty+))


(defparameter +skills-hash+
  (alexandria:alist-hash-table
   (mapcar (lambda (x) (cons (make-keyword (string-upcase (car x)))
			     (cdr x))) fccg::+skill-table+) :test #'equalp))

(defparameter +talent-hash+
  (recursive-alist-hash-table fccg::+talent+
			      :depth 1
			      :munge-value #'undot-list))
  ;(alexandria:alist-hash-table fccg::+talent+ :test #'equalp))

(defparameter +class-info-hash+
  (recursive-alist-hash-table
   (mapcar (lambda (x)
	     (cons (make-keyword (string-upcase (car x)))
		   (cdr x)))
	   fccg::+class+)))

(defparameter +class-hash+
  (alexandria:alist-hash-table
   (mapcar (lambda (x)
	     (cons (car x)
		   (let ((keys (mapcar (lambda (y) (make-keyword (string-upcase y))) (cadr x))))
		     (print keys)
		     (loop for item in (cddr x)
			for ht = (make-hash-table)
			do (mapcar (lambda (k v)
				     (setf (gethash k ht) v))
				   (cdr keys) (cdr item))
			collect ht))))
	   fccg::+class-tables+)))
		    
(defparameter +armor-names+ (mapcar #'car (last (butlast (cdr (assoc :armor fccg::+gear+)) 2) 8)))
(defparameter +armor-upgrades+ (mapcar #'car (cddr (assoc :armor-upgrades fccg::+gear+))))
(defparameter +armor-craftsmenship+ nil)
(defparameter +armor-construction+ nil)
(defparameter +armor-customization+ nil)


(loop
     with var = '+armor-craftsmenship+
   for item in +armor-upgrades+
   when (string= item "Crude")
   do (setf var '+armor-construction+)
   when (string= item "Beast")
     do (setf var '+armor-customization+)
   do (push item (symbol-value var)))

(defparameter +all-gear-names+
  (loop for item in fccg::+gear+
       when (not (member (car item) '(:armor-upgrades :weapon-upgrades :services)))
       nconc
       (loop
	    with state = :good
	  for item in (cddr item)
	  when (cl-ppcre:scan "([Uu]pgrade)|(Material)|Craftsmanship|Customization"
			      (format nil "~{~A~}" item))
	  do (setf state :bad)
	  when (and (eql state :good)
		    (not (every #'string= (last item 3) '("" "" ""))))
	  collect (car item))))

(defparameter +proficiencies+ '(:unarmed :blunt :edged :hurled :bows :black-powder :siege-weapons))

(defparameter +feat-names+ (mapcar (lambda (x)
				       (alexandria:make-keyword
					(substitute #\- #\space (car x))))
				     fccg::+feats+))

(defparameter +feat-hash+ (alexandria:alist-hash-table
			   (mapcar (lambda (x y)
				     (cons x (cdr y)))
				   +feat-names+ fccg::+feats+)))

(defparameter +abilites-hash+
  (alexandria:alist-hash-table
   (append
    fccg::+class-abilities+
    fccg::+ability-info+)
   :test #'equalp))

(defun comma-split (str)
  (let* ((comma (position #\, str)))
    (if comma
	(list (subseq str 0 comma)
	      (subseq str (1+ comma)))
	(list str))))

(defun better-capitalize (str)
  (with-output-to-string (s)
    (let ((words (split-sequence-if
		  (rcurry #'member '(#\Space #\-))
				 (string-downcase str)
				 :remove-empty-subseqs t))
	  (separators (split-sequence-if-not
		  (rcurry #'member '(#\Space #\-))
		  (string-downcase str)
		  :remove-empty-subseqs t)))
      (loop
	 for first = t then nil
	 for item in words
	 for separator in (cons #\Space separators)
	   unless first do (write-string separator s)
	   if (member item '("of") :test #'equalp)
	   do (write-string item s)
	   else if (member item '("i" "ii" "iii" "iv" "v" "xp") :test #'equalp)
	   do (write-string (string-upcase item) s)
	   else do
	   (write-char (char-upcase (elt item 0)) s)
	   (write-string (subseq item 1) s)))))


(defun fixup-ability (ability)
  (loop for item in (comma-split ability)
     collect
       (better-capitalize
	(string-trim " " (cl-ppcre:regex-replace "( \\d*/.*)|( \\+.*)" item "")))))

(loop for levels being the hash-values of +class-hash+
   do (loop for level in levels
	 for key = (if (gethash :special level)
		       :special
		       :abilities)
	 do (setf (gethash key level)
		  (fixup-ability (gethash key level)))))
