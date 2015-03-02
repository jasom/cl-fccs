(in-package :cl-fccs)

#+ps(ps:lisp ps:*ps-lisp-library*)

(defmacro aget (key table &optional default)
  #+ps
  (if default
    `(if (ps:in ,key ,table)
	 (ps:getprop ,table ,key)
	 ,default)
    `(ps:getprop ,table ,key))
  #-ps`(cl:gethash ,key ,table ,default))

(defun amake ()
  #+ps(ps:create)
  #-ps(make-hash-table :test #'equalp))

(defun akeys (obj)
  #+ps(chain *object (keys obj))
  #-ps(loop for k being the hash-keys of obj
	   collect k))

(defun ain (key table)
  #+ps(ps:in key table)
  #-ps(let ((v '#:dummy))
	(not (eql (gethash key table v) v))))

(defmacro defp (&rest args)
  #+ps`(ps:var ,@args)
  #-ps`(defparameter ,@args))

#+ps(defun string-capitalize (str)
      (+
       (chain
	str (char-at 0) (to-upper-case))
      (chain str (slice 1) (to-lower-case))))

#+ps(defun better-capitalize (str)
      (let ((words
	     (loop for item in (chain str (split (ps:new (*reg-exp "[- ]"))))
		  when (/= "" item) collect item))
	    (splitters (loop for item in (chain str (split (ps:new (*reg-exp "[^- ]"))))
			    when (/= "" item) collect item))
	    (result ""))

	(loop for word in words
	     for first = t then nil
	     unless first
	     do (setf result (+ result (chain splitters (shift))))
	     do (setf result (+ result (string-capitalize word))))
	result))
	     

(defun gpos (list value)
  (loop for item in list
       for pos from 0
       do (when #+ps(= item value)
		#-ps(equalp item value)
		(return-from gpos pos))))

(defun wstrim (x)
  #-ps(string-trim " " x)
  #+ps(chain x (trim)))

(defun to-keyword (x)
  #+ps(chain x (to-lower-case))
  #-ps(make-keyword (string-upcase x)))

(defmacro import-list (name)
  `(defp ,name
       (ps:lisp 
	`(list ,@,name))))

#+ps(import-list +proficiencies+)

#+ps(defun keywordp (x)
      (stringp x))

#+ps (defun string (x)
       x)

#+ps(defun make-keyword (x)
      (to-keyword x))
		     
#+ps(defp +species-hash+
	(ps:lisp (ht-to-obj +species-hash+)))

#+ps(defp +talent-hash+
	(ps:lisp (ht-to-obj +talent-hash+)))

#+ps(defp +skills-hash+
	(ps:lisp (ht-to-obj +skills-hash+)))

#+ps(defp +class-hash+
	(ps:lisp (ht-to-obj +class-hash+)))

#+ps(defp +class-info-hash+
	(ps:lisp (ht-to-obj +class-info-hash+)))

#+ps(defp +specialty-hash+
	(ps:lisp (ht-to-obj +specialty-hash+)))


#+ps(defun parse-integer (str)
      (let ((x (parse-int str)))
     (and (integerp x) x)))

(defun ssplit (chr str)
  #+ps(chain str (split chr))
  #-ps(split-sequence chr str))

(defun ssub (replace regex str)
  #+ps(chain str (replace (ps:new (*reg-exp regex "g")) replace))
  #-ps(cl-ppcre:regex-replace-all regex str replace))

(defun re-match (regex str)
  #+ps(chain str (match (ps:new (*reg-exp regex))))
  #-ps(cl-ppcre:scan regex str))

#+ps(defun integerp (obj)
      (and (= (typeof obj) :number)
	   (= (ps:rem obj 1) 0)))

(defmacro with-classish-slots (slots object &body b)
  #+ps`(with-slots ,slots ,object ,@b)
  #-ps(once-only (object)
	 `(symbol-macrolet
	   ,(loop for item in slots
		 collect `(,item (gethash ,(make-keyword (string item))
					  ,object)))
	 ,@b)))

#-ps(defpackage cl-fccs/throwaway)

(defun classishp (obj)
  #+ps(=
       (chain *object prototype to-string (call obj))
       "[object Object]")
  #-ps(hash-table-p obj))

(defmacro defclassish (name &rest slots)
  "Makes something sort of class like"
  (flet ((throwaway-name (x)
    (intern (symbol-name x) :cl-fccs/throwaway))
	 (parse-slots (slots)
	   (loop for item in slots
	      collect (make-keyword (string (car item))) into names
	      collect (or (getf (cdr item) :initform) nil) into initforms
	      collect (or (getf (cdr item) :validator) '(lambda (&key &allow-other-keys) t)) into validators
	      collect (or (getf (cdr item) :fixup) '(lambda (&key value &allow-other-keys) value)) into fixups
	      finally (return (values names initforms validators fixups)))))
  (multiple-value-bind
	(slot-names slot-initforms slot-validators slot-fixups)
      (parse-slots slots)
    (let ((pname (intern (format nil "~a-P" (symbol-name name)) *package*))
	  (cname (intern (format nil "MAKE-~a" (symbol-name name)) *package*))
	  (fname (intern (format nil "FIXUP-~a" (symbol-name name)) *package*)))
      `(progn
	 (defun ,pname
	     (obj)
	   (unless (classishp obj) (return-from ,pname nil))
	   ,@(loop for item in slot-names
		for validator in slot-validators
		collect `(unless (and
				  (ain ,item obj)
				  (funcall ,validator
					   :value (aget ,item obj)
					   :obj obj))
			   #-ps(log:info "~A: ~S" ,item (aget ,item obj :not-present))
			   (return-from ,pname nil)))
	   t)
	 (defun ,fname
	     (obj)
	   (unless (classishp obj) (return-from ,fname nil))
	   ,@(loop for item in slot-names
		for fixup in slot-fixups
		for initform in slot-initforms
		collect `(if (ain ,item obj)
			     (setf (aget ,item obj)
				   (funcall ,fixup
					    :value (aget ,item obj)
					    :obj obj))
			     (setf (aget ,item obj) ,initform)))
	   (when (,pname obj) obj))
	 (defun ,cname (&key ,@(loop for name in slot-names
				 for initform in slot-initforms
				 collect `((,name ,(throwaway-name name)) ,initform)))
	   (let ((r (amake)))
	     (setf
	      ,@(loop for name in slot-names
		   collect `(aget ,name r)
		   collect (throwaway-name name)))
	     r)))))))


#+ps(defun stringp (value)
      (= (typeof value) "string"))

#+ps(defun member (value list &key (key (lambda (x) x)))
      (loop for item in list
	 do (when (= value (funcall #'key item))
	      (return-from member t)))
      nil)


#+ps(defun every (predicate list)
      (loop for item in list
	   do (unless (funcall predicate item) (return-from every nil)))
      t)

#+ps(defun listp (item)
      (and (not (= (typeof item) "undefined"))
	   (= (chain item constructor) *Array)))

#+ps(defun emptyp (item)
      (and (listp item)
	   (= (chain item length) 0)))

#+ps(defmacro tlet ((&rest bindings) &body b)
      `(funcall
	(chain
	 (lambda ()
	   (let ,bindings
	     ,@b))
	 (bind this))))

(defmacro tlambda ((&rest args) &body b)
  `(chain
    (lambda ,args ,@b)
    (bind this)))
