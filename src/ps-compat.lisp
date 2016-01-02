(in-package :cl-fccs)

;#+ps(ps:lisp ps:*ps-lisp-library*)

#+ps(defun aget (key table &optional def)
      (chain table (get key def)))

#+ps(defun len (item)
      (@ item (count)))
#-ps(defun len (item)
      (length item))

#-ps(eval-when (:compile-toplevel :load-toplevel :execute)
      (defun optimize-setf (exp)
	(labels
	    ((let*-to-let-helper (vars body &optional (depth 1))
	       (when (= depth 100)
		 (break))
	       (if (null vars)
		   body
		   `((let (,(car vars))
		       ,@(let*-to-let-helper (cdr vars) body (1+ depth))))))
	     (let*-to-let (form)
	       (destructuring-bind
		     (let-star vars &body b) form
		 (assert (eql 'let* let-star))
		 (if (null vars)
		     `(let () ,@b)
		     `(let (,(car vars))
			,@(let*-to-let-helper (cdr vars) b))
		     ))))
	  #+(or)exp
	  #-(or)
	  (cond
	    ((not (listp exp))
	     exp)
	    ((eql (car exp) 'let*)
	     (optimize-setf (let*-to-let exp)))
	    ((not (eql (car exp) 'let))
	     exp)
	    (t
	     (destructuring-bind (letter vars &body b) exp
	       (declare (ignore letter))
	       (loop
		  with flat = (alexandria:flatten b)
		  with tree = b
		  for (var assignment) in vars
		  when (<= (count var flat) 1)
		  do (setf tree (subst assignment var tree))
		  else collect (list var assignment) into new-vars
		  finally (return
			    (if (eql (length tree) 1)
				`(let ,new-vars ,(optimize-setf (car tree)))
				`(let ,new-vars ,@tree))))))))))



#+ps(defun mapcar (fun list)
      (unless (chain *immutable *list (is-list list))
	(setf list (ps:new (chain *immutable (*list list)))))
      (chain list (map fun)))
#+ps(defun mapcar* (fun list) (mapcar fun list))
#-ps(defun mapcar* (&rest args) (apply #'map 'vector args))

#+ps(defun mapcan* (fun list) (mapcan fun list))
#-ps(defun mapcan* (&rest args)
      (apply #'concatenate 'vector (apply #'map 'vector args)))

#-ps(ps:defpsmacro list (&rest args)
      `(ps:new (chain *immutable (*list (ps:array ,@args)))))
  
#-ps(defmacro aget (key table &optional def)
      `(gethash ,key ,table ,def))

(defun amake ()
  #+ps(ps:new (chain *immutable (*map)))
  #-ps(make-hash-table :test #'equalp))

(defun akeys (obj)
  #+ps(chain obj (key-seq))
  #-ps(loop for k being the hash-keys of obj
	   collect k))

(defun ain (key table)
  #+ps(chain table (has key))
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
	      collect (or (getf (cdr item) :deep-depend) nil) into deep-depends
	      collect (or (getf (cdr item) :validator) '(lambda (&key &allow-other-keys) t)) into validators
	      collect (or (getf (cdr item) :fixup) '(lambda (&key value &allow-other-keys) value)) into fixups
	      finally (return (values names initforms deep-depends validators fixups)))))
    (multiple-value-bind
	  (slot-names slot-initforms slot-depends slot-validators slot-fixups)
	(parse-slots slots)
      (let ((pname (intern (format nil "~a-P" (symbol-name name)) *package*))
	    (cname (intern (format nil "MAKE-~a" (symbol-name name)) *package*))
	    (fname (intern (format nil "FIXUP-~a" (symbol-name name)) *package*))
	    (vname (intern (format nil "VALIDATE-CHANGED-~A" (symbol-name name)) *package*)))
	`(progn
	   (defun ,pname
	       (obj)
	     (unless (classishp obj) (return-from ,pname nil))
	     ,@(loop for item in slot-names
		  for validator in slot-validators
		  do `(unless (and
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
	       r))
	   (defun ,vname (obj slot-name)
	     (unless (classishp obj) (return-from ,vname nil))
	     (cond
	       ,@(loop for slot in slot-names
		    for depend in slot-depends
		    for validator in slot-validators
		    if depend
		    collect `((eql slot-name ,slot)
			      (,slot (,pname obj)))
		    else
		    collect `((eql slot-name ,slot)
			      (and
			       (ain ,slot obj)
			       (funcall ,validator
					:value (aget ,slot obj)
					:obj obj)))))))))))

(defun pappend (list &rest lists)
  #-ps (apply #'concatenate 'vector list lists)
  #+ps (apply (chain list concat) lists))

(defun unloopable (list)
  #-ps (coerce list 'vector)
  #+ps (ps:new (chain *immutable (*list list))))

#+ps(defun array* (&rest items)
      (unloopable items))
#-ps(defun array* (&rest contents) (make-array (length contents) :initial-contents contents :adjustable t :fill-pointer t))


(defun loopable (list)
  #-ps (coerce list 'list)
  #+ps (if list
	   (chain list (to-array))
	   nil))

#+ps(defun nth (n list)
      (chain list (get n)))
      
#+ps(defun mapcan (fn list)
      (chain (mapcar fn list)
	     (reduce (lambda (sofar val)
		       (chain sofar (concat val))))))

#+ps(defun stringp (value)
      (= (typeof value) "string"))

#+ps(defun member (value list &key (key (lambda (x) x)) test)
      (if test
	  (chain list (find (lambda (k v)
			      (funcall test
				       (funcall key k)
				       value))))
	  (chain (mapcar key list) (contains value))))

#+ps(defun every (predicate list)
      (chain list (every predicate)))

#+ps(defun listp (item)
      (chain *immutable *list (is-list item)))

#+ps(defun emptyp (item)
      (and (listp item)
	   (chain item (is-empty))))

(defun range (start end &optional (step 1))
  #-ps(loop for i from start below end by step)
  #+ps(ps:new (chain *immutable (*range start end step))))

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


#-ps(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ps-setf-expanders* (make-hash-table)))

(defmacro ps-define-setf-expander (access-fn lambda-list &body b)
  (let ((wv (or (lambda-fiddle:whole-lambda-var lambda-list) (gensym)))
	    (ev (or (lambda-fiddle:environment-lambda-var lambda-list) (gensym)))
	    (ll
	     (lambda-fiddle:remove-whole-part 
	      (lambda-fiddle:remove-environment-part lambda-list))))
	(print ll)
	(setf (gethash access-fn *ps-setf-expanders*)
	      (eval
		 `(lambda (,wv ,ev)
		   (destructuring-bind ,ll (cdr ,wv)
		     ,@b)))))
  '(progn))
	  
#-ps(defun ps-get-setf-expansion (place &optional environment)
      (cond
	((and (listp place) (gethash (car place) *ps-setf-expanders*))
	 (funcall (gethash (car place) *ps-setf-expanders*) place environment))
	(t
	 (let ((new (gensym)))
	   (values nil nil (list new) `(ps::ps-assign ,place ,new) place)))))

(ps-define-setf-expander aget (key table &optional def &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (ps-get-setf-expansion table env)
    (declare (ignorable setter newval))
    (let ((store (gensym))
	  (key-dummy (gensym))
	  (def-dummy (gensym)))
      (values
       `(,key-dummy ,def-dummy ,@dummies)
       `(,key ,def ,@vals)
       `(,store)
       `(progn
	  (setf ,table
		(chain ,getter (set ,key-dummy ,store)))
	  ,store)
       `(aget ,key-dummy ,getter ,def-dummy)))))

#-ps(ps:defmacro+ps ps-incf (place &optional (n 1))
  (multiple-value-bind (temp-vars value-forms store-vars store-form access-form)
      (ps-get-setf-expansion place)
    (let ((nsym (gensym)))
      (optimize-setf
       `(let* ((,nsym ,n)
	       ,@(loop for temp in temp-vars
		    for value in value-forms
		    collect (list temp value)))
	  (let ((,(car store-vars) (+ ,access-form ,nsym)))
	    ,store-form))))))

#-ps(ps:defmacro+ps ps-setf (place value &rest args)
      (multiple-value-bind (temp-vars value-forms store-vars store-form access-form)
	  (ps-get-setf-expansion place)
	(declare (ignorable access-form))
	`(progn
	   ,(optimize-setf
	     `(let* (,@(loop for temp in temp-vars
			  for value in value-forms
			  collect (list temp value)))
		;; parenscript and multiple-values is buggy
		#+(or)(multiple-value-bind ,store-vars ,(multiple-value-list value)
			,store-form)
		#-(or)(let ((,(car store-vars) ,value))
			,store-form)))
	   ,@(when args
		   `((ps-setf ,@args))))))

#-ps(ps:defpsmacro setf (&rest args)
      `(ps-setf ,@args))
      
#-ps(ps:defpsmacro incf (&rest args)
      `(ps-incf ,@args))
