(in-package :cl-fccs)

(defmacro ^ (&rest r) `(chain ,@r))

(defmacro my-make-state (args &body b)
  `(lambda (,@args)
     (let ((props
	    (if (ps:in :attrs ,(car args))
		(^ ,(car args) attrs)
		(ps:create)))
	   (children (^ ,(car args) children)))
       (setf (^ ,(car args) state)
	     (ps:new
	       (lambda ()
		 ,@b
		 this))))))

(defmacro mlambda ((arg) &body b)
  (let ((memo (gensym)))
    `(tlambda (,arg)
       (unless (ps:in ',memo this)
	 (setf (ps:@ this ,memo) (create)))
       (if (ps:in ,arg (ps:@ this ,memo))
	   (ps:getprop (ps:@ this ,memo) ,arg)
	   (let ((result (funcall (tlambda (,arg) ,@b) ,arg)))
	     (setf (ps:getprop (ps:@ this ,memo) ,arg) result)
	     result)))))

(defmacro checkbox-field (name  &key
				  (class-name "pure-u-1 pure-u-md-1-6")
				  (id (string (gensym)))
				  (input-class "pure-checkbox")
				  (show-label t))
  (let ((name (alexandria:make-keyword (string name))))
    `(tlet ((id ,id))
	     (htm
	      (:div
	       :class-name ,class-name
	       :style (create margin 0)
	       (:*validating-checkbox :default-value (aget ,name (chain c (obj)))
				   :id id
				   :input-class ,input-class
				   :error-style (create background-color "pink"
							  padding 0)
				   :style (create background-color "white"
						    padding 0)
				   :validator (chain c (validate ',name))
				   :onchange (chain c (handle-change ',name)))
	       ,@(when show-label
		       `((:label :html-for id
				 ,(better-capitalize (string name)))))
	       )))))

(defmacro autocomplete-input-field (name  &key
					    (class-name "pure-u-1 pure-u-md-1-6")
					    (input-class "pure-input-1")
					    input-type
					    update-completions
					    (id (string (Gensym)))
					    (label-as nil)
					    (show-label t)
					    (formatter '(lambda (x) x))
					    (parser '(lambda (x) x)))
  (let ((name (alexandria:make-keyword (string name))))
    `(tlet ((id ,id))
	     (htm
	      (:div
	       :class-name ,class-name
	       :style (create margin 0)
	       (:*validating-autocomplete
		:default-value (funcall ,formatter (aget ,name (chain c (obj))))
		:parser ,parser
		:id id
		:input-class ,input-class
		:type ,input-type
		:error-style (create background-color "pink"
				       padding 0)
		:style (create background-color "white"
				 padding 0)
		:update-completions ,update-completions
		:validator (chain c (validate ',name))
		:onchange (chain c (handle-change ',name)))
	       ,@(when show-label
		       `((:label :html-for id
				 ,(if label-as
				      label-as
				      (better-capitalize (string name))))))
	       )))))

(defmacro textarea-field (name &key
				 (class-name "pure-u-1 pure-u-md-1-6")
				 (formatter '(lambda (x) x))
				 (rows))
  (let ((name (alexandria:make-keyword (string name))))
    `(tlet ((id (genid)))
	     (htm
	      (:textarea
	       :id id
	       :class-name ,class-name
	       :rows ,rows
	       :style (create background-color "white" padding 0)
	       :onchange (let ((fn (chain c (handle-change ',name))))
                           (lambda (ev)
                             (funcall fn (chain ev target value))))
               (funcall ,formatter (aget ,name (chain c (obj)))))))))

(defmacro input-field (name  &key
			       (class-name "pure-u-1 pure-u-md-1-6")
			       (input-class "pure-input-1")
			       input-type
			       (label-as nil)
			       (show-label t)
			       (id (string (gensym)))
			       (formatter '(lambda (x) x))
			       (parser '(lambda (x) x))
			       override-value)
  (let ((name (alexandria:make-keyword (string name))))
  `(tlet ((id ,id))
       (htm
	(:div
	 :class-name ,class-name
	 :style (create margin 0)
	 (:*validating-input :default-value
                             (funcall ,formatter (aget ,name (chain c (obj))))
			     :parser ,parser
			     :id id
			     :input-class ,input-class
			     :type ,input-type
			     :error-style (create background-color "pink"
						    padding 0)
			     :style (create background-color "white"
					      padding 0)
			     :validator (chain c (validate ',name))
			     :onchange (chain c (handle-change ',name))
			     ,@(when override-value
				     `(:override-value ,override-value)))

	 ,@(when show-label
		 `((:label :html-for id
			   ,(if label-as
				label-as
				(better-capitalize (string name))))))
	 )))))

(defmacro skill-table (skills)
    `(htm
      (:div
       :class-name "pure-u-1 pure-u-md-1-2 pure-g"
       :style (create font-size "75%")
       :text-align "center"
       (:div :class-name "pure-u-1-12"
	     "OS")
       (:div :class-name "pure-u-1-6"
	     :style (create font-size "75%")
	     "Skill Name")
       (:div :class-name "pure-u-1-8"
	     :style (create font-size "75%")
	     "Attr.")
       (:div :class-name "pure-u-1-8"
	     :style (create font-size "75%")
	     "Skill Bonus")
       (:div :class-name "pure-u-1-8"
	     :style (create font-size "75%")
	     "Ranks")
       (:div :class-name "pure-u-1-8"
	     :style (create font-size "75%")
	     "Attr Mod.")
       (:div :class-name "pure-u-1-8"
	     :style (create font-size "75%")
	     "Misc Mod.")
       (:div :class-name "pure-u-1-8"
	     :style (create font-size "75%")
	     "Threat")
       ,@(loop for item in skills
	    for fitem = (ssub "_" " " (string item))
	    collect
	      `(htm
		(:div
		 (:div :class-name "pure-u-1-12"
		       (checkbox-field ,(make-keyword (format nil "~A-ORIGIN" fitem))
				       :class-name nil
				       :show-label nil))
		 (:div :class-name "pure-u-1-6"
		       (esc ,(string-capitalize
			      (let ((it (string item)))
				(if (> (length it) 11)
				    (subseq it 0 11)
				    it)))))
		 (:div :class-name "pure-u-1-8"
		       (esc ,(subseq (string (get-skill-attr item)) 0 3)))
		 (:div :class-name "pure-u-1-8"
		       (output-field ,(make-keyword (format nil "~A-BONUS" fitem))
				     :show-label nil
				     :class-name nil
				     :input-class "pure-input-1"))
		 (:div :class-name "pure-u-1-8"
		       (input-field ,(make-keyword (format nil "~a-RANKS" fitem))
				    :show-label nil
				    :class-name nil
				    :input-class "pure-input-1"
				    :parser parse-int))
		 (:div :class-name "pure-u-1-8"
		       (output-field ,(make-keyword (format nil "~a-ATTR-BONUS" fitem))
				     :show-label nil
				     :class-name nil
				     :input-class "pure-input-1"))
		 (:div :class-name "pure-u-1-8"
		       (output-field ,(make-keyword (format nil "~a-MISC-BONUS" fitem))
				     :show-label nil
				     :class-name nil
				     :input-class "pure-input-1"))
		 (:div :class-name "pure-u-1-8"
		       (input-field ,(make-keyword (format nil "~a-THREAT" fitem))
				    :show-label nil
				    :class-name nil
				    :input-class "pure-input-1"
				    :parser parse-int))))))))

(defmacro weapon-table (n)
  `(htm
    (:div :class-name "pure-u-1"
	  ,@(loop for i from 1 to n
	       collect
		 `(:*weapon-info
		   :default-value (aget ,(key-fmt :weapon-~d i) (chain c (obj)) nil)
					;TODO validator
		   :onchange (chain c (handle-change ,(key-fmt :weapon-~d i)))
		   :atk-bonus (output-field ,(key-fmt :weapon-~d-atk-bonus i)
					      :label-as "Atk"
					      :class-name "pure-u-1-6 pure-u-md-1-12")
		   :dmg-bonus (output-field ,(key-fmt :weapon-~d-dmg-bonus i)
					      :label-as "Dmg. Bonus"
					      :class-name "pure-u-1-6 pure-u-md-1-12"))))))

(defmacro output-field (name &key
			       (class-name nil)
			       (show-label t)
			       (label-as nil)
			       (input-class "pure-input-1"))
  `(htm
    (:*fudgable-field
     :name ,(make-keyword name)
     :class-name ,class-name
     :show-label ,show-label
     :label-as ,label-as
     :default-fudge (if (listp (aget ,(make-keyword name) (aget :fudges (chain c (obj)))))
			   (aget ,(make-keyword name) (aget :fudges (chain c (obj))))
			   (list))
     :validate-fudge (chain c
			      (validate-fudge this ,(make-keyword name)))
     :fudge-changed (chain c
			     (on-fudge-change ,(make-keyword name)))
     :value (calculate-field ,(make-keyword name) (chain c (obj)))
     :input-class ,input-class)))

(defmacro shallow-copy (obj)
  `(^ *object (assign (ps:create) ,obj)))

(defmacro defmithril-for-classish ((react-name name &key
					     onchange)
                                   controller-body
                                   &body b)
  `(defmithril ,react-name
               oninit
               (my-make-state (vnode)
                 (setf (^ this obj) (^ m (prop (shallow-copy (^ props default-value))))
                       (^ this handle-change)
                       (mlambda (v)
			 (tlambda (newval)
			   (setf (elt (chain this (obj)) v) newval)
			   ,@(when onchange
			       `((funcall ,onchange (^ this (obj)) v)))))
                       (chain this validate-fudge)
                       (mlambda (the-fudge)
			 (tlambda (newval)
			   (setf (elt (^ this (obj) fudges) the-fudge) newval)
			   (,(intern (format nil "~A-P" (string name)) (symbol-package name))
			     (elt (^ this (obj) fudges) the-fudge))))
		       (chain this validate)
		       (mlambda (v)
			 (tlambda (newval)
			   (setf (elt (^ this (obj)) v) newval)
			   (,(intern (format nil "VALIDATE-CHANGED-~A" (string name)) (symbol-package name))
			     (^ this (obj))
			     v))))
       ,controller-body
       this)
       ,@b))

(defmacro choice-field (name choices &key
				       (class-name "pure-u-1 pure-u-md-1-6")
				       (choice-class "pure-input-1")
				       (choice-values choices)
				       (id (string (gensym)))
				       (parser '(lambda (x) x))
				       override-value)
  (let ((name (alexandria:make-keyword (string name))))
    `(tlet ((id ,id))
	     (htm
	      (:div
	       :class-name ,class-name
	       (:*validating-select
		:default-value (aget ,name (chain c (obj)))
		:validator (chain c (validate ',name))
		:error-style (create background-color "pink"
				       padding 0)
		:style (create background-color "white"
				 padding 0)
		:parser ,parser
		:class-name ,choice-class
		:id id
		,@(when override-value (list :override-value override-value))
		:onchange (chain c (handle-change ',name))
		,@(loop for choice in choices
		     for choice-value in choice-values
		     collect `(:option :value ,choice-value
				       (esc ,(better-capitalize (string choice))))))
	       (:label :html-for id ,(better-capitalize(string name))))))))


(defmacro checkboxes-field (name choices &key
					 (class-name "pure-u-1 pure-u-md-1-6")
					 (checkbox-class nil)
					 (parser '(lambda (x) x)))
  (let ((name (alexandria:make-keyword (string name))))
    `(htm
	(:div
	 :class-name ,class-name
	 (:h3
	  :class-name "heading"
	  ,(better-capitalize (string name)))

	 (:*validating-checkboxes
	  :default-value (aget ,name (chain c (obj)))
	  :validator (chain c (validate ',name))
	  :parser ,parser
	  :class-name ,checkbox-class
	  :onchange (chain c (handle-change ',name))
	  :choices ,choices)))))

#+ps(defvar *row-keyn* 0)

(defmacro list-field (name make-row &key
				      make-new
				      (row-key '(lambda (_ i) (genid)))
				      inner-class
				      (show-name t)
				      read-only 
				      (class-name "pure-u-1 pure-u-md-1-6"))
  (let ((name (alexandria:make-keyword (string name))))
    `(htm
      (:div
       :class-name ,class-name
       ,@(when show-name (list (better-capitalize (string name))))
       (:*val-list
	:read-only ,read-only
	:value (chain (aget ,name (chain c (obj))) (slice))
	:validator (chain c (validate ',name))
	:onchange (chain c (handle-change ',name))
	:inner-class ,inner-class
	:row-key ,row-key
	:make-new ,make-new
	:make-row ,make-row)))))


 

