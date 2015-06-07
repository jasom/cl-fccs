(in-package :cl-fccs)

(defmacro mlambda ((arg) &body b)
  (let ((memo (gensym)))
    `(lambda (,arg)
       (unless (ps:in ',memo this)
	 (setf (ps:@ this ,memo) (create)))
       (if (ps:in ,arg (ps:@ this ,memo))
	   (ps:getprop (ps:@ this ,memo) ,arg)
	   (let ((result (funcall (tlambda (,arg) ,@b) ,arg)))
	     (setf (ps:getprop (ps:@ this ,memo) ,arg) result)
	     result)))))

(defmacro checkbox-field (name  &key
			       (class-name "pure-u-1 pure-u-md-1-6")
			       (input-class "pure-checkbox")
			       (show-label t))
  (let ((name (alexandria:make-keyword (string name))))
    `({
       (tlet ((id (genid)))
	     (htm
	      (:div
	       :class-name ,class-name
	       :style ({(create margin 0))
	       (:*validating-checkbox :default-value ({(aget ,name (chain this state obj)))
				   :id ({ id)
				   :input-class ,input-class
				   :error-style ({(create background-color "pink"
							  padding 0))
				   :style ({(create background-color "white"
						    padding 0))
				   :validator ({ (chain this (validate ',name)))
				   :on-change ({ (chain this (handle-change ',name))))
	       ,@(when show-label
		       `((:label :html-for ({ id)
				 ,(better-capitalize (string name)))))
	       ))))))

(defmacro autocomplete-input-field (name  &key
					    (class-name "pure-u-1 pure-u-md-1-6")
					    (input-class "pure-input-1")
					    input-type
					    update-completions
					    (label-as nil)
					    (show-label t)
					    (formatter '(lambda (x) x))
					    (parser '({ (lambda (x) x))))
  (let ((name (alexandria:make-keyword (string name))))
    `({
       (tlet ((id (genid)))
	     (htm
	      (:div
	       :class-name ,class-name
	       :style ({(create margin 0))
	       (:*validating-autocomplete
		:default-value ({ (funcall ,formatter (aget ,name (chain this state obj))))
		:parser ,parser
		:id ({ id)
		:input-class ,input-class
		:type ,input-type
		:error-style ({(create background-color "pink"
				       padding 0))
		:style ({(create background-color "white"
				 padding 0))
		:update-completions ({ ,update-completions)
		:validator ({ (chain this (validate ',name)))
		:on-change ({ (chain this (handle-change ',name))))
	       ,@(when show-label
		       `((:label :html-for ({ id)
				 ,(if label-as
				      label-as
				      (better-capitalize (string name))))))
	       ))))))

(defmacro input-field (name  &key
			       (class-name "pure-u-1 pure-u-md-1-6")
			       (input-class "pure-input-1")
			       input-type
			       (label-as nil)
			       (show-label t)
			       (formatter '(lambda (x) x))
			       (parser '({ (lambda (x) x)))
			       override-value)
  (let ((name (alexandria:make-keyword (string name))))
  `({
     (tlet ((id (genid)))
       (htm
	(:div
	 :class-name ,class-name
	 :style ({(create margin 0))
	 (:*validating-input :default-value ({ (funcall ,formatter (aget ,name (chain this state obj))))
			     :parser ,parser
			     :id ({ id)
			     :input-class ,input-class
			     :type ,input-type
			     :error-style ({(create background-color "pink"
						    padding 0))
			     :style ({(create background-color "white"
					      padding 0))
			     :validator ({ (chain this (validate ',name)))
			     :on-change ({ (chain this (handle-change ',name)))
			     ,@(when override-value
				     `(:override-value ,override-value)))

	 ,@(when show-label
		 `((:label :html-for ({ id)
			   ,(if label-as
				label-as
				(better-capitalize (string name))))))
	 ))))))

(defmacro skill-table (skills)
    `(htm
      (:div
       :class-name "pure-u-1 pure-u-md-1-2 pure-g"
       :style ({(create font-size "75%"))
       :text-align "center"
       (:div :class-name "pure-u-1-12"
	     "OS")
       (:div :class-name "pure-u-1-6"
	     :style ({(create font-size "75%"))
	     "Skill Name")
       (:div :class-name "pure-u-1-8"
	     :style ({(create font-size "75%"))
	     "Attr.")
       (:div :class-name "pure-u-1-8"
	     :style ({(create font-size "75%"))
	     "Skill Bonus")
       (:div :class-name "pure-u-1-8"
	     :style ({(create font-size "75%"))
	     "Ranks")
       (:div :class-name "pure-u-1-8"
	     :style ({(create font-size "75%"))
	     "Attr Mod.")
       (:div :class-name "pure-u-1-8"
	     :style ({(create font-size "75%"))
	     "Misc Mod.")
       (:div :class-name "pure-u-1-8"
	     :style ({(create font-size "75%"))
	     "Threat")
       (progn
	 ,@(loop for item in skills
	      for fitem = (ssub "_" " " (string item))
	      collect
		`(htm
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
				     :parser ({ parse-int)))
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
				     :parser ({ parse-int)))))))))

(defmacro weapon-table (n)
  `(htm
    (:div :class-name "pure-u-1"
	  ,@(loop for i from 1 to n
	       collect
		 `(:*weapon-info
		   :default-value ({(aget ,(key-fmt :weapon-~d i) (chain this state obj) nil))
					;TODO validator
		   :on-change ({(chain this (handle-change ,(key-fmt :weapon-~d i))))
		   :atk-bonus ({(output-field ,(key-fmt :weapon-~d-atk-bonus i)
					      :label-as "Atk"
					      :class-name "pure-u-1-6 pure-u-md-1-12"))
		   :dmg-bonus ({(output-field ,(key-fmt :weapon-~d-dmg-bonus i)
					      :label-as "Dmg. Bonus"
					      :class-name "pure-u-1-6 pure-u-md-1-12")))))))

(defmacro output-field (name &key
			       (class-name nil)
			       (show-label t)
			       (label-as nil)
			       (input-class "pure-input-1"))
  `(htm
    (:*fudgable-field
     :name ,(make-keyword name)
     :class-name ({ ,class-name )
     :show-label ({ ,show-label )
     :label-as ({ ,label-as )
     :default-fudge ({ (if (listp (aget ,(make-keyword name) (aget :fudges (chain this state obj))))
			   (aget ,(make-keyword name) (aget :fudges (chain this state obj)))
			   (list)))
     :validate-fudge ({(chain this
			      (validate-fudge this ,(make-keyword name))))
     :fudge-changed ({(chain this
			     (on-fudge-change ,(make-keyword name))))
     :value ({ (calculate-field ,(make-keyword name) (chain this state obj)))
     :input-class ,input-class))
  #+(or)`({
     (tlet ((id (genid)))
	   (htm
	    (:div
	     :class-name ,class-name
	     :style ({(create margin 0))
	     (:div
	      :style ({(create padding 0))
	      :class-name ,input-class
	      ({ (calculate-field
		  ,(make-keyword (string name))
		  (chain this state))))
	     ,@(when show-label
		     `(
		       (:label 
			,(better-capitalize(string name))))))))))

(defmacro defreact-for-classish ((react-name name &key
					     on-change) &body b)
  `(defreact ,react-name
       mixins (ps:array (chain *react addons *pure-render-mixin))
       handle-change (mlambda (v)
		       (tlambda (newval)
			 (chain this
				(set-state
				 (lambda (state props context)
				   (let ((newobj (chain state obj (set v newval))))
				     ,@(when on-change
					     `((,on-change newobj v)))
				     (create obj newobj)))))))
       validate-fudge (mlambda (the-fudge)
			(tlambda (newval)
			  (let* ((obj
				  (chain this state obj))
				 (newobj
				  (chain this state obj
					 (update-in
					  (ps:array :fudges the-fudge)
					  (lambda (oldval) newval)))))
			    (,(intern (format nil "~A-P" (string name)) (symbol-package name))
			      newobj))))
       validate (mlambda (v)
		  (tlambda (newval)
		    (let ((newstate (chain this state obj (set v newval))))
		      (,(intern (format nil "VALIDATE-CHANGED-~A" (string name)) (symbol-package name))
			newstate
			v))))
       ,@b))

(defmacro choice-field (name choices &key
				       (class-name "pure-u-1 pure-u-md-1-6")
				       (choice-class "pure-input-1")
				       (choice-values choices)
				       (parser '({ (lambda (x) x) }))
				       override-value)
  (let ((name (alexandria:make-keyword (string name))))
    `({
       (tlet ((id (genid)))
	     (htm
	      (:div
	       :class-name ,class-name
	       (:*validating-select
		:default-value ({ (aget ,name (chain this state obj)))
		:validator ({ (chain this (validate ',name)))
		:error-style ({(create background-color "pink"
				       padding 0))
		:style ({(create background-color "white"
				 padding 0))
		:parser ,parser
		:class-name ,choice-class
		:id ({ id)
		,@(when override-value (list :override-value override-value))
		:on-change ({ (chain this (handle-change ',name)))
		,@(loop for choice in choices
		     for choice-value in choice-values
		     collect `(:option :value ,choice-value
				       (esc (better-capitalize (string ,choice))))))
	       (:label :html-for ({ id) ,(better-capitalize(string name)))))))))


(defmacro checkboxes-field (name choices &key
					 (class-name "pure-u-1 pure-u-md-1-6")
					 (checkbox-class nil)
					 (parser '({ (lambda (x) x) })))
  (let ((name (alexandria:make-keyword (string name))))
    `({
       (htm
	(:div
	 :class-name ,class-name
	 (:h3
	  :style ({(create background-color "#eee"))
	  ,(better-capitalize (string name)))

	 (:*validating-checkboxes
	  :default-value ({(aget ,name (chain this state obj)))
	  :validator ({ (chain this (validate ',name)))
	  :parser ,parser
	  :class-name ,checkbox-class
	  :on-change ({ (chain this (handle-change ',name)))
	  :choices ,choices))))))

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
	:value ({(aget ,name (chain this state obj)))
	:validator ({ (chain this (validate ',name)))
	:on-change ({ (chain this (handle-change ',name)))
	:inner-class ,inner-class
	:row-key ({ ,row-key)
	:make-new ({ ,make-new)
	:make-row ({ ,make-row))))))

