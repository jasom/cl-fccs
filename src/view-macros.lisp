(in-package :cl-fccs)

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
	       (:*validating-checkbox :default-value ({ (chain this state ,name))
				   :id ({ id)
				   :input-class ,input-class
				   :error-style ({(create background-color "pink"
							  padding 0))
				   :style ({(create background-color "white"
						    padding 0))
				   :validator ({ (chain this (validate this ',name)))
				   :on-change ({ (chain this (handle-change this ',name))))
	       ,@(when show-label
		       `((:label :html-for ({ id)
				 ,(better-capitalize (string name)))))
	       ))))))

(defmacro input-field (name  &key
			       (class-name "pure-u-1 pure-u-md-1-6")
			       (input-class "pure-input-1")
			       input-type
			       (show-label t)
			       (parser '({ (lambda (x) x))))
  (let ((name (alexandria:make-keyword (string name))))
  `({
     (tlet ((id (genid)))
       (htm
	(:div
	 :class-name ,class-name
	 :style ({(create margin 0))
	 (:*validating-input :default-value ({ (chain this state ,name))
			     :parser ,parser
			     :id ({ id)
			     :input-class ,input-class
			     :type ,input-type
			     :error-style ({(create background-color "pink"
						    padding 0))
			     :style ({(create background-color "white"
					      padding 0))
			     :validator ({ (chain this (validate this ',name)))
			     :on-change ({ (chain this (handle-change this ',name))))
	 ,@(when show-label
		 `((:label :html-for ({ id)
			   ,(better-capitalize (string name)))))
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

(defmacro output-field (name &key
			       (class-name nil)
			       (show-label t)
			       (input-class "pure-input-1"))
  `(htm
    (:*fudgable-field
     :name ,(make-keyword name)
     :class-name ({ ,class-name )
     :show-label ({ ,show-label )
     :default-fudge ({ (if (listp (aget ,(make-keyword name) (chain this state fudges)))
			  (aget ,(make-keyword name) (chain this state fudges)) 
			  (list)))
     :validate-fudge ({(chain this
			      (validate-fudge this ,(make-keyword name))))
     :fudge-changed ({(chain this
			     (on-fudge-change this ,(make-keyword name))))
     :value ({ (calculate-field ,(make-keyword name) (chain this state)))
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
    mixins (list (chain *react addons *pure-render-mixin))
    handle-change (lambda (ths v)
		(lambda (newval)
		  (let ((updater (create)))
		    (setf (getprop updater v) newval)
		    (chain ths (set-state updater)))
		  ,@(when on-change
			  `((let ((updater (create)))
			      (setf (getprop updater v)
				    (create $set newval))
			      (,on-change
				(chain *react addons (update
						      (chain ths state)
						      updater))
				v))))))
    on-fudge-change (lambda (ths the-fudge)
		    (lambda (newval)
		      (let ((updater (create)))
			(setf (aget the-fudge updater)
			      (create $set newval))
			(chain ths (set-state
				    (create fudges
					    (chain *react addons
						   (update (chain ths state fudges)
							   updater))))))
		      ,@(when on-change
			      `((let ((updater (create fudges (create))))
				  (setf (aget the-fudge (aget :fudges updater))
					(create $set newval))
				  (,on-change
				   (chain *react addons (update
							 (chain ths state)
							 updater))
				   the-fudge))))))
    validate-fudge (lambda (ths the-fudge)
		     (lambda (newval)
		       (let ((updater (create fudges (create))))
			 (setf (aget the-fudge (aget :fudges updater))
			       (create $set newval))
			 (,(intern (format nil "~A-P" (string name)) (symbol-package name))
			   (chain *react addons (update
						 (chain ths state)
						 updater))))))
    validate (lambda (ths v)
	       (lambda (newval)
		 (let ((updater (create)))
		   (setf (getprop updater v)
			 (create $set newval))
		   (,(intern (format nil "~A-P" (string name)) (symbol-package name))
		    (chain *react addons (update
					  (chain ths state)
					  updater))))))
    ,@b))

(defmacro choice-field (name choices &key
				       (class-name "pure-u-1 pure-u-md-1-6")
				       (choice-class "pure-input-1")
				       (choice-values choices)
				       (parser '({ (lambda (x) x) })))
  (let ((name (alexandria:make-keyword (string name))))
    `({
       (tlet ((id (genid)))
	     (htm
	      (:div
	       :class-name ,class-name
	       (:*validating-select
		:default-value ({ (chain this state ,name))
		:validator ({ (chain this (validate this ',name)))
		:error-style ({(create background-color "pink"
				       padding 0))
		:style ({(create background-color "white"
				 padding 0))
		:parser ,parser
		:class-name ,choice-class
		:id ({ id)
		:on-change ({ (chain this (handle-change this ',name)))
		,@(loop for choice in choices
		     for choice-value in choice-values
		     collect `(:option :value ,choice-value
				       (esc (string ,choice)))))
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
	  :default-value ({ (chain this state ,name))
	  :validator ({ (chain this (validate this ',name)))
	  :parser ,parser
	  :class-name ,checkbox-class
	  :on-change ({ (chain this (handle-change this ',name)))
	  :choices ,choices))))))

#+ps(defvar *row-keyn* 0)

(defmacro list-field (name make-row &key
				      make-new
				      (row-key '(lambda (_ i) (genid)))
				      inner-class
				      read-only 
				      (class-name "pure-u-1 pure-u-md-1-6"))
  (let ((name (alexandria:make-keyword (string name))))
    `(htm
      (:div
       :class-name ,class-name
       ,(better-capitalize (string name))
       (:*val-list
	:read-only ,read-only
	:value ({ (chain this state ,name))
	:validator ({ (chain this (validate this ',name)))
	:on-change ({ (chain this (handle-change this ',name)))
	:inner-class ,inner-class
	:row-key ({ ,row-key)
	:make-new ({ ,make-new)
	:make-row ({ ,make-row))))))
