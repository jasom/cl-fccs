#-ps(in-package cl-fccs)

(defreact-for-classish (*fudge fudge
			       :on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (chain this props default-value))
  render (lambda ()
	   (htm
	    (:div
	     :class-name "pure-u-1"
	     (input-field value
			  :parser ({ parse-int)
			  :class-name "pure-u-1-2")
	     (input-field notes
			  :class-name "pure-u-1-2")))))

(defun post-data (url data)
  (let ((request (new *x-m-l-http-request)))
    (chain request (open "POST" url t))
    (chain request (set-request-header "Content-Type" "application/json; charset=utf-8"))
    (let ((encoded-data (chain *json* (stringify data))))
      (chain console (log (chain encoded-data length)))
      (chain console (log (chain *l-z-string (compress encoded-data) length) ))
      (chain request (send encoded-data)))))

(defun get-h-offset (element)
  (let* ((rect (chain element (get-bounding-client-rect)))
	 (brect (chain document body (get-bounding-client-rect))))
    (- (chain rect left) (chain brect left))))


(defreact *fudgable-field
    mixins (list (chain *react addons *pure-render-mixin))
    get-initial-state (lambda ()
			(create popup nil))
    handle-click (lambda (ev)
		   (when (> (chain ev detail) 1)
		     (chain this
		       (set-state
			(create popup
				(if (> (get-h-offset (chain ev current-target))
				       200)
				    :right
				    :left))))))
    render (lambda ()
	     (htm
	      (:div
	       :class-name ({(chain this props class-name))
	       :style ({(create margin 0))
	       (:div
		:style ({(create padding 0))
		:on-click ({(chain this handle-click))
		:class-name ({(chain this props input-class))
		({(chain this props value))
		({(when (chain this state popup)
		    (htm
		     (:div
		      :style ({(create position :relative
				       width "100%" 
				       display :block
				       height 0))
		      (:div
		       :class-name
		       ({(if (eql (chain this state popup) :left)
			     "positioned-popup-left"
			     "positioned-popup-right"))
		       (:*val-list
			:value ({(chain this props default-fudge))
			:validator ({(chain this props validate-fudge))
			:on-change ({(chain this props fudge-changed))
			:row-key ({(lambda () (genid)))
			:make-row ({(lambda (data updater)
				      (htm (:*fudge
					    :default-value ({ data)
					    :on-change ({ updater)))))
			:make-new ({(chain
				     (lambda ()
				       (make-fudge :field
						   (to-keyword
						   (chain this props name))))
				     (bind this))))
			  
		       (:button
			:class-name "pure-button"
			:on-click ({(chain
				     (lambda (event)
				       (chain event (prevent-default))
				       (chain this (set-state (create popup nil))))
				     (bind this))) "Close")
				
		       ))))))
	       ({ (when (chain this props show-label)
		 (htm (:label ({(better-capitalize
				 (or (chain this props label-as)
				     (chain this props name))))))))))))

(defreact *validating-checkbox
    get-initial-state (lambda ()
			(create value (chain this props default-value)))
    handle-changed (lambda (ev)
		     (let ((val (chain ev target checked)))
		       (chain this (set-state (create value val)))
		       (when (chain this props (validator val))
			 (chain this props (on-change val)))))
    render (lambda ()
	     (htm (:input :checked ({ (chain this state value))
			  :id ({ (chain this props id))
			  :type :checkbox
			  :on-change ({ (chain this handle-changed))
			  :class-name ({ (chain this props input-class))
			  :style ({
				   (if (chain this
					      props
					      (validator
						      (chain this state value)))
				       (chain this props style)
				       (chain this props error-style)))))))

(defreact *validating-input
    get-initial-state (lambda ()
			(let ((val (chain this props default-value)))
			  (create value val
				  valid (chain this props
					       (validator
						(chain this props (parser val)))))))
    handle-changed (lambda (ev)
		     (when (chain this timeout)
		       (chain window (clear-timeout (chain this timeout))))
		     (let* ((rawval (chain ev target value))
			    (val (chain this props (parser rawval)))
			    (fn
			     (tlambda ()
			       (if (chain this props (validator val))
				   (progn
				     (chain this props (on-change val))
				     (chain this (set-state
						  (create valid t))))
				   (chain this (set-state (create valid nil)))
				   )))
			    (timeout (chain window (set-timeout fn 250))))
		       (chain this (set-state (create value rawval)))
		       (setf (chain this timeout) timeout)))
					   
    render (lambda ()
	     (htm (:input :value ({ (chain this state value))
			  :id ({ (chain this props id))
			  :type ({(if (chain this props type)
				      (chain this props type)
				      "text"))
			  :on-change ({ (chain this handle-changed))
			  :class-name ({ (chain this props input-class))
			  :style ({
				   (if (chain this state valid) 
				       (chain this props style)
				       (chain this props error-style)))))))
(defreact *validating-checkboxes
    get-initial-state (lambda ()
			(create value (chain this props default-value)))
    handle-changed (lambda (val)
		     (chain
			 (lambda (ev)
			   (let
			       ((value-copy (chain this state value (slice ))))
			     (if (chain ev target checked)
				 (chain value-copy (splice 0 0 val))
				 (chain value-copy
				   (splice 
				    (loop for i from 0
				       for item in value-copy
				       when (= item val) return i
				       finally (return (1+ i)))
				    1)))
			     (chain this (set-state (create value value-copy)))
			     (when (chain this props (validator value-copy))
			       (chain this props (on-change value-copy)))))
		       (bind this)))
    render (lambda ()
	     (htm
	      (:fieldset
					;:class-name ({ (chain this props class-name))
	       ({(loop for item in (chain this props choices)
		    collect (htm
			     (:div :class-name ({(chain this props class-name))
				   :key ({ item)
				   (:input
				    :type :checkbox
				    :class-name "pure-checkbox"
					;:label ({ item)
				    :checked ({ (member item (chain this state value)))
				    :on-change ({ (chain this (handle-changed item))))
				   (:label ({ item))))))))))

(defreact *validating-select
    get-initial-state (lambda ()
			(create value (chain this props default-value)))
    handle-changed (lambda (ev)
		     (when (chain this timeout)
		       (chain window (clear-timeout (chain this timeout))))
		     (let* ((rawval (chain ev target value))
			    (val (chain this props (parser rawval)))
			    (fn
				(tlambda ()
				  (if (chain this props (validator val))
				      (progn
					(chain this props (on-change val))
					(chain this (set-state (create valid t))))
				      (chain this (set-state (create valid nil)))
				      )))
			    (timeout (chain window (set-timeout fn 250))))
		       (chain this (set-state (create value rawval)))
		       (setf (chain this timeout) timeout)))
    #+(or)(lambda (ev)
		     (let ((val (chain this props (parser (chain ev target value)))))
		       (chain this (set-state (create value (chain ev target value))))
		       (when (chain this props (validator val))
			 (chain this props (on-change val)))))
    render (lambda ()
	     (htm (:select :value ({ (chain this state value))
			   :class-name ({ (chain this props class-name))
			   :on-change ({ (chain this handle-changed))
			   :id ({ (chain this props id))
			   :style ({
				   (if (chain this props (validator
							  (chain this state value)))
				       (chain this props style)
				       (chain this props error-style)))
			   ({ (chain this props children))))))

(defreact *val-list
    get-initial-state (lambda ()
			(let ((ths this))
			  (create keys
				  (loop for item in (chain ths props value)
				     for i from 0
				     collect (chain ths props (row-key item i))))))
    handle-change (lambda (ths v)
		    (lambda (newval)
		      (let ((newarray (chain ths props value (slice))))
			(setf (aref newarray v) newval)
			;(chain ths (set-state (create value newarray)))
			(chain ths props (on-change newarray)))))
    del (lambda (the-key)
	  (let ((ths this))
	    (lambda (ev)
	      (chain ev (prevent-default))
	      (destructuring-bind (newitems newkeys)
		  (loop for item in (chain ths props value)
		     for key in (chain ths state keys)
		     unless (eql key the-key)
		     collect item into items and
		     collect key into keys
		     finally (return (list items keys)))
		(chain ths props (on-change newitems))
		(chain ths (set-state (create keys newkeys)))
		))))
		       
    add (lambda ()
	  (let ((newarray (chain this props value (slice)))
		(newitem (chain this props (make-new))))
	    (chain newarray (push newitem))
	    (chain this (set-state
			 (create keys
				 (chain this state keys
					(concat
					 (list
					  (chain
					   this props
					   (row-key
					    newitem
					    (1- (chain newarray length))))))))))
	    (chain this props (on-change newarray))))
    render (lambda ()
	     (let* ((ths this)
		    (keys
		     (if (chain this props read-only)
			 (loop for item in (chain ths props value)
			    for i from 0
			    collect (chain ths props (row-key item i)))
			 (chain this state keys)))
		    (ths this)
		    (items
		     (loop
			for item in (chain ths props value)
			for key in keys
			for i from 0
			collect
			  (htm
			   (:div
			    :class-name ({ (chain ths props inner-class))
			    :key ({ key)
			    (:div
			     :class-name "pure-u-1 pure-u-md-1-12"
			     (:button :class-name "pure-button"
				      :style ({(create margin 0 padding "0.25em"))
				      :on-click ({ (chain ths (del key)))
				      (esc (string #\en_dash))))
			    ({ (chain ths props (make-row item (chain ths (handle-change ths i)))))
			    )))))
	       (htm
		(:fieldset
		 ({ items)
		 ({
		   (if (chain this props make-new)
		       (htm
			(:button :class-name "pure-button"
				 :on-click ({ (chain
					       (lambda (event)
						 (chain event (prevent-default))
						 (chain this (add))) (bind this)))
				 "+")))))))))

(defreact-for-classish (*weapon-info weapon-info
			       :on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (chain this props default-value))
  render (lambda ()
	    (htm
	     (:div
	      (input-field name :class-name "pure-u-1 pure-u-md-9-24")
	      (choice-field type #.cl-fccs::+proficiencies+
			    :choice-values #.(mapcar (lambda (x) (string-downcase (string x))) cl-fccs::+proficiencies+)
			    :class-name "pure-u-1 pure-u-md-1-3")
	      (input-field dmg-die :class-name "pure-u-1-6 pure-u-md-1-12")
	      (:div :style ({(create text-align :center)) :class-name "pure-u-1-24" "+")
	      ({(chain this props dmg-bonus))
	      ({(chain this props atk-bonus))
	      (input-field threat
			  :parser ({ parse-int) :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field size :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field hand
			  :parser ({ parse-int) :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field weight
			   :label-as "Wgt."
			  :parser ({ parse-int) :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field rng
			  :parser ({ (lambda (x) (when x (parse-int x))))
			  :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field shots
			  :parser ({ parse-int) :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field qualities :class-name "pure-u-1 pure-u-md-1-2")))))

		  
(defreact-for-classish (*fc-class fc-class
				  :on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (chain this props default-value))
  render (lambda ()
	   (htm
	    (:div
	     :class-name "pure-u-1 pure-u-md-11-12"
	     (input-field the-class
			  :class-name "pure-u-1 pure-u-md-2-3"
			  :parser ({ to-keyword))
	     (input-field level
			  :class-name "pure-u-1 pure-u-md-1-4"
			  :input-type "number"
			  :parser ({ parse-int))))))
	       

(defreact-for-classish (*ability-info ability-info
				  :on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (chain this props default-value))
  render (lambda ()
	   (htm
	    (:div :class-name nil
		  (:div
		   :class-name "pure-u-1-3"
		  ({ (+
		      (chain this state name)
		      " ("
		      (if (fc-class-p (chain this state from))
			  (+
			   (string-capitalize (chain this state from :the-class))
			   "/"
			   (chain this state from level))
			  (chain this state from))
		      ")")))
		  #+(or)(input-field name
			       :class-name "pure-u-1-4")
		  (input-field parameter
			       :class-name "pure-u-1-4")
		  (choice-field list-as 
				(:combat :non-combat :spellcasting)
				:choice-values ("combat" "non-combat" "spellcasting")
			       :class-name "pure-u-5-12")))))

(defreact-for-classish (*feat-info feat-info
				  :on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (chain this props default-value))
  render (lambda ()
	   (htm
	    (:div :class-name nil
		  (input-field name
			       :class-name "pure-u-2-3")
		   (input-field parameter
				:class-name "pure-u-1-3")
		  (input-field notes
			       :class-name "pure-u-7-12")
		  
		  (choice-field list-as 
				(:combat :non-combat :spellcasting)
				:choice-values ("combat" "non-combat" "spellcasting")
				:class-name "pure-u-5-12")))))
			
(defreact-for-classish (*character fc-character :on-change (tlambda (newch) (chain this (update-abilities newch))))
    get-initial-state (lambda ()
			(let ((value (chain this props default-value)))
			  (setf (aget :section value) "basics")
			  value))
    update-abilities (lambda (newch field) 
		       (unless (chain this upload-timer)
			 (setf (chain this upload-timer)
			       (chain window (set-timeout (chain this upload-data) 10000))))
		       ;(post-data "/fccs2/test/" newch)
		       (when (or t (eql field :classes))
			 (let ((updater (create)))
			   (setf (getprop updater :ability-list)
				 (fixup-abilities newch))
			   (chain this (set-state updater)))))
    upload-data (lambda ()
		  (post-data
		   (+
		    "/fccs2/save-character/"
		    (chain this props id))
		   (chain this state))
		  (setf (chain this upload-timer) nil))
    change-section (lambda (section)
		     (tlambda (ev)
		       (chain this (set-state (create section section)))))
    render (lambda ()
	     (htm
	      (:div
	       (:div
		:class-name "pure-menu-horizontal"
		(:ul :class-name "pure-menu-list"
		     (:li
		      :class-name ({(+ "pure-menu-item"
				     (if (= (chain this state  section) "basics")
					 " pure-menu-selected" "")))
		      (:a :href "#" :class-name "pure-menu-link"
			  :on-click ({(chain this (change-section "basics")))
			  "Basics"))
		     (:li
		      :class-name ({(+ "pure-menu-item"
				     (if (= (chain this state  section) "combat")
					 " pure-menu-selected" "")))
		      (:a :href "#" :class-name "pure-menu-link"
			  :on-click ({(chain this (change-section "combat")))
			  "Combat"))
		     (:li
		      :class-name ({(+ "pure-menu-item"
				     (if (= (chain this state  section) "gear")
					 " pure-menu-selected" "")))
		      (:a :href "#" :class-name "pure-menu-link"
			  :on-click ({(chain this (change-section "gear")))
			  "Gear"))
		     (:li
		      :class-name ({(+ "pure-menu-item"
				     (if (= (chain this state  section) "spell")
					 " pure-menu-selected" "")))
		      (:a :href "#" :class-name "pure-menu-link"
			  :on-click ({(chain this (change-section "spell")))
			  "Spellcasting"))
		     (:li
		      :class-name "pure-menu-item"
		      (:a :href "/fccs2/" :class-name "pure-menu-link"
			  :on-click ({(tlambda ()
					(when
					    (chain this upload-timer)
					  (chain this (upload-data)))))
			  "Save & Exit"))))
	       ({(cond
		   ((= (chain this state section) "basics")
		    (htm
		     (:form :key "basics"
		      :class-name "pure-form pure-form-stacked"
		      (:div
		       :class-name "pure-g"
		       (:div
			:class-name "pure-u-1 pure-u-md-3-4 pure-u-xl-3-8 pure-g"
			(input-field character-name
				     :class-name "pure-u-1 pure-u-md-1-4")
			(input-field species
				     :class-name "pure-u-1 pure-u-md-1-8")
			(input-field talent
				     :class-name "pure-u-1 pure-u-md-1-8"
				     )
			(input-field specialty
				     :class-name "pure-u-1 pure-u-md-1-4")
			(choice-field strong-attr
				      (:str :dex :con :int :wis :cha)
				      :choice-values 
				      ("str" "dex" "con" "int" "wis" "cha")
				      :class-name "pure-u-1 pure-u-md-1-8")
			(choice-field weak-attr
				      (:str :dex :con :int :wis :cha)
				      :choice-values 
				      ("str" "dex" "con" "int" "wis" "cha")
				      :class-name "pure-u-1 pure-u-md-1-8")
			(list-field classes
				    (lambda (data updater)
				      (htm (:*fc-class
					    :default-value ({ data)
					    :on-change ({ updater))))
				    :class-name "pure-u-1 pure-g"
					;:row-key (lambda (x) (aget :the-class x))
				    :inner-class "pure-u-1-3 pure-g"
				    :make-new #'make-fc-class
				    )
			(input-field player-name
				     :class-name "pure-u-1 pure-u-md-1-3")
			(input-field xp 
				     :parser ({ parse-int)
				     :class-name "pure-u-1 pure-u-md-1-3")
			(cl-fccs::output-field next-xp
					       :class-name "pure-u-1 pure-u-md-1-3")
			(input-field gender :class-name "pure-u-1 pure-u-md-1-6")
			(input-field age :class-name "pure-u-1 pure-u-md-1-6")
			(input-field height :class-name "pure-u-1 pure-u-md-1-6")
			(input-field weight :class-name "pure-u-1 pure-u-md-1-6")
			(input-field eyes :class-name "pure-u-1 pure-u-md-1-6")
			(input-field hair :class-name "pure-u-1 pure-u-md-1-6")
			(:div
			 :class-name "pure-u-1 pure-u-md-1-2"
			 (:table
			  :class-name "pure-table"
			  (:thead
			   (:tr
			    (:th
			     :col-span 3
			     :style ({(create text-align "center"))
			     "Physical Attributes")
			    (:th
			     (output-field skill-points :show-label nil))))
			  (:tr
			   (:td "Name")
			   (:td "Base")
			   (:td "Real")
			   (:td "Mod."))
			  (:tr
			   (:th "Str")
			   (:td (input-field base-str :class-name nil 
					     :input-class "pure-input-1"
					     :show-label nil
					     :parser ({ parse-int)))
			   (:td (output-field real-str :class-name nil
					      :show-label nil
					      :input-class "pure-input-1"))
			   (:td (output-field str-mod :class-name nil
					      :show-label nil
					      :input-class "pure-input-1")))
			  (:tr
			   (:th "Dex")
			   (:td (input-field base-dex :class-name nil 
					     :show-label nil
					     :input-class "pure-input-1"
					     :parser ({ parse-int)))
			   (:td (output-field real-dex :class-name nil
					      :show-label nil
					      :input-class "pure-input-1"))
			   (:td (output-field dex-mod :class-name nil
					      :show-label nil
					      :input-class "pure-input-1")))
			  (:tr
			   (:th "Con")
			   (:td (input-field base-con :class-name nil 
					     :input-class "pure-input-1"
					     :show-label nil
					     :parser ({ parse-int)))
			   (:td (output-field real-con :class-name nil
					      :show-label nil
					      :input-class "pure-input-1"))
			   (:td (output-field con-mod :class-name nil
					      :show-label nil
					      :input-class "pure-input-1")))))
			(:div
			 :class-name "pure-u-1 pure-u-md-1-2"
			 (:table
			  :class-name "pure-table"
			  (:thead
			   (:tr
			    (:th
			     :col-span 4
			     :style ({(create text-align "center"))
			     "Mental Attributes")))
			  (:tr
			   (:td "Name")
			   (:td "Base")
			   (:td "Real")
			   (:td "Mod."))
			  (:tr
			   (:th "Int")
			   (:td (input-field base-int :class-name nil 
					     :input-class "pure-input-1"
					     :show-label nil
					     :parser ({ parse-int)))
			   (:td (output-field real-int :class-name nil
					      :show-label nil
					      :input-class "pure-input-1"))
			   (:td (output-field int-mod :class-name nil
					      :show-label nil
					      :input-class "pure-input-1")))
			  (:tr
			   (:th "Wis")
			   (:td (input-field base-wis :class-name nil 
					     :show-label nil
					     :input-class "pure-input-1"
					     :parser ({ parse-int)))
			   (:td (output-field real-wis :class-name nil
					      :show-label nil
					      :input-class "pure-input-1"))
			   (:td (output-field wis-mod :class-name nil
					      :show-label nil
					      :input-class "pure-input-1")))
			  (:tr
			   (:th "Cha")
			   (:td (input-field base-cha :class-name nil 
					     :input-class "pure-input-1"
					     :show-label nil
					     :parser ({ parse-int)))
			   (:td (output-field real-cha :class-name nil
					      :show-label nil
					      :input-class "pure-input-1"))
			   (:td (output-field cha-mod :class-name nil
					      :show-label nil
					      :input-class "pure-input-1")))	
			  )
			 ))
		       (:div
			:class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8 pure-g"
			#+(or)(:img :src "/fccs2/pub/images/fc.png"
				    :class-name "pure-img")
			(output-field starting-action-dice
				      :class-name "pure-u-1 pure-u-md-1-2")
			(output-field action-dice-type
				      :class-name "pure-u-1 pure-u-md-1-2"))
		       (:div
			:class-name "pure-u-1 pure-u-xl-1-2"
			(:h2
			 :class-name "pure-u-3-4"
			 :style ({(create text-align "center"
					  margin 0
					  background-color "#e0e0e0"))
			 "Skills")
			(output-field total-ranks
				      :class-name "pure-u-1-4")
			(skill-table #.(subseq +skills+ 0 (ceiling (length +skills+) 2)))
			(skill-table #.(subseq +skills+ (ceiling (length +skills+) 2))))
		       (:div
			:class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
			(:h2
			 :style ({(create text-align "center"
					  margin 0
					  background-color "#e0e0e0"))
			 "Focuses")
			(list-field crafting-foci
				    (lambda (data updater)
				      (htm
				       (:div
					:class-name "pure-u-11-12"
					(:*validating-input
					 :default-value ({ data)
					 :parser ({ (lambda (x) x))
					 :style ({(create padding 0))
					 :input-class "pure-input-1"
					 :validator ({ (lambda () t))
					 :on-change ({ updater)))))
				    :class-name "pure-u-1 pure-u-md-1-2 pure-g"
				    :inner-class nil
				    :make-new (lambda () ""))
			(list-field ride-foci
				    (lambda (data updater)
				      (htm
				       (:div
					:class-name "pure-u-11-12"
					(:*validating-input
					 :default-value ({ data)
					 :parser ({ (lambda (x) x))
					 :input-class "pure-input-1"
					 :style ({(create padding 0))
					 :validator ({ (lambda () t))
					 :on-change ({ updater)))))
				    :class-name "pure-u-1 pure-u-md-1-2 pure-g"
				    :inner-class nil
				    :make-new (lambda () "")))
		       (:div
			:class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
			(:h2
			 :style ({(create text-align "center"
					  margin 0
					  background-color "#e0e0e0"))
			 "Interests")
			(input-field alignment
				     :class-name "pure-u-1 pure-u-md-1-2")
			(list-field languages
				    (lambda (data updater)
				      (htm
				       (:*validating-input
					:default-value ({ data)
					:parser ({ (lambda (x) x))
					:input-class "pure-input-1"
					:validator ({ (lambda () t))
					:on-change ({ updater))))
				    :class-name  nil
				    :inner-class "pure-u-1 pure-u-md-1-2"
				    :make-new (lambda () ""))
			(list-field interests
				    (lambda (data updater)
				      (htm
				       (:*validating-input
					:default-value ({ data)
					:parser ({ (lambda (x) x))
					:input-class "pure-input-1"
					:validator ({ (lambda () t))
					:on-change ({ updater))))
				    :class-name  nil
				    :inner-class "pure-u-1 pure-u-md-1-2"
				    :make-new (lambda () "")))
		       (:div
			:class-name "pure-u-1 pure-u-xl-1-2"
			(:h2
			 :style ({(create text-align "center"
					  margin 0
					  background-color "#e0e0e0"))
			 "Subplots")
			(list-field completed-subplots
				    (lambda (data updater)
				      (htm
				       (:*validating-input
					:default-value ({ data)
					:parser ({ (lambda (x) x))
					:input-class "pure-input-1"
					:validator ({ (lambda () t))
					:on-change ({ updater))))
				    :class-name  nil
				    :inner-class "pure-u-1 pure-u-md-1-4"
				    :make-new (lambda () ""))
			(list-field incomplete-subplots
				    (lambda (data updater)
				      (htm
				       (:*validating-input
					:default-value ({ data)
					:parser ({ (lambda (x) x))
					:input-class "pure-input-1"
					:validator ({ (lambda () t))
					:on-change ({ updater))))
				    :class-name  nil
				    :inner-class "pure-u-1 pure-u-md-1-4"
				    :make-new (lambda () "")))
		       (:div
			:class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
			(:h2
			 :style ({(create text-align "center"
					  margin 0
					  background-color "#e0e0e0"))
			 "Coin")
			(input-field coin-in-hand
				     :parser ({ parse-int)
				     :class-name "pure-u-1")
			(input-field stake
				     :parser ({ parse-int)
				     :class-name "pure-u-1"))
		       (:div
			:class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
			(:div
			 :class-name "pure-u-1 pure-u-md-7-8"
			 :style ({(create margin 0 padding 0))
			 (:h2
			  :style ({(create text-align "center"
					   margin 0
					   background-color "#e0e0e0"))
			  "Lifestyle"))
			(output-field lifestyle
				      :class-name "pure-u-1 pure-u-md-1-8"
				      :show-label nil)
			(input-field panache
				     :parser ({ parse-int)
				     :class-name "pure-u-1 pure-u-md-1-2")
			(input-field prudence
				     :parser ({ parse-int)
				     :class-name "pure-u-1 pure-u-md-1-2")
			(output-field appearance-bonus
				      :class-name "pure-u-1 pure-u-md-1-2")
			(output-field money-saved/earned
				      :class-name "pure-u-1 pure-u-md-1-2")
			(output-field income
				      :class-name "pure-u-1 pure-u-md-1-2"))

		       (:h2 :class-name "pure-u-1 heading" "Abilities")
		       (list-field ability-list
				   (lambda (data updater)
				     (htm (:*ability-info
					   :default-value ({ data)
					   :on-change ({ updater))))
				   :read-only t
				   :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
				   :row-key (lambda (x) (+ (aget :name x)
							   (aget :from x)))
				   :inner-class "pure-u-1 pure-u-md-1-2")
		       (list-field feat-list
				   (lambda (data updater)
				     (htm (:*feat-info
					   :default-value ({ data)
					   :on-change ({ updater))))
				   :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
					;:row-key (lambda (x) (+ (aget :name x)
					;(aget :parameter x)
					;(aget :notes x)))
				   :make-new #'make-feat-info
				   :inner-class "pure-u-1 pure-u-md-1-2")
		       ))))
		   ((= (chain this state section) "combat")
		    (htm
		     (:form :key "combat"
		      :class-name "pure-form pure-form-stacked"
		      (:div
		       :class-name "pure-g"
		       (:div
			:class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
			(output-field defense :class-name "pure-u-1-6")
			(output-field base-defense :class-name "pure-u-1-6"
				      :label-as "Base")
			(output-field defense-attr-mod :label-as "Attr"
				      :class-name "pure-u-1-6")
			(output-field defense-size-mod :label-as "Size"
				      :class-name "pure-u-1-6")
			(output-field defense-misc-mod :label-as "Misc"
				      :class-name "pure-u-1-6")
			(output-field vitality :class-name "pure-u-1-2")
			(output-field wounds :class-name "pure-u-1-2")
			(input-field critical-injuries :class-name "pure-u-1")
			(:h2
			 :class-name "header pure-u-1"
			 "Size")
			(output-field size :class-name "pure-u-1-3")
			(output-field footprint :class-name "pure-u-1-3")
			(output-field reach :class-name "pure-u-1-3")
			(output-field ground-speed :class-name "pure-u-1-3")
			(:div :class-name "pure-u-1-3") ;TODO Insert other speed
			(output-field travel-speed :class-name "pure-u-1-3")
			(checkboxes-field proficiency-list
					  ({ +proficiencies+)
					  :class-name "pure-u-1"
					  :checkbox-class "pure-u-1-4")
			(checkboxes-field forte-list
					  ({ +proficiencies+)
					  :class-name "pure-u-1"
					  :checkbox-class "pure-u-1-4"))
		       (:div
			:class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
			(output-field initiative :class-name "pure-u-1-4")
			(output-field base-init :class-name "pure-u-1-4")
			(output-field init-attr-mod :class-name "pure-u-1-4")
			(output-field init-misc-mod :class-name "pure-u-1-4")
			(:div
			 :class-name "pure-u-1 pure-u-sm-1-2 pure-g"
			 (:h2
			  :class-name "heading pure-u-1"
			  "Base Attacks")
			 (:div
			  :class-name "pure-u-1-3 label"
			  "Type")
			 (:div
			  :class-name "pure-u-1-6 label"
			  "Total")
			 (:div
			  :class-name "pure-u-1-6 label"
			  "Base")
			 (:div
			  :class-name "pure-u-1-6 label"
			  "Attr. Mod")
			 (:div
			  :class-name "pure-u-1-6 label"
			  "Misc. Mod")
			 (:div
			  :class-name "pure-u-1-3 label"
			  "Unarmed")
			 (output-field :unarmed-bonus
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :bab
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :unarmed-attr-mod
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :unarmed-misc-mod
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (:div
			  :class-name "pure-u-1-3 label"
			  "Melee")
			 (output-field :melee-bonus
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :bab
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :melee-attr-mod
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :melee-misc-mod
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (:div
			  :class-name "pure-u-1-3 label"
			  "Ranged")
			 (output-field :ranged-bonus
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :bab
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :ranged-attr-mod
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :ranged-misc-mod
				       :class-name "pure-u-1-6"
				       :show-label nil))
			(:div
			 :class-name "pure-u-1 pure-u-sm-1-2 pure-g"
			 (:h2
			  :class-name "heading pure-u-1"
			  "Saving Throws")
			 (:div
			  :class-name "pure-u-1-3 label"
			  "Type")
			 (:div
			  :class-name "pure-u-1-6 label"
			  "Total")
			 (:div
			  :class-name "pure-u-1-6 label"
			  "Base")
			 (:div
			  :class-name "pure-u-1-6 label"
			  "Attr. Mod")
			 (:div
			  :class-name "pure-u-1-6 label"
			  "Misc. Mod")
			 (:div
			  :class-name "pure-u-1-3 label"
			  "Fortitude")
			 (output-field :fortitude-bonus
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :fortitude-base
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :fortitude-attr-mod
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :fortitude-misc-mod
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (:div
			  :class-name "pure-u-1-3 label"
			  "Reflex")
			 (output-field :reflex-bonus
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :reflex-base
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :reflex-attr-mod
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :reflex-misc-mod
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (:div
			  :class-name "pure-u-1-3 label"
			  "Will")
			 (output-field :will-bonus
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :will-base
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :will-attr-mod
				       :class-name "pure-u-1-6"
				       :show-label nil)
			 (output-field :will-misc-mod
				       :class-name "pure-u-1-6"
				       :show-label nil))
			(weapon-table 4)
			(choice-field armor-type (:none :partial :moderate)
				      :class-name "pure-u-1 pure-u-md-1-4"
				      :choice-values ("none" "partial" "moderate"))
			(input-field armor-name
				     :class-name "pure-u-1 pure-u-md-1-4")
			(input-field armor-craftsmanship
				     :class-name "pure-u-1 pure-u-md-1-4")
			(input-field armor-construction
				     :class-name "pure-u-1 pure-u-md-1-4")
			(input-field armor-customizations
				     :class-name "pure-u-1 pure-u-md-5-6"
				     :formatter (lambda (x)
						  (chain x (join ", ")))
				     :parser ({ (lambda (x) (chain x
							      (split (ps:new (*reg-exp ", *")))))))
			(choice-field armor-fittings
				      ("none" "light" "heavy"))
			)
		       ))))
		   ))))))

(defreact *character-list
    mixins (list (chain *react addons *pure-render-mixin))
    edit (lambda (id)
	   (lambda ()
	     (setf (chain window location)
		   (+ "/fccs2/character/" id))))
    render (lambda ()
	     (let ((ths this))
	       (htm
		(:div
		 (:table
		  :class-name "pure-table"
		  (:thead
		   (:tr
		    (:th "ID")
		    (:th "Character Name")
		    (:th "Player Name")
		    (:th "Level")))
		  (:tbody
		   ({(loop for item in (chain ths props value)
			collect
			  (htm
			   (:tr
			    :on-click ({(chain ths (edit (aget :id item))))
			    :key ({(aget :id item))
			    (:td ({(aget :id item)))
			    (:td ({(aget :char-name item)))
			    (:td ({(aget :player-name item)))
			    (:td ({(aget :career-level item)))))))))
		 (:form
		  (:button
		   :form-action "/fccs2/new-character/"
		   :form-method "POST"
		   :class-name "pure-button"
		   :on-click ({(lambda ()
				 (setf (chain window location)
				       "/fccs2/new-character/")))
		   "New")))))))
		  

;(chain *react (render (htm :*Test) (chain document body)))
;(chain *react (render (htm (:*Character)) (chain document body)))
