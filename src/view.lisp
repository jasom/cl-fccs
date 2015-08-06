(in-package cl-fccs)

(defvar *view-only* nil)

(defun index-of (item the-array)
  (if (not (chain *array prototype index-of))
      (loop for i from 0
	 for this-item in the-array
	   when (= item this-item) return i
	 finally (return -1))
      (chain the-array (index-of item))))

(defun fixup-path (path)
  (+ *prepend-path*
     (if (eql (elt path 0) #\/)
		(chain path (slice 1))
		path)))

(defreact-for-classish (*contact-info
			contact-info
			:on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (create obj (chain this props default-value)))
  render (lambda ()
	   (htm
	    (:div
	     (input-field name
			  :class-name "pure-u-md-1-2")
	     (input-field trust
			  :class-name "pure-u-md-1-8")
	     (input-field size
			  :class-name "pure-u-md-1-8")
	     (input-field reach
			  :class-name "pure-u-md-1-8")
	     (input-field speed
			  :class-name "pure-u-md-1-8")
	     (input-field attributes
			  :class-name "pure-u-md-5-6")
	     (input-field rep-cost)
	     (input-field init)
	     (input-field atk)
	     (input-field def)
	     (input-field res)
	     (input-field health)
	     (input-field comp)
	     (input-field skills
			  :class-name "pure-u-1")
	     (input-field qualities
			  :class-name "pure-u-1")
	     (input-field attacks
			  :class-name "pure-u-1")
	     (input-field gear
			  :class-name "pure-u-1")))))


(defreact-for-classish (*holding-info
			holding-info
			:on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (create obj (chain this props default-value)))
  render (lambda ()
	   (htm
	    (:div
	     (input-field name
			  :class-name "pure-u-1 pure-u-md-1-2")
	     (input-field scale
			  )
	     (input-field guests
			  )
	     (input-field max-guests
			  )
	     (input-field upgrades
			  :class-name "pure-u-1 pure-u-md-5-6")
	     (input-field rep-cost)))))

(defreact-for-classish (*magic-item-info
			magic-item-info
			:on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (create obj (chain this props default-value)))
  render (lambda ()
	   (htm
	    (:div
	     (input-field name
			  :class-name "pure-u-1 pure-u-md-1-4")
	     (input-field level
			  :class-name "pure-u-1 pure-u-md-1-8")
	     (input-field essences
			  :class-name "pure-u-1 pure-u-md-1-4")
	     (input-field charms
			  :class-name "pure-u-1 pure-u-md-1-4")
	     (input-field rep-cost
			  :class-name "pure-u-1 pure-u-md-1-8")))))

(defreact-for-classish (*fudge fudge
			       :on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (create obj (chain this props default-value)))
  render (lambda ()
	   (htm
	    (:div
	     :class-name "pure-u-1"
	     (input-field value
			  :parser ({ parse-int)
			  :class-name "pure-u-1-2")
	     (input-field notes
			  :class-name "pure-u-1-2")))))

(defun post-data (url data &key complete-callback raw)
  (let ((request (new *x-m-l-http-request)))
    (chain request (open "POST" url t))
    (chain request (set-request-header "Content-Type" "application/json; charset=utf-8"))
    (chain request (set-request-header "x-csrf-token"
				      (chain document (get-element-by-id "csrf") inner-h-t-m-l (trim))))
    (let ((encoded-data
	   (if raw
	       data
	       (chain *json* (stringify data)))))
     (when complete-callback
	(setf (chain request onload) complete-callback))
     (chain request (send encoded-data))
      
      #+(or)(chain console (log (chain encoded-data length)))
      #+(or)(chain console (log (chain *l-z-string (compress encoded-data) length) ))
      #+(or)(let ((request2 (new *x-m-l-http-request))
	    (comp-data (chain *l-z-string (compress-to-u-t-f-8
#+(or)"abcdefghijklmnopqrstuvwxyzqwertyuiopasdfghjklzxcvbnm,1234567890imqwoimf9082mfoimqw
qowimefoqmwefoimwoifmqoimoimiomeoimfoimoimoqiwmeimfoim"
					   #-(or)encoded-data))))
	(chain request2 (open "POST" "/fccs2/lztest/" t))
	(chain request2 (set-request-header "Content-Type" "bin"))
	(console.log (chain comp-data (char-code-at 0)))
	(console.log (chain comp-data (char-code-at 1)))
	(chain request2 (send comp-data)))
      )))

(defun get-h-offset (element)
  (let* ((rect (chain element (get-bounding-client-rect)))
	 (brect (chain document body (get-bounding-client-rect))))
    (- (chain rect left) (chain brect left))))


(defreact *fudgable-field
    mixins (ps:array (chain *react addons *pure-render-mixin))
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
			  :disabled ({ *view-only*)
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
	     (htm (:input :value ({ (if (and (chain this state valid)
			     (ps:in 'override-value (chain this props)))
			(chain this props override-value)
			(chain this state value)))
			  :id ({ (chain this props id))
			  :type ({(if (chain this props type)
				      (chain this props type)
				      "text"))
			  :on-change ({ (chain this handle-changed))
			  :class-name ({ (chain this props input-class))
			  :disabled ({ *view-only*)
			  :style ({
				   (if (chain this state valid) 
				       (chain this props style)
				       (chain this props error-style)))))))

(defreact *validating-autocomplete
    get-initial-state (lambda ()
			(let ((val (chain this props default-value)))
			  (create
			   value val
			   focused nil
			   completions (array)
			   selected-completion nil
			   valid (chain this props
					(validator
					 (chain this props (parser val)))))))
    completions-changed (lambda (new-completions)
			  (chain this (set-state (create completions new-completions)))
			  (when new-completions
			    (chain this (set-state (create selected-completion (elt new-completions 0))))))
    defocus (lambda ()
	      (chain this (set-state (create focused nil))))
    focus (lambda ()
	    (chain this (set-state (create focused t))))
    handle-keydown (lambda (ev)
		     (cond
		       ((= (chain ev key-code) 27)   ;Escape
			(when (chain this state focused)
			  (chain ev (prevent-default))
			  (chain this (defocus))))
		       ((or
			 (= (chain ev key-code) 13)  ;Enter
			 (and
			  (= (chain ev key-code) 9)  ;Tab
			  (chain this state focused)))
			(chain ev (prevent-default))
			(when (chain this state selected-completion)
			   (chain this
				  (handle-changed
				   (create
				    target (create
					    value
					    (chain this state selected-completion))))))
			   (chain this (defocus)))
		       ((= (chain ev key-code) 9)    ;Tab, completions not open
			;; Do nothing
			)
		       ((= (chain ev key-code) 40)   ;Down
			(chain ev (prevent-default))
			(let ((completions
			       (chain this state completions))
			      (selected-completion (chain this state selected-completion)))
			(when completions
			  (loop for i from 0 below (length completions)
			       when (= (elt completions i) selected-completion)
			     do (chain this (set-state
					     (create selected-completion
						     (elt completions
							  (mod (1+ i) (length completions))))))))))
		       ;;Up
		       ((= (chain ev key-code) 38)
			(chain ev (prevent-default))
			(let ((completions
			       (chain this state completions))
			      (selected-completion (chain this state selected-completion)))
			  (when completions
			    (loop for i from 0 below (length completions)
			       when (= (elt completions i) selected-completion)
			       do (chain this (set-state
					       (create selected-completion
						       (elt completions
							    (mod (+ i (1- (length completions)))
								 (length completions))))))))))
		       (t
			 (chain this (focus)))))
    handle-changed (lambda (ev)
		     (when (chain this timeout)
		       (chain window (clear-timeout (chain this timeout))))
		     (let* ((rawval (chain ev target value))
			    (val (chain this props (parser rawval)))
			    (fn
			     (tlambda ()
			       (chain this props (update-completions
						  rawval
						  (chain this completions-changed)))
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
    completion-clicked (lambda (item)
			 (tlambda (ev)
			   (chain ev (prevent-default))
			   (chain this (handle-changed (create
							target (create value item)
							dont-focus t)))
			   (chain this (defocus))))
    render (lambda ()
	     (htm
	      (:div
	       (:input :value ({ (chain this state value))
		       :id ({ (chain this props id))
		       :type ({(if (chain this props type)
				   (chain this props type)
				   "text"))
		       :disabled ({ *view-only*)
		       :on-change ({ (chain this handle-changed))
		       :on-key-down({ (chain this handle-keydown))
		       :class-name ({ (chain this props input-class))
		       :style ({
				(if (chain this state valid) 
				    (chain this props style)
				    (chain this props error-style))))
	       ({
		 (when (chain this state focused)
		   (htm
		    (:div
		     :style ({(create position :relative
				      width "100%" 
				      display :block
				      height 0))
		     (:div
		      :class-name "pure-menu positioned-popup-left"
		      (:ul :class-name "pure-menu-list"
			   ({(loop for item in (chain this state completions)
				collect
				  (htm
				   (:li
				    :class-name
				    ({(if (eql item (chain this state selected-completion))
					  "pure-menu-item pure-menu-selected"
					  "pure-menu-item"))
				    (:a
				     :href "#"
				     :class-name "pure-menu-link nopad"
				     :on-click ({ (chain this (completion-clicked item)))

				     :key ({ item)
				     ({ item))))))))))))))))

(defreact *validating-checkboxes
    get-initial-state (lambda ()
			(create value (chain this props default-value)))
    handle-changed (lambda (val)
		     (chain
			 (lambda (ev)
			   (let
			       ((value-copy
				 (if (chain ev target checked)
				     (chain this state value (push val))
				     (chain this state value
					    (delete
					     (chain this state value (index-of val)))))))
			     (chain this (set-state (create value value-copy)))
			     (when (chain this props (validator value-copy))
			       (chain this props (on-change value-copy)))))
		       (bind this)))
    render (lambda ()
	     (htm
	      (:fieldset
					;:class-name ({ (chain this props class-name))
	       ({(loop for item in (loopable (chain this props choices))
		    collect (htm
			     (:div :class-name ({(chain this props class-name))
				   :key ({ item)
				   (:input
				    :type :checkbox
				    :disabled ({ *view-only*)
				    :class-name "pure-checkbox"
					;:label ({ item)
				    :checked ({ (member item (chain this state value)))
				    :on-change ({ (chain this (handle-changed item))))
				   (:label ({ item))))))))))

(defreact *validating-select
    get-initial-state (lambda ()
			(create value (chain this props default-value)
				valid (chain this props
					     (validator (chain this props default-value)))))
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
	     (htm (:select
		   :disabled ({ *view-only*)
		   :value
		   ({(if (and (chain this state valid)
			      (ps:in 'override-value (chain this props)))
			 (chain this props override-value)
			 (chain this state value)))
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
			(create keys
				(loop for item in (chain this props value (to-array))
				   for i from 0
				   collect (chain this props (row-key item i)))))
    handle-change (mlambda (v)
		    (tlambda (newval)
		      (let ((newarray (chain this props value (set v newval))))
			;(chain this (set-state (create value newarray)))
			(chain this props (on-change newarray)))))
    del (lambda (the-key)
	  (tlambda (ev)
	      (chain ev (prevent-default))
	      (destructuring-bind (newitems newkeys)
		  (loop for item in (loopable (chain this props value))
		     for key in (chain this state keys)
		     unless (eql key the-key)
		     collect item into items and
		     collect key into keys
		     finally (return (ps:array items keys)))
		(chain this props (on-change (unloopable newitems)))
		(chain this (set-state (create keys newkeys))))))
    move-helper (lambda (item-position)
		  (let ((items (chain this props value))
			(keys (chain this state keys)))
		    (unless (<= item-position 0)
		      (chain this props
			     (on-change (chain
					 (chain items (slice 0 (1- item-position)))
					 (concat
					  (chain items (slice item-position (1+ item-position)))
					  (chain items (slice (1- item-position) item-position))
					  (chain items (slice (1+ item-position)))))))
		      (chain this (set-state
				   (create keys
					   (chain
					    (chain keys (slice 0 (1- item-position)))
					    (concat
					     (chain keys (slice item-position (1+ item-position)))
					     (chain keys (slice (1- item-position) item-position))
					     (chain keys (slice (1+ item-position)))))))))))
    down (lambda (the-key)
	   (tlambda (ev)
	     (chain ev (prevent-default))
	     (let ((item-position (index-of the-key (chain this state keys))))
	       (unless (>= item-position (1- (chain this state keys length)))
		 (chain this (move-helper (1+ item-position)))))))
    up (lambda (the-key)
	 (tlambda (ev)
	   (chain ev (prevent-default))
	   (let ((item-position (index-of the-key (chain this state keys))))
	     (chain this (move-helper item-position)))))

    add (lambda ()
	  (let* ((newitem (chain this props (make-new)))
		 (newarray (chain this props value (push newitem))))
	    (chain this (set-state
			 (create keys
				 (chain this state keys
					(concat
					 (ps:array
					  (chain
					   this props
					   (row-key
					    newitem
					    (1- (chain newarray (count)))))))))))
	    (chain this props (on-change newarray))))
    render (lambda ()
	     (let* ((keys
		     (if (or (chain this props read-only) *view-only*)
			 (loop for item in (chain this props value (to-array))
			    for i from 0
			    collect (chain this props (row-key item i)))
			 (chain this state keys)))
		    (items
		     (loop
			for item in (chain this props value (to-array))
			for key in keys
			for i from 0
			collect
			  (htm
			   (:div
			    :class-name ({ (chain this props inner-class))
			    :key ({ key)
			    ({(unless (or (chain this props read-only) *view-only*)
				(htm
				 (:div
				  (:div
				   :class-name "pure-u-1 pure-u-md-1-12"
				   (:button :class-name "pure-button"
					    :style ({(create margin 0 padding "0.25em"))
					    :on-click ({ (chain this (del key)))
					    (esc (string #\en_dash))))
				  (:div
				   :class-name "pure-u-1 pure-u-md-1-12"
				   (:button :class-name "pure-button"
					    :style ({(create margin 0 padding "0.25em"))
					    :on-click ({ (chain this (up key)))
					    (esc (string #\upwards_arrow))))
				  (:div
				   :class-name "pure-u-1 pure-u-md-1-12"
				   (:button :class-name "pure-button"
					    :style ({(create margin 0 padding "0.25em"))
					    :on-click ({ (chain this (down key)))
					    (esc (string #\downwards_arrow))))))))
			    ({ (chain this props (make-row item (chain this (handle-change i))))))))))
	       (htm
		(:fieldset
		 ({ items)
		 ({
		   (if (and
			(chain this props make-new)
			(not *view-only*))
		       (htm
			(:button :class-name "pure-button"
				 :on-click ({ (chain
					       (lambda (event)
						 (chain event (prevent-default))
						 (chain this (add))) (bind this)))
				 "+")))))))))

(defreact-for-classish (*weapon-info weapon-info
			       :on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (create obj (chain this props default-value)))
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
			  :parser ({ parse-float) :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field rng
			  :parser ({ (lambda (x) (when x (parse-int x))))
			  :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field shots
			  :parser ({ parse-int) :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field qualities :class-name "pure-u-1 pure-u-md-1-2")))))

		  
(defreact-for-classish (*gear-info gear-info
				   :on-change (tlambda (v field)
						(chain this props (on-change v))
						(if (= field "name")
						    (chain this (update-fields (aget :name v))))))
  get-initial-state (lambda () (create obj (chain this props default-value)))
  update-fields (lambda (name)
		  (post-data
		   (fixup-path "/complete/gear-info")
		   name
		   (let ((ginfo this))
		     :complete-callback
		     (lambda ()
		       (let ((response (chain *json* (parse (chain this response-text)))))
			 (when (> (chain response length) 0)
			   (setf response
				 (loop for item in response
				    collect (if (= item null) "" item)))
			   (funcall (chain ginfo (handle-change :effect)) (elt response 1))
			   (funcall (chain ginfo (handle-change :size)) (elt response 2))
			   (funcall (chain ginfo (handle-change :hand)) (elt response 3))
			   (funcall (chain ginfo (handle-change :weight)) (elt response 4))))))))
  render (lambda ()
	   (htm
	    (:div
	     (autocomplete-input-field
	      name
	      :update-completions
	      (lambda (val fn)
		(post-data
		 (fixup-path "/complete/gear")
		 val
		 :complete-callback
		 (lambda ()
		   (funcall fn
			    (chain *json* (parse (chain this response-text))))))):class-name "pure-u-1 pure-u-md-1-3")
	     (input-field effect
			  :override-value ({(aget :effect (chain this state obj)))
			  :class-name "pure-u-1 pure-u-md-1-3")
	     (choice-field size (:n :f :d :t :s :m :l :h :g :c :e :v)
			   :override-value ({(aget :size (chain this state obj)))
			   :choice-values ("n" "f" "d" "t" "s" "m" "l" "h" "g" "c" "e" "v")
			   :class-name "pure-u-1 pure-u-md-1-8")
	     (input-field hand
			  :override-value ({(aget :hand (chain this state obj)))
			  :class-name "pure-u-1 pure-u-md-1-12")
	     (input-field weight
			  :override-value ({(aget :weight (chain this state obj)))
			  :class-name "pure-u-1 pure-u-md-1-12"
			  :parser ({ #'parse-int))))))


	     
				 
(defreact-for-classish (*fc-class fc-class
				  :on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (create obj (chain this props default-value)))
  render (lambda ()
	   (htm
	    (:div
	     :class-name "pure-u-1 pure-u-md-11-12"
	     (autocomplete-input-field
	      the-class
	      :update-completions
	      (lambda (val fn)
		(post-data
		 (fixup-path "/complete/class")
		 val
		 :complete-callback
		 (lambda ()
		   (funcall fn
			    (chain *json* (parse (chain this response-text)))))))
	      :class-name "pure-u-1 pure-u-md-2-3"
	      :parser ({ to-keyword))
	     (input-field level
			  :class-name "pure-u-1 pure-u-md-1-4"
			  :input-type "number"
			  :parser ({ parse-int))))))



(defreact-for-classish (*spell-info spell-info
				    :on-change (tlambda (v field)
						 (chain this props (on-change v))
						 (if (= field "name")
						     (chain this (update-fields (aget :name v))))))
  get-initial-state (lambda () (create obj (chain this props default-value)))
  update-fields (lambda (name)
		  (post-data
		   (fixup-path "/complete/spell-info")
		   name
		   :complete-callback
		   (let ((spinfo this))
		     (lambda ()
		       (let ((response (chain *json* (parse (chain this response-text)))))
			 (when (> (chain response length) 0)
			   (setf response
				 (loop for item in response
				    collect (if (= item null) "" item)))
			   (funcall (chain spinfo (handle-change :level)) (elt response 1))
			   (funcall (chain spinfo (handle-change :discipline)) (elt response 2))
			   (funcall (chain spinfo (handle-change :casting-time)) (elt response 3))
			   (funcall (chain spinfo (handle-change :distance)) (elt response 4))
			   (funcall (chain spinfo (handle-change :area)) (elt response 5))
			   (funcall (chain spinfo (handle-change :saving-throw)) (elt response 6))
			   (funcall (chain spinfo (handle-change :duration)) (elt response 7))
			   (funcall (chain spinfo (handle-change :preparation-cost)) (elt response 8))
			   (funcall (chain spinfo (handle-change :effect)) (elt response 9))))))))
  render (lambda ()
	   (htm
	    (:div
	     :class-name nil
	     (autocomplete-input-field
	      name
	      :class-name "pure-u-1 pure-u-md-1-4"
	      :update-completions
	      (lambda (val fn)
		(post-data
		 (fixup-path "/complete/spell")
		 val
		 :complete-callback
		 (lambda ()
		   (funcall fn (chain *json* (parse (chain this response-text)))))))
	      :class-name "pure-u-1 pure-u-md-1-8")
	     (input-field level
			  :class-name "pure-u-1 pure-u-md-1-12"
			  :input-type "number"
			  :parser ({ parse-int))
	     (input-field discipline
			  :class-name "pure-u-1 pure-u-md-1-4"
			  :override-value ({(aget :discipline (chain this state obj))))
	     (input-field casting-time
			  :class-name "pure-u-1 pure-u-md-1-4"
			  :override-value ({(aget :casting-time (chain this state obj))))
	     (input-field distance
			  :class-name "pure-u-1 pure-u-md-1-4"
			  :override-value ({(aget :distance (chain this state obj))))
	     (input-field area
			  :class-name "pure-u-1 pure-u-md-1-4"
			  :override-value ({(aget :area (chain this state obj))))
	     (input-field saving-throw
			  :class-name "pure-u-1 pure-u-md-1-4"
			  :override-value ({(aget :saving-throw (chain this state obj))))
	     (input-field duration
			  :class-name "pure-u-1 pure-u-md-1-4"
			  :override-value ({(aget :duration (chain this state obj))))
	     (input-field preparation-cost
			  :class-name "pure-u-1 pure-u-md-1-4"
			  :override-value ({(aget :preparation-cost (chain this state obj))))
	     (input-field effect
			  :class-name "pure-u-1 pure-u-md-1-4"
			  :override-value ({(aget :effect (chain this state obj))))
	     ))))

(defreact-for-classish (*ability-info ability-info
				  :on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (create obj (chain this props default-value)))
  render (lambda ()
	   (htm
	    (:div :class-name nil
		  (:div
		   :class-name "pure-u-1-3"
		  ({ (+
		      (aget :name (chain this state obj))
		      " ("
		      (if (fc-class-p (aget :from (chain this state obj)))
			  (+
			   (string-capitalize
			    (aget :the-class (aget :from (chain this state obj))))
			   "/"
			   (aget :level (aget :from
					      (chain this state obj))))
			  (aget :from (chain this state obj)))
		      ")")))
		  #+(or)(input-field name
			       :class-name "pure-u-1-4")
		  (input-field parameter
			       :class-name "pure-u-1-4")
		  (choice-field list-as 
				(:combat :non-combat :spellcasting :none)
				:choice-values ("combat" "non-combat" "spellcasting" "none")
			       :class-name "pure-u-5-12")))))

(defreact-for-classish (*feat-info feat-info
				  :on-change (tlambda (v) (chain this props (on-change v))))
  get-initial-state (lambda () (create obj (chain this props default-value)))
  render (lambda ()
	   (htm
	    (:div :class-name nil
		  (autocomplete-input-field
		   name
		   :update-completions
		   (lambda (val fn)
		     (post-data
		      (fixup-path "/complete/feat")
		      val
		      :complete-callback
		      (lambda ()
			(funcall fn
				 (chain *json* (parse (chain this response-text)))))))
		   :class-name "pure-u-2-3")
		   (input-field parameter
				:class-name "pure-u-1-3")
		  (input-field notes
			       :class-name "pure-u-7-12")
		  (choice-field list-as 
				(:combat :non-combat :spellcasting)
				:choice-values ("combat" "non-combat" "spellcasting")
				:class-name "pure-u-5-12")))))
			
(defreact *character-menu
    mixins (ps:array (chain *react addons *pure-render-mixin))
    get-initial-state (lambda ()
			(create section "basics"))
    change-section (mlambda (section)
		     (tlambda (ev)
		       (chain ev (prevent-default))
		       (chain this (set-state (create section section)))))
    render
    (lambda ()
      (htm (:*character
	    :default-value ({(chain this props default-value))
	    :change-section ({ (chain this change-section))
	    :character-id ({(chain this props character-id))
	    :section ({(chain this state section))))))

(defreact-for-classish (*character fc-character
				   :on-change
				   (tlambda (newch) (chain this (update-abilities newch))))
  get-initial-state (lambda ()
		      (create obj (chain this props default-value)))
  on-fudge-change (mlambda (the-fudge)
		    (tlambda (newval)
		      (chain this
			     (set-state
			      (lambda (state props context)
				(let ((newobj (chain state obj (set-in (ps:array
									:fudges
									the-fudge)
								       newval))))
				  (chain this (update-abilities newobj))
				  (create obj newobj)))))))
  update-abilities (lambda (newch field) 
		     (unless (chain this upload-timer)
		       (setf (chain this upload-timer)
			     (chain window (set-timeout (chain this upload-data) 10000))))
		     (when (or t (eql field :classes))
		       (chain this
			      (set-state
			       (lambda (state props context)
				 (let ((new-obj (chain state obj
						       (set :ability-list
							    (fixup-abilities newch)))))
				   (chain this (set-state
						(create obj new-obj)))))))))
  upload-data (lambda ()
		(post-data
		 (+
		  (fixup-path "/save-character/")
		  (chain this props character-id))
		 (chain this state obj (to-j-s)))
		(setf (chain this upload-timer) nil))
  render (lambda ()
	   (htm
	    (:div
	     (:div
	      :class-name "pure-menu-horizontal"
	      (:ul :class-name "pure-menu-list"
		   (:li
		    :class-name ({(+ "pure-menu-item"
				     (if (= (chain this props section) "basics")
					 " pure-menu-selected" "")))
		    (:a :href "#" :class-name "pure-menu-link"
			:on-click ({(chain this props (change-section "basics")))
			"Basics"))
		   (:li
		    :class-name ({(+ "pure-menu-item"
				     (if (= (chain this props  section) "combat")
					 " pure-menu-selected" "")))
		    (:a :href "#" :class-name "pure-menu-link"
			:on-click ({(chain this props (change-section "combat")))
			"Combat"))
		   (:li
		    :class-name ({(+ "pure-menu-item"
				     (if (= (chain this props  section) "gear")
					 " pure-menu-selected" "")))
		    (:a :href "#" :class-name "pure-menu-link"
			:on-click ({(chain this props (change-section "gear")))
			"Gear"))
		   (:li
		    :class-name ({(+ "pure-menu-item"
				     (if (= (chain this props  section) "spell")
					 " pure-menu-selected" "")))
		    (:a :href "#" :class-name "pure-menu-link"
			:on-click ({(chain this props (change-section "spell")))
			"Spellcasting"))
		   (:li
		    :class-name ({(+ "pure-menu-item"
				     (if (= (chain this props  section) "bio")
					 " pure-menu-selected" "")))
		    (:a :href "#" :class-name "pure-menu-link"
			:on-click ({(chain this props (change-section "bio")))
			"Bio/Notes"))
		   (:li
		    :class-name "pure-menu-item"
		    (:a :href ({(+ (fixup-path "/pdf-character/")
				   (chain this props character-id)))
			:target "_blank"
			:class-name "pure-menu-link"
			"PDF"))
		   (:li
		    :class-name "pure-menu-item"
		    (:a :href ({(fixup-path "/")) :class-name "pure-menu-link"
			:on-click ({(tlambda ()
				      (when
					  (chain this upload-timer)
					(chain this (upload-data)))))
			"Save & Exit")))) 
	     ({(cond
		 ((= (chain this props section) "basics")
		  (htm
		   (:form :key "basics"
			  :class-name "pure-form pure-form-stacked"
			  (:div
			   :class-name "pure-g"
			   (:div
			    :class-name "pure-u-1 pure-u-md-3-4 pure-u-xl-3-8 pure-g"
			    (input-field character-name
					 :class-name "pure-u-1 pure-u-md-1-4")
			    (autocomplete-input-field species
				:update-completions
			     (lambda (val fn)
			       (post-data
				(fixup-path "/complete/species")
				val
				:complete-callback
				(lambda ()
				  (funcall fn
					   (chain *json* (parse (chain this response-text)))))))
					 :class-name "pure-u-1 pure-u-md-1-8")
			    (autocomplete-input-field
			     talent
				:update-completions
			     (lambda (val fn)
			       (post-data
				(fixup-path "/complete/talent")
				val
				:complete-callback
				(lambda ()
				  (funcall fn
					   (chain *json* (parse (chain this response-text)))))))
					 :class-name "pure-u-1 pure-u-md-1-8"
					 )
			    (autocomplete-input-field
			     specialty
			     :update-completions
			     (lambda (val fn)
			       (post-data
				(fixup-path "/complete/specialty")
				val
				:complete-callback
				(lambda ()
				  (funcall fn
					   (chain *json* (parse (chain this response-text)))))))
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
				 (output-field attr-points :show-label nil))))
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
			    (output-field total-studies)
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
			    (list-field studies
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
		 ((= (chain this props section) "combat")
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
			    (output-field defense-armor-mod :label-as "Def"
					  :class-name "pure-u-1-6")
			    (output-field defense-attr-mod :label-as "Attr"
					  :class-name "pure-u-1-6")
			    (output-field defense-size-mod :label-as "Size"
					  :class-name "pure-u-1-6")
			    (output-field defense-misc-mod :label-as "Misc"
					  :class-name "pure-u-1-6")
			    (output-field vitality :class-name "pure-u-1-3")
			    (output-field wounds :class-name "pure-u-1-3")
			    (output-field dr :class-name "pure-u-1-3")
			    (input-field critical-injuries :class-name "pure-u-1")
			    (:h2
			     :class-name "heading pure-u-1"
			     "Size")
			    (output-field size :class-name "pure-u-1-3")
			    (output-field footprint :class-name "pure-u-1-3")
			    (output-field reach :class-name "pure-u-1-3")
			    (output-field ground-speed :class-name "pure-u-1-3")
			    (output-field run-speed :class-name "pure-u-1-3")
			    ;;(:div :class-name "pure-u-1-3") ;TODO Insert other speed
			    (output-field travel-speed :class-name "pure-u-1-3")
			    (checkboxes-field proficiency-list
					      ({ +proficiencies+)
					      :class-name "pure-u-1"
					      :checkbox-class "pure-u-1-4")
			    (checkboxes-field forte-list
					      ({ +proficiencies+)
					      :class-name "pure-u-1"
					      :checkbox-class "pure-u-1-4")
			    (:h3 :class-name "heading pure-u-1"
				 "Tricks")
			    (list-field tricks
					(lambda (data updater)
					  (htm (:*validating-input
						:default-value ({ data)
						:error-style ({(create background-color "pink"
								       padding 0))
						:style ({(create background-color "white"
								 padding 0))
						:validator ({ #'stringp)
						:parser ({(lambda (x) x))
						:on-change ({ updater))))
					:show-name nil
					:class-name "pure-u-1 pure-u-xl-1-2 pure-g"
					;:row-key (lambda (x) (aget :the-class x))
					:inner-class "pure-u-1 pure-g"
					:make-new (lambda () "")))
				       
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
			    (input-field armor-dr
					 :class-name "pure-u-1 pure-u-md-1-6"
					 :parser ({ #'parse-int))
			    (input-field armor-dp
					 :class-name "pure-u-1 pure-u-md-1-6"
					 :parser ({ #'parse-int))
			    (input-field armor-acp
					 :class-name "pure-u-1 pure-u-md-1-6"
					 :parser ({ #'parse-int))
			    (input-field armor-speed
					 :class-name "pure-u-1 pure-u-md-1-6"
					 :parser ({ #'parse-int))
			    (input-field armor-weight
					 :class-name "pure-u-1 pure-u-md-1-6"
					 :parser ({ #'parse-int))
			    (input-field armor-disguise
					 :class-name "pure-u-1 pure-u-md-1-6"
					 :parser ({ #'parse-int))
			    (input-field armor-craftsmanship
					 :class-name "pure-u-1 pure-u-md-1-4")
			    (input-field armor-construction
					 :class-name "pure-u-1 pure-u-md-1-4")
			    (input-field armor-customizations
					 :class-name "pure-u-1 pure-u-md-5-6"
					 :formatter (lambda (x)
						      (chain x (join ", ")))
					 :parser ({(lambda (x)
						     (unloopable
						     (chain x (split (ps:new (*reg-exp ", *"))))))))
			    (choice-field armor-fittings
					  ("none" "light" "heavy"))
			    )
			   (:div
			    :class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
			    (output-field action-attack)
			    (output-field action-bullrush)
			    (output-field action-coupe-de-grace)
			    (output-field action-disarm)
			    (output-field action-feint)
			    (output-field action-grapple)
			    (output-field action-pummel)
			    (output-field action-taunt)
			    (output-field action-threaten)
			    (output-field action-tire)
			    (output-field action-trip)
			    )
			   ))))
		 ((= (chain this props section) "gear")
		  (htm
		   (:form
		    :key "gear"
		    :class-name "pure-form pure-form-stacked"
		    (:div
		     :class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
		     (:h2 :class-name "heading pure-u-1"
			  "Carrying Capacity")
		     (output-field light-capacity)
		     (output-field heavy-capacity)
		     (output-field lift)
		     (output-field push/drag))
		    (:div
		     :class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
		     (:h2 :class-name "heading pure-u-1"
			  "Reputation and Renown")
		     (output-field legend)
		     (input-field reputation
				  :parser ({ #'parse-int))
		     (output-field renown)
		     (input-field heroic-renown
				  :parser ({ #'parse-int))
		     (input-field military-renown
				  :parser ({ #'parse-int))
		     (input-field noble-renown
				  :parser ({ #'parse-int)))
		     (:h2 :class-name "heading pure-u-1 pure-u-xl-1-2"
			  "Gear")
		    (list-field gear
				(lambda (data updater)
				  (htm (:*gear-info
					:default-value ({ data)
					:on-change ({ updater))))
				:show-name nil
				:class-name "pure-u-1 pure-u-xl-1-2 pure-g"
					;:row-key (lambda (x) (aget :the-class x))
				:inner-class "pure-u-1 pure-g"
				:make-new #'make-gear-info)
		    (:div
		     :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
		     (:h2 :class-name "heading pure-u-1"
			  "Mount")
		     (input-field mount-name
				  :label-as "Name"
				  :class-name "pure-u-1 pure-u-md-1-4")
		     (input-field mount-size
				  :label-as "Size"
				  :class-name "pure-u-1 pure-u-md-1-12")
		     (input-field mount-footprint
				  :label-as "Footprint"
				  :class-name "pure-u-1 pure-u-md-1-12")
		     (input-field mount-reach
				  :label-as "Reach"
				  :class-name "pure-u-1 pure-u-md-1-12")
		     (input-field mount-speed
				  :label-as "Speed"
				  :class-name "pure-u-1 pure-u-md-1-12")
		     (input-field mount-travel
				  :label-as "Travel"
				  :class-name "pure-u-1 pure-u-md-1-12")
		     (input-field mount-init
				  :class-name "pure-u-1 pure-u-md-1-12"
				  :label-as "Init")
		     (input-field mount-atk
				  :class-name "pure-u-1 pure-u-md-1-12"
				  :label-as "Atk")
		     (input-field mount-def
				  :class-name "pure-u-1 pure-u-md-1-12"
				  :label-as "Def")
		     (input-field mount-res
				  :class-name "pure-u-1 pure-u-md-1-12"
				  :label-as "Res")
		     (input-field mount-attributes
				  :label-as "Attributes"
				  :class-name "pure-u-1 pure-u-md-1-2")
		     (input-field mount-health
				  :label-as "Health"
				  :class-name "pure-u-1 pure-u-md-1-12"
				  )
		     (input-field mount-comp
				  :label-as "Comp"
				  :class-name "pure-u-1 pure-u-md-1-12"
				  )
		     (input-field mount-skills
				  :label-as "Skills"
				  :class-name "pure-u-1")
		     (input-field mount-qualities
				  :label-as "Qualities"
				  :class-name "pure-u-1")
		     (input-field mount-attacks
				  :label-as "Attacks"
				  :class-name "pure-u-1")
				  )

		    (:div
		     :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
		     (:h2 :class-name "heading pure-u-1"
			  "Vehicle")
		     (input-field vehicle-name
				  :label-as "Name"
				  :class-name "pure-u-1 pure-u-md-1-3")
		     (input-field vehicle-speed
				  :label-as "Speed"
				  :class-name "pure-u-1 pure-u-md-1-12")
		     (input-field vehicle-travel
				  :label-as "Travel"
				  :class-name "pure-u-1 pure-u-md-1-12")
		     (input-field vehicle-size
				  :label-as "Size"
				  :class-name "pure-u-1 pure-u-md-1-8")
		     (input-field vehicle-def
				  :label-as "Def"
				  :class-name "pure-u-1 pure-u-md-1-12")
		     (input-field vehicle-occupancy
				  :label-as "Occ"
				  :class-name "pure-u-1 pure-u-md-1-12")
		     (input-field vehicle-load
				  :label-as "Load"
				  :class-name "pure-u-1 pure-u-md-1-12")
		     (input-field vehicle-const
				  :label-as "Const"
				  :class-name "pure-u-1 pure-u-md-1-8")
		     (input-field vehicle-qualities
				  :label-as "Qualities"
				  :class-name "pure-u-1 pure-u-md-1"))
		    (:div
		     :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
		     (:h2 :class-name "heading pure-u-1"
			  "Contacts")
		    (list-field contacts
				(lambda (data updater)
				  (htm (:*contact-info
					:default-value ({ data)
					:on-change ({ updater))))
				:show-name nil
				:class-name "pure-u-1 pure-g"
				:inner-class "pure-u-1 pure-u-md-1-2 pure-g"
				:make-new #'make-contact-info
				))
		    (:div
		     :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
		     (:h2 :class-name "heading pure-u-1"
			  "Holdings")
		     (list-field holdings
				 (lambda (data updater)
				   (htm (:*holding-info
					 :default-value ({ data)
					 :on-change ({ updater))))
				 :show-name nil
				 :class-name "pure-u-1 pure-g"
				 :inner-class "pure-u-1 pure-u-md-1-2 pure-g"
				 :make-new #'make-holding-info
				 ))
		    (:div
		     :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
		     (:h2 :class-name "heading pure-u-1"
			  "Magic Items")
		     (list-field magic-items
				 (lambda (data updater)
				   (htm (:*magic-item-info
					 :default-value ({ data)
					 :on-change ({ updater))))
				 :show-name nil
				 :class-name "pure-u-1 pure-g"
				 :inner-class "pure-u-1 pure-g"
				 :make-new #'make-magic-item-info
				 ))
		    )))
		 ((= (chain this props section) "spell")
		  (htm
		   (:form
		    :key "spell"
		    :class-name "pure-form pure-form-stacked"
		    (:div
		     :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
		     (input-field casting-level
				  :parser ({ #'parse-int))
		     (input-field spell-points
				  :parser ({ #'parse-int)))
		    (:div
		     :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
		     (output-field spellcasting-total
				   :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8")
		     (input-field spellcasting-ranks
				  :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8"
				  :parser ({ #'parse-int))
		     (output-field int-mod
				   :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8")
		     (output-field spellcasting-misc-mod
				   :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8"))
		    (:div
		     :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
		     (output-field spells-known
				   :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8")
		     (output-field real-wis
				   :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8")
		     (output-field spells-known-misc-mod
				   :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8"))
		    (:div
		     :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
		     (output-field save-dc
				   :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8")
		     (output-field cha-mod
				   :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8")
		     (input-field spellcasting-feats
				  :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8"
				  :parser ({ #'parse-int)))
		    (list-field spells
				(lambda (data updater)
				  (htm (:*spell-info
					:default-value ({ data)
					:on-change ({ updater))))
				:class-name "pure-u-1 pure-g"
					;:row-key (lambda (x) (aget :the-class x))
				:inner-class "pure-u-1 pure-u-xl-1-2 pure-g"
				:make-new #'make-spell-info
				))))
		 ((= (chain this props section) "bio")
		  (htm (:form
			:class-name "pure-form pure-g"
			(:h2 :class-name "heading pure-u-1"
			     "Biography")
			(:div :class-name "pure-u-1-12")
			(:div
			 :class-name "pure-u-5-6"
			 (textarea-field
			  biography
			  :rows 30
			  :class-name "pure-input-1"))
			(:div :class-name "pure-u-1-12")
			(:h2 :class-name "heading pure-u-1"
			     "Notes")
			(:div :class-name "pure-u-1-12")
			(:div
			 :class-name "pure-u-5-6"
			 (textarea-field
			  notes
			  :rows 30
			  :class-name "pure-input-1"))
			(:div :class-name "pure-u-1-12"))))
		 ))))))

(defreact *login
    mixins (ps:array (chain *react addons *pure-render-mixin))
    get-initial-state (lambda ()
			(create username ""
				password ""
				state :initial))
    handle-changed (lambda (fieldname)
		     (tlambda (event)
		       (chain event (prevent-default))
		       (let ((updater (create)))
			 (setf (getprop updater fieldname)
			       (chain event target value)))
		       (chain this (set-state updater))))
    render (lambda ()
	     (htm
	      (:form
	       :action "."
	       :method :post
	       :class-name "pure-form"
	       (:label :html-for "username" "Username")
	       (:input :id "username" :class-name "pure-input"
			:name "username"
			:value ({ (chain this state username))
			:on-change ({ (chain this (handle-changed "username"))))
	       (:label :html-for "password" "Password")
	       (:input :id "password" :class-name "pure-input"
		       :type "password"
		       :name "password"
		       :on-change ({ (chain this (handle-changed "password")))
		       :value ({ (chain this state password)))
	       (:input
		:type :submit
		:value "Login"
		:class-name "pure-button")))))

(defreact *account-settings
    mixins (ps:array (chain *react addons *pure-render-mixin))
    render (lambda ()
	     (htm
	      (:div
	       (:*logout)
	       (:*change-password)))))

(defreact *logout
    mixins (ps:array (chain *react addons *pure-render-mixin))
    render (lambda ()
	     (htm
	      (:form
	       :action ({(fixup-path "/logout/"))
	       :method :post
	       :class-name "pure-form"
	       (:input
		:type :hidden
		:name "csrf-token"
		:value ({(chain document (get-element-by-id "csrf") inner-h-t-m-l (trim))))
	       (:input
		:type "submit"
		:class-name "pure-button"
		:value "Logout")))))

    

(defreact *change-password
    mixins (ps:array (chain *react addons *pure-render-mixin))
    get-initial-state (lambda ()
			(create password1 ""
				password2 ""
				state :initial))
    handle-changed (lambda (fieldname)
		     (tlambda (event)
		       (chain event (prevent-default))
		       (let ((updater (create)))
			 (setf (getprop updater fieldname)
			       (chain event target value)))
		       (chain this (set-state updater))))
    render (lambda ()
	     (htm
	      (:form
	       :class-name "pure-form"
		(:legend "Password")
		(:label :html-for "password1" "New Password")
		(:input :id "password1" :class-name "pure-input"
			:type "password"
			:value ({ (chain this state password1))
			:on-change ({ (chain this (handle-changed "password1"))))
		(:label :html-for "password2" "Confirm Password")
		(:input :id "password2" :class-name "pure-input"
			:type "password"
			:on-change ({ (chain this (handle-changed "password2")))
			:value ({ (chain this state password2)))
		(:button
		 :class-name "pure-button"
		 :on-click ({(tlambda (event)
			       (chain event (prevent-default))
			       (if (= (chain this state password1)
				      (chain this state password2))
				   (progn
				     (chain this (set-state (create state :sent)))
				     (post-data (fixup-path "/set-password/")
						(chain this state password1)
						:raw t
						:complete-callback
						(tlambda ()
						  (chain
						   this
						   (set-state (create state :success))))))
				   (alert "Passwords must match")))))
		({(cond
		    ((eql (chain this state state) :initial) nil)
		    ((eql (chain this state state) :sent)
		     (htm (:p "In-progress...")))
		    ((eql (chain this state state) :success)
		     (htm (:p "Success!")))
		    ;;TODO error
		    ))))))

(defreact *character-list
    mixins (ps:array (chain *react addons *pure-render-mixin))
    edit (lambda (id)
	   (lambda ()
	     (setf (chain window location)
		   (+ (fixup-path "/character/") id))))
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
		   ({(loop for item in (loopable (chain ths props value))
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
		  (:input
		   :type :hidden
		   :name "csrf-token"
		   :read-only t
		   :value ({(chain document (get-element-by-id "csrf") inner-h-t-m-l (trim))))
		  (:button
		   :form-action ({(fixup-path "/new-character/"))
		   :form-method "POST"
		   :class-name "pure-button"
		   "New"))
		 (:p
		  (:a :href ({(fixup-path "/account/"))
		      "Account Settings"))
		 (:p
		  (:a :href "https://phab.jasom.org/"
		      "Bugs/Feature requests")))))))

;(chain *react (render (htm :*Test) (chain document body)))
;(chain *react (render (htm (:*Character)) (chain document body)))
;; Keep session alive
(set-interval (lambda ()
		(post-data (fixup-path "/dummy/") ""))
	      (* 60 60 1000))
