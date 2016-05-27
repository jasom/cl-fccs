(in-package cl-fccs)

(defvar *view-only* nil)

(defmacro constructor (args &body b)
  `(lambda ,args
     ,@b
     this))

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

(defmithril-for-classish (*contact-info
			contact-info
			:onchange (tlambda (v) (chain props (onchange v))))
    (setf (^ this obj) (^ m (prop (^ props default-value))))
  view (lambda (c props children)
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


(defmithril-for-classish (*holding-info
			holding-info
			:onchange (tlambda (v) (chain props (onchange v))))
    (setf (^ this obj)
	  (^ m (prop (^ props default-value))))
  view (lambda (c props children)
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

(defmithril-for-classish (*magic-item-info
			magic-item-info
			:onchange (tlambda (v) (chain props (onchange v))))
    (setf (^ this obj)
	  (^ m (prop (^ props default-value))))
  view (lambda (c props children)
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

(defmithril-for-classish (*fudge fudge
			       :onchange (tlambda (v) (chain props (onchange v))))
  (setf (^ this obj) (^ m (prop (^ props default-value))))
  view (lambda (c props children)
	   (htm
	    (:div
	     :class-name "pure-u-1"
	     (input-field value
			  :parser  parse-int
			  :class-name "pure-u-1-2")
	     (input-field notes
			  :class-name "pure-u-1-2")))))
(defun xhr-config (request)
  (chain request (set-request-header "Content-Type" "application/json; charset=utf-8"))
  (chain request (set-request-header "x-csrf-token"
				     (chain document (get-element-by-id "csrf") inner-h-t-m-l (trim)))))


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
     (chain request (send encoded-data)))))

(defun get-h-offset (element)
  (let* ((rect (chain element (get-bounding-client-rect)))
	 (brect (chain document body (get-bounding-client-rect))))
    (- (chain rect left) (chain brect left))))


(defmithril *fudgable-field
  controller
  (constructor ()
    (setf
     (chain this popup) (chain m (prop nil))
     (chain this handle-click)
     (tlambda (ev)
       (chain ev (prevent-default))
       (when (> (chain ev detail) 1)
	 (chain this
		(popup
		 (if (> (get-h-offset (chain ev current-target))
			200)
		     :right
		     :left)))))))
  view (lambda (c props children)
         (htm
	  (:div
	   :class-name (chain props class-name)
	   :style (create margin 0)
           (:div
             :style (create padding 0)
             :onclick (chain c handle-click)
             :class-name (chain props input-class)
             (chain props value)
             (when (chain c (popup))
               (htm
                 (:div
                   :style (create position :relative
                                  width "100%" 
                                  display :block
                                  height 0)
                   (:div
                     :class-name
                     (if (eql (^ c popup) :left)
                       "positioned-popup-left"
                       "positioned-popup-right")
                     (:*val-list
                       :value (chain props default-fudge)
                       :validator (chain props validate-fudge)
                       :onchange (chain props fudge-changed)
                       :row-key (lambda () (genid))
                       :make-row (lambda (data updater)
                                   (htm (:*fudge
                                          :default-value  data
                                          :onchange  updater)))
                       :make-new (chain
                                   (lambda ()
                                     (make-fudge :field
                                                 (to-keyword
                                                   (chain props name))))
                                   (bind this)))

                     (:button
                       :class-name "pure-button"
                       :onclick (chain
                                  (lambda (event)
                                    (chain event (prevent-default))
                                    (chain c (popup nil))))
                       "Close"))))))
	        (when (chain props show-label)
		 (htm (:label (better-capitalize
				 (or (chain props label-as)
				     (chain props name))))))))))

(defmithril *validating-checkbox
    controller
    (constructor (props children)
      (setf (chain this value)
            (chain m (prop (chain props default-value)))
            (chain this handle-changed)
            (tlambda (ev)
              (let ((val (chain ev target checked)))
                (chain this (value val))
                (when (chain props (validator val))
                  (chain props (onchange val)))))))
    view (lambda (c props children)
	  (htm (:input :checked (chain c (value))
                       :id  (chain props id)
                       :type :checkbox
                       :disabled  *view-only*
                       :oninput  (chain c handle-changed)
                       :class-name  (chain props input-class)
                       :style 
                       (if (chain props
                                  (validator
                                    (chain c (value))))
                         (chain props style)
                         (chain props error-style))))))

(defmithril *validating-input
    controller
    (constructor (props children)
       (setf (^ this value)
             (^ m (prop (chain props default-value)))
             (^ this valid)
             (^ m (prop
		   (^ props (validator
			     (^ props
				(parser (chain props default-value)))))))
             (^ this handle-changed)
             (tlambda (ev)
               (if (chain this timeout)
                 (chain window (clear-timeout (chain this timeout)))
                 (^ m (start-computation)))
               (let* ((rawval (chain ev target value))
                      (val (chain props (parser rawval)))
                      (fn
                        (tlambda ()
                                 (if (chain props (validator val))
                                   (progn
                                     (chain props (onchange val))
                                     (chain this (valid t)))
                                   (chain this (valid nil)))
                                 (^ m (end-computation))))
                      (timeout (chain window (set-timeout fn 250))))
                 (chain this (value rawval))
                 (setf (chain this timeout) timeout)))))
       view (tlambda (c props children)
	     (htm (:input :value  (if (and (^ c (valid))
                                           (ps:in 'override-value (chain props)))
                                    (chain props override-value)
                                    (chain c (value)))
                          :id  (chain props id)
                          :type (if (chain props type)
                                  (chain props type)
                                  "text")
                          :oninput  (chain c handle-changed)
                          :class-name  (chain props input-class)
                          :disabled  *view-only*
                          :style 
                          (if (chain c (valid))
                            (chain props style)
                            (chain props error-style))))))

(defmithril *validating-autocomplete
    controller
    (constructor (props children)
                 (let ((val (chain props default-value)))
                   (setf
                     (^ this pending-autocomplete) (^ m (prop nil))
                     (^ this value) (^ m (prop val))
                     (^ this focused) (^ m (prop nil))
                     (^ this completions) (^ m (prop (array)))
                     (^ this selected-completion) (^ m (prop nil))
                     (^ this valid) (^ m (prop
                                           (^ props (validator (^ props (parser val))))))
                     (^ this rawval) (^ m (prop nil))
                     (^ this completions-changed)
                     (tlambda (new-completions)
                              (if (/= (chain this (pending-autocomplete))
                                      (chain this (rawval)))
                                (progn
                                  (chain this (pending-autocomplete (chain this (rawval))))
                                  (chain props
                                         (update-completions
                                           (chain this (rawval))
                                           (chain this (completions-changed)))))
                                (chain this (pending-autocomplete nil)))
                              (chain this (completions new-completions))
                              (chain this (selected-completion (elt new-completions 0)))
			      (^ m (start-computation))
                              (^ m (end-computation)))
                     (^ this defocus) (tlambda () (chain this (focused nil)))
                     (^ this focus) (tlambda () (chain this (focused t)))
                     (^ this handle-keydown)
                     (tlambda (ev)
                              (cond
                                ((= (chain ev key-code) 27)   ;Escape
                                 (when (chain this (focused))
                                   (chain ev (prevent-default))
                                   (chain this (defocus))))
                                ((or
                                   (= (chain ev key-code) 13)  ;Enter
                                   (and
                                     (= (chain ev key-code) 9)  ;Tab
                                     (chain this (focused))))
                                 (chain ev (prevent-default))
                                 (when (^ this (selected-completion))
                                   (chain this (handle-changed (^ this (selected-completion)) t)))
				 (chain this (defocus)))
                              ((= (chain ev key-code) 9)    ;Tab, completions not open
                               ;; Do nothing
                               )
                              ((and
                                 (= (chain ev key-code) 40)   ;Down
                                 (chain this (focused)))
                               (chain ev (prevent-default))
                               (let ((completions
                                       (chain this (completions)))
                                     (selected-completion (chain this (selected-completion))))
                                 (when completions
                                   (loop for i from 0 below (length completions)
                                         when (= (elt completions i) selected-completion)
                                         do (chain this (selected-completion
                                                          (elt completions
                                                               (mod (1+ i) (length completions)))))))))
                              ;;Up
                              ((and
                                 (= (chain ev key-code) 38)
                                 (chain this (focused)))
                               (chain ev (prevent-default))
                               (let ((completions
                                       (chain this completions))
                                     (selected-completion (chain this (selected-completion)))
                                     (when completions
                                       (loop for i from 0 below (length completions)
                                             when (= (elt completions i) selected-completion)
                                             do (chain this (selected-completion
                                                              (elt completions
                                                                   (mod (+ i (1- (length completions)))
                                                                        (length completions))))))))))
                              (t
                                (chain this (focus)))))
		     (^ this handle-changed)
		     (tlambda (rawval &optional no-autocomplete)
		       (let* ((val (chain props (parser rawval))))
			 (chain this (rawval rawval))
			 (chain this (value rawval))
			 (unless (or (chain this (pending-autocomplete))
				     no-autocomplete
				     (not rawval))
			   (chain this (pending-autocomplete rawval))
			   (chain props (update-completions
					 rawval
					 (chain this completions-changed))))
			 (if (chain props (validator val))
			     (progn
			       (chain props (onchange val))
			       (chain this (valid t)))
			     (chain this (valid nil)))))
                   (^ this completion-clicked)
                   (tlambda (item)
                            (tlambda (ev)
                                     (chain ev (prevent-default))
                                     (chain this (handle-changed item))
                                     (chain this (defocus)))))))
    view
    (lambda (c props children)
      (htm
        (:div
          (:input :value  (^ c (value))
                  :id  (chain props id)
                  :type (if (chain props type)
                          (chain props type)
                          "text")
                  :disabled  *view-only*
                  :oninput  (^ m (with-attr :value (^ c handle-changed) c))
                  :onkeydown (chain c handle-keydown)
                  :class-name  (chain props input-class)
                  :style (if (chain c (valid))
                           (chain props style)
                           (chain props error-style)))
          (when (and
                  (chain c (focused))
                  (/= 0 (length (chain c (completions)))))
            (htm
              (:div
                :style (create position :relative
                               width "100%" 
                               display :block
                               height 0)
                (:div
                  :class-name "pure-menu positioned-popup-left"
                  (:ul :class-name "pure-menu-list"
                       (loop for item in (chain c (completions))
                             collect
                             (htm
                               (:li
                                 :class-name
                                 (if (eql item (chain c (selected-completion)))
                                   "pure-menu-item pure-menu-selected"
                                   "pure-menu-item")
                                 :key item
                                 (:a
                                   :href "#"
                                   :class-name "pure-menu-link nopad"
                                   :onclick  (chain c (completion-clicked item))
                                   item)))))))))))))

(defmithril *validating-checkboxes
    controller
    (constructor (props children)
      (setf
        (^ this value) (^ m (prop (^ props default-value)))
        (^ this handle-changed)
        (tlambda (val)
          (tlambda (ev)
             (let
               ((value-copy
                  (if (chain ev target checked)
                    (chain this (value) (push val))
                    (chain this (value)
                           (delete
                             (chain this (value) (index-of val)))))))
               (chain this (value value-copy))
               (when (chain props (validator value-copy))
                 (chain props (onchange value-copy))))))))
    view (lambda (c props children)
           (htm
             (:fieldset
               ;:class-name  (chain props class-name)
               (loop for item in (loopable (chain props choices))
                     collect (htm
                               (:div :class-name (chain props class-name)
                                     :key  item
                                     (:input
                                       :type :checkbox
                                       :disabled  *view-only*
                                       :class-name "pure-checkbox"
                                       ;:label  item
                                       :checked  (member item (chain c (value)))
                                       :oninput  (chain c (handle-changed item)))
                                     (:label  item))))))))

(defmithril *validating-select
    controller
  (constructor (props children)
	       (setf
		(^ this value) (^ m (prop (^ props default-value)))
		(^ this valid) (^ m (prop (^ props (validator (^ props default-avlue)))))
		(^ this handle-changed)
		(tlambda (rawval)
		  (if (chain this timeout)
		      (chain window (clear-timeout (chain this timeout)))
		      (^ m (start-computation)))
		  (let* ((val (chain props (parser rawval)))
			 (fn (tlambda ()
			       (if (chain props (validator val))
				   (progn
				     (chain props (onchange val))
				     (chain this (valid t)))
				   (chain this (valid nil)))
			       (^ m (end-computation))))
			 (timeout (chain window (set-timeout fn 250))))
		    (chain this (value rawval))
		    (setf (chain this timeout) timeout)))))
  view (tlambda (c props children)
                   (htm (:select
                          :disabled  *view-only*
                          :value
                          (if (and (chain c valid)
                                   (ps:in 'override-value (chain props)))
                            (chain props override-value)
                            (chain c (value)))
                          :class-name  (chain props class-name)
                          :onchange  (^ m (with-attr :value (^ c handle-changed)))
                          :id  (chain props id)
                          :style 
                          (if (chain props (validator (chain c (value))))
                            (chain props style)
                            (chain props error-style))
                          children))))

(defmithril *val-list
    controller
    (constructor (props children)
      (setf
        (^ this keys)
        (^ m (prop
               (loop for item in (chain props value (to-array))
                     for i from 0
                     collect (chain props (row-key item i)))))
        (^ this handle-change)
        (mlambda (v)
          (tlambda (newval)
            (let ((newarray (chain props value (set v newval))))
              (chain props (onchange newarray)))))
        (^ this del)
        (tlambda (the-key)
	  (tlambda (ev)
            (chain ev (prevent-default))
            (destructuring-bind (newitems newkeys)
              (loop for item in (loopable (chain props value))
                    for key in (chain this (keys))
                    unless (eql key the-key)
                    collect item into items and
                    collect key into keys
                    finally (return (ps:array items keys)))
              (chain props (onchange (unloopable newitems)))
              (chain this (keys newkeys)))))
        (^ this move-helper)
        (tlambda (item-position)
          (let ((items (chain props value))
                (keys (chain this (keys))))
            (unless (<= item-position 0)
              (chain props
                     (onchange (chain
                                 (chain items (slice 0 (1- item-position)))
                                 (concat
                                   (chain items (slice item-position (1+ item-position)))
                                   (chain items (slice (1- item-position) item-position))
                                   (chain items (slice (1+ item-position)))))))
              (chain this (keys
                            (chain
                              (chain keys (slice 0 (1- item-position)))
                              (concat
                                (chain keys (slice item-position (1+ item-position)))
                                (chain keys (slice (1- item-position) item-position))
                                (chain keys (slice (1+ item-position))))))))))
        (^ this down)
        (tlambda (the-key)
	   (tlambda (ev)
	     (chain ev (prevent-default))
	     (let ((item-position (index-of the-key (chain (keys)))))
	       (unless (>= item-position (1- (chain this (keys) length)))
		 (chain this (move-helper (1+ item-position)))))))
        (^ this up)
        (tlambda (the-key)
	 (tlambda (ev)
	   (chain ev (prevent-default))
	   (let ((item-position (index-of the-key (chain this (keys)))))
	     (chain this (move-helper item-position)))))

        (^ this add)
        (tlambda (props)
	  (let* ((newitem (chain props (make-new)))
		 (newarray (chain props value (push newitem))))
	    (chain this (keys
                          (chain this (keys)
                                 (concat (ps:array
                                           (chain
                                             props
                                             (row-key
                                               newitem
                                               (1- (chain newarray (count))))))))))
            (chain props (onchange newarray))))))
    view (lambda (c props children)
	     (let* ((keys
		     (if (or (chain props read-only) *view-only*)
			 (loop for item in (chain props value (to-array))
			    for i from 0
			    collect (chain props (row-key item i)))
			 (chain c (keys))))
		    (items
		     (loop
			for item in (chain props value (to-array))
			for key in keys
			for i from 0
			collect
			  (htm
			   (:div
			    :class-name (or (chain props inner-class) nil)
			    :key  key
			    (unless (or (chain props read-only) *view-only*)
				(htm
				 (:div
				  (:div
				   :class-name "pure-u-1 pure-u-md-1-12"
				   (:button :class-name "pure-button"
					    :style (create margin 0 padding "0.25em")
					    :onclick  (chain c (del key))
					    (esc (string #\en_dash))))
				  (:div
				   :class-name "pure-u-1 pure-u-md-1-12"
				   (:button :class-name "pure-button"
					    :style (create margin 0 padding "0.25em")
					    :onclick  (chain c (up key))
					    (esc (string #\upwards_arrow))))
				  (:div
				   :class-name "pure-u-1 pure-u-md-1-12"
				   (:button :class-name "pure-button"
					    :style (create margin 0 padding "0.25em")
					    :onclick  (chain c (down key))
					    (esc (string #\downwards_arrow)))))))
			     (chain props (make-row item (chain c (handle-change i)))))))))
	       (htm
		(:fieldset
		  items
		   (if (and
			(chain props make-new)
			(not *view-only*))
		       (htm
			(:button :class-name "pure-button"
				 :onclick (lambda (event)
                                            (chain event (prevent-default))
                                            (chain c (add props)))
				 "+"))))))))

(defmithril-for-classish (*weapon-info weapon-info
			       :onchange (tlambda (v) (chain props (onchange v))))
    (setf (^ this obj) (^ m (prop (^ props default-value))))
  view (lambda (c props children)
	    (htm
	     (:div
	      (input-field name :class-name "pure-u-1 pure-u-md-9-24")
	      (choice-field type #.cl-fccs::+proficiencies+
			    :choice-values #.(mapcar (lambda (x) (string-downcase (string x))) cl-fccs::+proficiencies+)
			    :class-name "pure-u-1 pure-u-md-1-3")
	      (input-field dmg-die :class-name "pure-u-1-6 pure-u-md-1-12")
	      (:div :style (create text-align :center) :class-name "pure-u-1-24" "+")
	      (chain props dmg-bonus)
	      (chain props atk-bonus)
	      (input-field threat
			  :parser  parse-int :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field size :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field hand
			  :parser  parse-int :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field weight
			   :label-as "Wgt."
			  :parser  parse-float :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field rng
			  :parser  (lambda (x) (when x (parse-int x)))
			  :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field shots
			  :parser  parse-int :class-name "pure-u-1-6 pure-u-md-1-12")
	      (input-field qualities :class-name "pure-u-1 pure-u-md-1-2")))))

		  
(defmithril-for-classish (*gear-info gear-info
				   :onchange (tlambda (v field)
						(chain props (onchange v))
						(if (= field "name")
						    (chain this (update-fields (aget :name v))))))
  (setf
    (^ this obj) (^ m (prop (^ props default-value)))
    (^ this update-fields)
    (tlambda (name)
      (^ m (request (create
                 :method "POST"
                 :url (fixup-path "/complete/gear-info")
                 :config xhr-config
                 :data name))
         then
         (tlambda (response)
           (when (and (/= response nil) (> (chain response length) 0))
             (setf response
                   (loop for item in response
                         collect (if (= item null) "" item)))
             (funcall (chain this (handle-change :effect)) (elt response 1))
             (funcall (chain this (handle-change :size)) (elt response 2))
             (funcall (chain this (handle-change :hand)) (elt response 3))
             (funcall (chain this (handle-change :weight)) (elt response 4)))))))
  view (lambda (c props children)
	   (htm
	    (:div
	     (autocomplete-input-field
	      name
	      :update-completions
	      (lambda (val fn)
                (^ m
                   (request (create
                              :method "POST"
                              :url (fixup-path "/complete/gear")
                              :config xhr-config
                              :data val))
                   (then
                     (lambda (response)
		   (funcall fn response)))))
              :class-name "pure-u-1 pure-u-md-1-3")
	     (input-field effect
			  :override-value (aget :effect (chain c (obj)))
			  :class-name "pure-u-1 pure-u-md-1-3")
	     (choice-field size (:n :f :d :t :s :m :l :h :g :c :e :v)
			   :override-value (aget :size (chain c (obj)))
			   :choice-values ("n" "f" "d" "t" "s" "m" "l" "h" "g" "c" "e" "v")
			   :class-name "pure-u-1 pure-u-md-1-8")
	     (input-field hand
			  :override-value (aget :hand (chain c (obj)))
			  :class-name "pure-u-1 pure-u-md-1-12")
	     (input-field weight
			  :override-value (aget :weight (chain c (obj)))
			  :class-name "pure-u-1 pure-u-md-1-12"
			  :parser  #'parse-int)))))


	     
				 
(defmithril-for-classish (*fc-class fc-class
				  :onchange (tlambda (v) (chain props (onchange v))))
    (setf (^ this obj) (^ m (prop (^ props default-value))))
  view (lambda (c props children)
         (htm
           (:div
             :class-name "pure-u-1 pure-u-md-11-12"
             (autocomplete-input-field
               the-class
               :update-completions
               (lambda (val fn)
                 (^ m
                    (request
                      (create :method "POST"
                              :url (fixup-path "/complete/class")
                              :config xhr-config
                              :data val))
                    (then
                      (lambda (response)
                     (funcall fn response)))))
               :class-name "pure-u-1 pure-u-md-2-3"
               :parser  to-keyword)
             (input-field level
                          :class-name "pure-u-1 pure-u-md-1-4"
                          :input-type "number"
                          :parser  parse-int)))))



(defmithril-for-classish (*spell-info spell-info
				    :onchange (tlambda (v field)
						 (chain props (onchange v))
						 (if (= field "name")
						     (chain this (update-fields (aget :name v))))))
    (setf
      (^ this obj) (^ m (prop (^ props default-value)))
      (^ this update-fields)
      (tlambda (name)
        (^ m
           (request
             (create :method "POST"
                     :url (fixup-path "/complete/spell-info")
                     :config xhr-config
                     :data name)
             (then
               (tlambda (response)
                (when (> (chain response length) 0)
                  (setf response
                        (loop for item in response
                              collect (if (= item null) "" item)))
                  (funcall (chain this (handle-change :level)) (elt response 1))
                  (funcall (chain this (handle-change :discipline)) (elt response 2))
                  (funcall (chain this (handle-change :casting-time)) (elt response 3))
                  (funcall (chain this (handle-change :distance)) (elt response 4))
                  (funcall (chain this (handle-change :area)) (elt response 5))
                  (funcall (chain this (handle-change :saving-throw)) (elt response 6))
                  (funcall (chain this (handle-change :duration)) (elt response 7))
                  (funcall (chain this (handle-change :preparation-cost)) (elt response 8))
                  (funcall (chain this (handle-change :effect)) (elt response 9)))))))))
  view (lambda (c props children)
         (htm
           (:div
             :class-name nil
             (autocomplete-input-field
               name
               :class-name "pure-u-1 pure-u-md-1-4"
               :update-completions
               (lambda (val fn)
                 (^ m
                    (request
                      (create
                        :method "POST"
                        :url (fixup-path "/complete/spell")
                        :config xhr-config
                        :data val))
                    (then fn)))
               :class-name "pure-u-1 pure-u-md-1-8")
             (input-field level
                          :class-name "pure-u-1 pure-u-md-1-12"
                          :input-type "number"
                          :override-value (aget :level (chain c (obj)))
                          :parser  parse-int)
             (input-field discipline
                          :class-name "pure-u-1 pure-u-md-1-4"
                          :override-value (aget :discipline (chain c (obj))))
             (input-field casting-time
                          :class-name "pure-u-1 pure-u-md-1-4"
                          :override-value (aget :casting-time (chain c (obj))))
             (input-field distance
                          :class-name "pure-u-1 pure-u-md-1-4"
                          :override-value (aget :distance (chain c (obj))))
             (input-field area
                          :class-name "pure-u-1 pure-u-md-1-4"
                          :override-value (aget :area (chain c (obj))))
             (input-field saving-throw
                          :class-name "pure-u-1 pure-u-md-1-4"
                          :override-value (aget :saving-throw (chain c (obj))))
             (input-field duration
                          :class-name "pure-u-1 pure-u-md-1-4"
                          :override-value (aget :duration (chain c (obj))))
             (input-field preparation-cost
                          :class-name "pure-u-1 pure-u-md-1-4"
                          :override-value (aget :preparation-cost (chain c (obj))))
             (input-field effect
                          :class-name "pure-u-1 pure-u-md-1-4"
                          :override-value (aget :effect (chain c (obj))))))))

(defmithril-for-classish (*ability-info ability-info
				  :onchange (tlambda (v) (chain props (onchange v))))
    nil
  view (lambda (c props children)
         (htm
           (:div :class-name nil
                 (:div
                   :class-name "pure-u-1-3"
                   (+
                     (aget :name (chain c (obj)))
                     " ("
                     (if (fc-class-p (aget :from (chain c (obj))))
                       (+
                         (string-capitalize
                           (aget :the-class (aget :from (chain c (obj)))))
                         "/"
                         (aget :level (aget :from
                                            (chain c (obj)))))
                       (aget :from (chain c (obj))))
                     ")"))
                 (input-field parameter
                              :class-name "pure-u-1-4")
                 (choice-field list-as 
                               (:combat :non-combat :spellcasting :none)
                               :choice-values ("combat" "non-combat" "spellcasting" "none")
                               :class-name "pure-u-5-12")))))

(defmithril-for-classish (*feat-info feat-info
				  :onchange (tlambda (v) (chain props (onchange v))))
    nil
  view (lambda (c props children)
         (htm
           (:div :class-name nil
                 (autocomplete-input-field
                   name
                   :update-completions
                   (lambda (val fn)
                     (^ m
                        (request
                          (create :method "POST"
                                  :config xhr-config
                                  :data val
                                  :url (fixup-path "/complete/feat")))
                        (then fn)))
                   :class-name "pure-u-2-3")
                 (input-field parameter
                              :class-name "pure-u-1-3")
                 (input-field notes
                              :class-name "pure-u-7-12")
                 (choice-field list-as 
                               (:combat :non-combat :spellcasting)
                               :choice-values ("combat" "non-combat" "spellcasting")
                               :class-name "pure-u-5-12")))))
			
(defmithril *character-menu
    controller
  (constructor ()
    (setf (chain this section)
          (chain m (prop (or (^ window location hash (slice 1)) "basics")))
          (chain this change-section)
          (mlambda (section)
            (tlambda (ev)
	      (chain ev (prevent-default))
              (setf (^ window location hash) section)
              (chain this (section section))))))
  view
  (lambda (c props children)
    (htm (:*character
           :default-value (chain props default-value)
           :change-section  (chain c change-section)
           :character-id (chain props character-id)
           :section (chain c (section))))))

(defun simple-update (path)
  (lambda (val fn)
    (^ m
       (request
         (create
           :method "POST"
           :url (fixup-path path)
           :config xhr-config
           :data val))
       (then
         (lambda (response)
           (funcall fn response))))))

(defmithril-for-classish (*character fc-character
				   :onchange
				   (tlambda (newch) (chain this (update-abilities newch))))
    (setf
      (^ this delete-popup) (^ m (prop nil))
      (^ this on-fudge-change)
      (mlambda (the-fudge)
        (tlambda (newval)
          (let ((newobj (chain this
                               (obj)
                               (set-in (ps:array :fudges the-fudge)
                                       newval))))
            (chain (obj newobj))
            (chain this (update-abilities newobj)))))
      (^ this update-abilities)
      (tlambda (newch field)
	(unless (chain this upload-timer)
	  (setf (chain this upload-timer)
		(chain window (set-timeout (chain this upload-data) 10000))))
	(when (or t (eql field :classes))
	  (let ((new-obj (chain this
				(obj)
				(set :ability-list
				     (fixup-abilities newch)))))
	    (chain this (obj new-obj)))))
      (^ this delete)
      (lambda ()
        (^ m
           (request
             (create
               :method "POST"
               :url "./delete/"
               :config xhr-config
               :deserialize (lambda (x) x)
               :serialize (lambda (x) x)))
           (then (lambda () (setf (ps:@ window location) (fixup-path "/"))))))
      (^ this upload-data)
      (tlambda ()
        (^ m
           (request
             (create
               :method "POST"
               :url (+
                      (fixup-path "/save-character/")
                      (chain props character-id))
               :config xhr-config
	       :deserialize (lambda (x) x)
               :data
		 (chain this (obj) (to-j-s)))))
        (setf (chain this upload-timer) nil)))
  view (lambda (c props children)
         (htm
           (:div
             (:div
               :class-name "pure-menu-scrollable pure-menu-horizontal"
               (:ul :class-name "pure-menu-list"
                    (:li
                      :class-name (+ "pure-menu-item"
                                     (if (= (chain props section) "basics")
                                       " pure-menu-selected" ""))
                      (:a :href "#" :class-name "pure-menu-link"
                          :onclick (chain props (change-section "basics"))
                          "Basics"))
                    (:li
                      :class-name (+ "pure-menu-item"
                                     (if (= (chain props  section) "combat")
                                       " pure-menu-selected" ""))
                      (:a :href "#" :class-name "pure-menu-link"
                          :onclick (chain props (change-section "combat"))
                          "Combat"))
                    (:li
                      :class-name (+ "pure-menu-item"
                                     (if (= (chain props  section) "gear")
                                       " pure-menu-selected" ""))
                      (:a :href "#" :class-name "pure-menu-link"
                          :onclick (chain props (change-section "gear"))
                          "Gear"))
                    (:li
                      :class-name (+ "pure-menu-item"
                                     (if (= (chain props  section) "spell")
                                       " pure-menu-selected" ""))
                      (:a :href "#" :class-name "pure-menu-link"
                          :onclick (chain props (change-section "spell"))
                          "Spellcasting"))
                    (:li
                      :class-name (+ "pure-menu-item"
                                     (if (= (chain props  section) "bio")
                                       " pure-menu-selected" ""))
                      (:a :href "#" :class-name "pure-menu-link"
                          :onclick (chain props (change-section "bio"))
                          "Bio/Notes"))
                    (:li
                      :class-name "pure-menu-item"
                      (:a :href (+ (fixup-path "/pdf-character/")
                                   (chain props character-id))
                          :target "_blank"
                          :class-name "pure-menu-link"
                          "PDF"))
                    (unless
                      (or (chain props read-only) *view-only*)
                      (htm
                        (:li
                          :class-name "pure-menu-item"
                          (:a :href "#" 
                              :class-name "pure-menu-link"
                              :onclick (tlambda (ev)
                                                (chain ev (prevent-default))
                                                (chain c (delete-popup t))))
                              "Delete")
                          (when (chain c (delete-popup))
                            (htm
                              (:div
                                :style (create position :fixed
                                               width "100%" 
                                               display :block
                                               height "100%"
                                               background-color "white")
                                (:p "Are you sure?")
                                (:a :class-name "pure-button"
                                    :href "#"
                                    :onclick (chain c delete)
                                    "Yes")
                                (:a :class-name "pure-button"
                                    :href "#"
                                    :onclick (tlambda (ev)
                                                      (chain ev (prevent-default))
                                                      (chain c (delete-popup nil)))
                                    "No"))))))
                    (:li
                      :class-name "pure-menu-item"
                      (:a :href (fixup-path "/") :class-name "pure-menu-link"
                          :onclick (tlambda ()
                                            (when
                                              (chain c upload-timer)
                                              (chain c (upload-data))))
                          "Save & Exit"))))
             (cond
               ((= (chain props section) "basics")
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
                                                       (simple-update "/complete/species")
                                                       :class-name "pure-u-1 pure-u-md-1-8")
                             (autocomplete-input-field
                               talent
                               :update-completions
                               (simple-update "/complete/talent")
                               :class-name "pure-u-1 pure-u-md-1-8"
                               )
                             (autocomplete-input-field
                               specialty
                               :update-completions
                               (simple-update  "/complete/specialty")
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
                                                  :default-value  data
                                                  :onchange  updater)))
                                         :class-name "pure-u-1 pure-g"
                                         ;:row-key (lambda (x) (aget :the-class x))
                                         :inner-class "pure-u-1-3 pure-g"
                                         :make-new #'make-fc-class
                                         )
                             (input-field player-name
                                          :class-name "pure-u-1 pure-u-md-1-3")
                             (input-field xp 
                                          :parser  parse-int
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
                                       :style (create text-align "center")
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
                                                     :parser  parse-int))
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
                                                     :parser  parse-int))
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
                                                     :parser  parse-int))
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
                                       :style (create text-align "center")
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
                                                     :parser  parse-int))
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
                                                     :parser  parse-int))
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
                                                     :parser  parse-int))
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
                               :style (create text-align "center"
                                              margin 0
                                              background-color "#e0e0e0")
                               "Skills")
                             (output-field total-ranks
                                           :class-name "pure-u-1-4")
                             (skill-table #.(subseq +skills+ 0 (ceiling (length +skills+) 2)))
                             (skill-table #.(subseq +skills+ (ceiling (length +skills+) 2))))
                           (:div
                             :class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
                             (:h2
                               :style (create text-align "center"
                                              margin 0
                                              background-color "#e0e0e0")
                               "Focuses")
                             (list-field crafting-foci
                                         (lambda (data updater)
                                           (htm
                                             (:div
                                               :class-name "pure-u-11-12"
                                               (:*validating-input
                                                 :default-value  data
                                                 :parser  (lambda (x) x)
                                                 :style (create padding 0)
                                                 :input-class "pure-input-1"
                                                 :validator  (lambda () t)
                                                 :onchange  updater))))
                                         :class-name "pure-u-1 pure-u-md-1-2 pure-g"
                                         :inner-class nil
                                         :make-new (lambda () ""))
                             (list-field ride-foci
                                         (lambda (data updater)
                                           (htm
                                             (:div
                                               :class-name "pure-u-11-12"
                                               (:*validating-input
                                                 :default-value  data
                                                 :parser  (lambda (x) x)
                                                 :input-class "pure-input-1"
                                                 :style (create padding 0)
                                                 :validator  (lambda () t)
                                                 :onchange  updater))))
                                         :class-name "pure-u-1 pure-u-md-1-2 pure-g"
                                         :inner-class nil
                                         :make-new (lambda () "")))
                           (:div
                             :class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
                             (:h2
                               :style (create text-align "center"
                                              margin 0
                                              background-color "#e0e0e0")
                               "Interests")
                             (output-field total-studies)
                             (input-field alignment
                                          :class-name "pure-u-1 pure-u-md-1-2")
                             (list-field languages
                                         (lambda (data updater)
                                           (htm
                                             (:*validating-input
                                               :default-value  data
                                               :parser  (lambda (x) x)
					       :style (create padding 0)
                                               :input-class "pure-input-1"
                                               :validator  (lambda () t)
                                               :onchange  updater)))
                                         :class-name  nil
                                         :inner-class "pure-u-1 pure-u-md-1-2"
                                         :make-new (lambda () ""))
                             (list-field studies
                                         (lambda (data updater)
                                           (htm
                                             (:*validating-input
                                               :default-value  data
                                               :parser  (lambda (x) x)
					       :style (create padding 0)
                                               :input-class "pure-input-1"
                                               :validator  (lambda () t)
                                               :onchange  updater)))
                                         :class-name  nil
                                         :inner-class "pure-u-1 pure-u-md-1-2"
                                         :make-new (lambda () "")))
                           (:div
                             :class-name "pure-u-1 pure-u-xl-1-2"
                             (:h2
                               :style (create text-align "center"
                                              margin 0
                                              background-color "#e0e0e0")
                               "Subplots")
                             (list-field completed-subplots
                                         (lambda (data updater)
                                           (htm
                                             (:*validating-input
                                               :default-value  data
                                               :parser  (lambda (x) x)
                                               :input-class "pure-input-1"
                                               :validator  (lambda () t)
                                               :onchange  updater)))
                                         :class-name  nil
                                         :inner-class "pure-u-1 pure-u-md-1-4"
                                         :make-new (lambda () ""))
                             (list-field incomplete-subplots
                                         (lambda (data updater)
                                           (htm
                                             (:*validating-input
                                               :default-value  data
                                               :parser  (lambda (x) x)
                                               :input-class "pure-input-1"
                                               :validator  (lambda () t)
                                               :onchange  updater)))
                                         :class-name  nil
                                         :inner-class "pure-u-1 pure-u-md-1-4"
                                         :make-new (lambda () "")))
                           (:div
                             :class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
                             (:h2
                               :style (create text-align "center"
                                              margin 0
                                              background-color "#e0e0e0")
                               "Coin")
                             (input-field coin-in-hand
                                          :parser  parse-int
                                          :class-name "pure-u-1")
                             (input-field stake
                                          :parser  parse-int
                                          :class-name "pure-u-1"))
                           (:div
                             :class-name "pure-u-1 pure-u-md-1-2 pure-u-xl-1-4 pure-g"
                             (:div
                               :class-name "pure-u-1 pure-u-md-7-8"
                               :style (create margin 0 padding 0)
                               (:h2
                                 :style (create text-align "center"
                                                margin 0
                                                background-color "#e0e0e0")
                                 "Lifestyle"))
                             (output-field lifestyle
                                           :class-name "pure-u-1 pure-u-md-1-8"
                                           :show-label nil)
                             (input-field panache
                                          :parser  parse-int
                                          :class-name "pure-u-1 pure-u-md-1-2")
                             (input-field prudence
                                          :parser  parse-int
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
                                                :default-value  data
                                                :onchange  updater)))
                                       :read-only t
                                       :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
                                       :row-key (lambda (x) (+ (aget :name x)
                                                               (aget :from x)))
                                       :inner-class "pure-u-1 pure-u-md-1-2")
                           (list-field feat-list
                                       (lambda (data updater)
                                         (htm (:*feat-info
                                                :default-value  data
                                                :onchange  updater)))
                                       :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
                                       ;:row-key (lambda (x) (+ (aget :name x)
                                       ;(aget :parameter x)
                                       ;(aget :notes x)))
                                       :make-new #'make-feat-info
                                       :inner-class "pure-u-1 pure-u-md-1-2")
                           ))))
               ((= (chain props section) "combat")
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
                                               +proficiencies+
                                               :class-name "pure-u-1"
                                               :checkbox-class "pure-u-1-4")
                             (checkboxes-field forte-list
                                               +proficiencies+
                                               :class-name "pure-u-1"
                                               :checkbox-class "pure-u-1-4")
                             (:h3 :class-name "heading pure-u-1"
                                  "Tricks")
                             (list-field tricks
                                         (lambda (data updater)
                                           (htm (:*validating-input
                                                  :default-value  data
                                                  :error-style (create background-color "pink"
                                                                       padding 0)
                                                  :style (create background-color "white"
                                                                 padding 0)
                                                  :validator  #'stringp
                                                  :parser (lambda (x) x)
                                                  :onchange  updater)))
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
                                          :parser  #'parse-int)
                             (input-field armor-dp
                                          :class-name "pure-u-1 pure-u-md-1-6"
                                          :parser  #'parse-int)
                             (input-field armor-acp
                                          :class-name "pure-u-1 pure-u-md-1-6"
                                          :parser  #'parse-int)
                             (input-field armor-speed
                                          :class-name "pure-u-1 pure-u-md-1-6"
                                          :parser  #'parse-int)
                             (input-field armor-weight
                                          :class-name "pure-u-1 pure-u-md-1-6"
                                          :parser  #'parse-int)
                             (input-field armor-disguise
                                          :class-name "pure-u-1 pure-u-md-1-6"
                                          :parser  #'parse-int)
                             (input-field armor-craftsmanship
                                          :class-name "pure-u-1 pure-u-md-1-4")
                             (input-field armor-construction
                                          :class-name "pure-u-1 pure-u-md-1-4")
                             (input-field armor-customizations
                                          :class-name "pure-u-1 pure-u-md-5-6"
                                          :formatter (lambda (x)
                                                       (chain x (join ", ")))
                                          :parser (lambda (x)
                                                    (unloopable
                                                      (chain x (split (ps:new (*reg-exp ", *")))))))
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
               ((= (chain props section) "gear")
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
                                   :parser  #'parse-int)
                      (output-field renown)
                      (input-field heroic-renown
                                   :parser  #'parse-int)
                      (input-field military-renown
                                   :parser  #'parse-int)
                      (input-field noble-renown
                                   :parser  #'parse-int))
                    (:h2 :class-name "heading pure-u-1 pure-u-xl-1-2"
                         "Gear")
                    (list-field gear
                                (lambda (data updater)
                                  (htm (:*gear-info
                                         :default-value  data
                                         :onchange  updater)))
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
                                           :default-value  data
                                           :onchange  updater)))
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
                                           :default-value  data
                                           :onchange  updater)))
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
                                           :default-value  data
                                           :onchange  updater)))
                                  :show-name nil
                                  :class-name "pure-u-1 pure-g"
                                  :inner-class "pure-u-1 pure-g"
                                  :make-new #'make-magic-item-info
                                  ))
                    )))
               ((= (chain props section) "spell")
                (htm
                  (:form
                    :key "spell"
                    :class-name "pure-form pure-form-stacked"
                    (:div
                      :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
                      (input-field casting-level
                                   :parser  #'parse-int)
                      (input-field spell-points
                                   :parser  #'parse-int))
                    (:div
                      :class-name "pure-u-1 pure-u-xl-1-2 pure-g"
                      (output-field spellcasting-total
                                    :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8")
                      (input-field spellcasting-ranks
                                   :class-name "pure-u-1 pure-u-md-1-4 pure-u-xl-1-8"
                                   :parser  #'parse-int)
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
                                   :parser  #'parse-int))
                    (list-field spells
                                (lambda (data updater)
                                  (htm (:*spell-info
                                         :default-value  data
                                         :onchange  updater)))
                                :class-name "pure-u-1 pure-g"
                                ;:row-key (lambda (x) (aget :the-class x))
                                :inner-class "pure-u-1 pure-u-xl-1-2 pure-g"
                                :make-new #'make-spell-info
                                ))))
               ((= (chain props section) "bio")
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
                       (:div :class-name "pure-u-1-12")))))))))



(defmithril *account-settings
    view (lambda (c props children)
	     (htm
	      (:div
	       (:*logout)
	       (:*change-password)))))

(defmithril *logout
    view (lambda (c props children)
	     (htm
	      (:form
	       :action (fixup-path "/logout/")
	       :method :post
	       :class-name "pure-form"
	       (:input
		:type :hidden
		:name "csrf-token"
		:value (chain document (get-element-by-id "csrf") inner-h-t-m-l (trim)))
	       (:input
		:type "submit"
		:class-name "pure-button"
		:value "Logout")))))

(defmithril *change-password
    controller
  (constructor (props children)
    (setf (chain this password1) (chain m (prop ""))
	  (chain this password2) (chain m (prop ""))
	  (chain this state) (chain m (prop :initial))))
    view (lambda (c)
	     (htm
	      (:form
	       :class-name "pure-form"
		(:legend "Password")
		(:label :html-for "password1" "New Password")
		(:input :id "password1" :class-name "pure-input"
			:type "password"
			:value  (chain c (password1))
			:oninput  (^ m (with-attr "value" (chain c password1))))
		(:label :html-for "password2" "Confirm Password")
		(:input :id "password2" :class-name "pure-input"
			:type "password"
			:oninput  (^ m (with-attr "value" (chain c password2)))
			:value  (chain c (password2)))
		(:button
		 :class-name "pure-button"
		 :onclick (tlambda (event)
			       (chain event (prevent-default))
			       (if (= (chain c (password1))
				      (chain c (password2)))
				   (progn
				     (chain c (state :sent))
				     (post-data (fixup-path "/set-password/")
						(chain c (password1))
						:raw t
						:complete-callback
						(tlambda ()
						  (^ m (start-computation))
						  (^ c (state :success))
						  (^ c (password1 ""))
						  (^ c (password2 ""))
						  (^ m (end-computation)))))
				   (alert "Passwords must match")))
		 "Submit")
		(cond
		    ((eql (chain c (state)) :initial) nil)
		    ((eql (chain c (state)) :sent)
		     (htm (:p "In-progress...")))
		    ((eql (chain c (state)) :success)
		     (htm (:p "Success!"))))))))

(defmithril *character-list
    controller
  (constructor ()
	       (setf
		(chain this edit)
		(tlambda (id)
		  (lambda ()
		    (setf (chain window location)
			  (+ (fixup-path "/character/") id "/"))))))
  view (lambda (c props children)
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
		(loop for item in (loopable (chain props value))
		   collect
		     (htm
		      (:tr
		       :onclick (chain c (edit (aget :id item)))
		       :key (aget :id item)
		       (:td (aget :id item))
		       (:td (aget :char-name item))
		       (:td (aget :player-name item))
		       (:td (aget :career-level item)))))))
	   (:form
	    (:input
	     :type :hidden
	     :name "csrf-token"
	     :read-only t
	     :value (chain document (get-element-by-id "csrf") inner-h-t-m-l (trim)))
	    (:button
	     :form-action (fixup-path "/new-character/")
	     :form-method "POST"
	     :class-name "pure-button"
	     "New"))
	   (:p
	    (:a :href (fixup-path "/account/")
		"Account Settings"))
	   (:p
	    (:a :href "https://github.com/jasom/cl-fccs/issues"
		"Bugs/Feature requests"))))))



; I want AJAX errors to be swallowed, but lets log in case there's anything interesting
(setf (^ m deferred onerror) (lambda (e) (^ console (error e))))

;; Keep session alive
(set-interval (lambda ()
		(post-data (fixup-path "/dummy/") ""))
	      (* 60 60 1000))

