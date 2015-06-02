(in-package #:cl-fccs)
#-ps(declaim (optimize (speed 1)(space 1) (debug 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun key-fmt (k &rest args)
  (make-keyword
   (apply #'format nil (string k) args))))

(defp +career-levels+
    #-ps fccg::+career-levels+
    #+ps (ps:lisp `(quote ,fccg::+career-levels+)))
    
(defp *fields* (amake))

#-ps(defun ht-to-obj (ht)
      (let ((result nil))
	(maphash (lambda (k v)
		   (cond
		     ((hash-table-p v)
		      (push (ht-to-obj v) result))
		     ((listp v)
		      (push
		       `(list ,@(loop for item in v
				     if (hash-table-p item)
				     collect (ht-to-obj item)
				     else collect `',item))
		       result))
		     (t
		      (push `',v result)))
		   (push k result)) ht)
	`(chain *Immutable
		(from-j-s (create ,@result)))))

(defun calculate-field (fn character)
  (or
   (ignore-errors
     (let ((total-fudge 0)
	   (base-fudge (funcall (aget fn *fields*) character)))
       (cond
	 ((integerp base-fudge)
	  (loop for item in (loopable (aget fn (aget :fudges character)))
	     do (incf total-fudge (aget :value item)))
	  (+ total-fudge 
	     base-fudge))
	 (t (if (emptyp (aget fn (aget :fudges character)))
		base-fudge
		(elt (aget :fudges character) 0))))))
   0))

(defmacro deffield (fn (chvar) &body b)
  `(setf (aget ,fn *fields*)
	 (lambda (,chvar)
	   ,@b)))

(defun parse-integerish (string)
  (let ((fixed (ssub #\- #\en_dash string)))
  #+ps(or (parse-integer fixed) 0)
  #-ps(cond
    ((notany #'digit-char-p string) 0)
    (t (parse-integer fixed :junk-allowed t)))))
      
(defun number-validator (&key value &allow-other-keys)
  #-ps(numberp value)
  #+ps(= (typeof value) :number))
      
(defun integer-validator (&key value &allow-other-keys)
  (integerp value))

(defun bool-validator (&key value &allow-other-keys)
  (or (eql value t)
      (not value)))

(defun maybe-integer-v (&key value)
  (or (null value) (integerp value)))

#+ps(defun identity (value) value)
#+ps(defp *id-counter* 0)
#+ps(defun genid () (+ "gid" (incf *id-counter*)))

(defun string-validator (&key value &allow-other-keys)
  (stringp value))

(defun list-of (predicate)
       (lambda (&key value &allow-other-keys)
	 (and
	  (listp value)
	      (every predicate value))))

(defun get-species-mod (character attr)
  (let* ((species-data
	  (if (equal (aget :species character) "Human")
	      (aget (aget :talent character)
		    +talent-hash+) 
	      (aget (aget :species character)
		    +species-hash+)))
	 (attrs (if species-data
		    (aget :attr species-data)
		    (list)))
	 (strong (aget :strong-attr character))
	 (weak (aget :weak-attr character))
	 (mod 0))
    (loop for item in (loopable attrs)
       do
	 (cond
	   ((or (eql (elt item 0) :any)
		(listp (elt item 0)))
	    (when (and (eql strong attr)
		       (> (elt item 1) 0))
	      (incf mod (elt item 1)))
	    (when (and (eql weak attr)
		       (< (elt item 1) 0))
	      (incf mod (elt item 1))))
	   ((eql (elt item 0) attr)
	    (incf mod (elt item 1)))))
    mod))

#-ps(eval-when (:compile-toplevel :load-toplevel :execute)
      (defun get-skill-attr (skill)
      (let ((attr-name (elt (aget skill +skills-hash+) 0)))
	(make-keyword (format nil "~a-MOD" (string-upcase attr-name))))))

(defun loose-sum (&rest args)
  (let ((sum 0))
    (loop for item in args
	 when (integerp item) do (incf sum item))
    sum))

(defmacro true-score (character attr)
  (alexandria:once-only
      (character)
    (let* ((attribute-name (string-upcase (string attr)))
	  (atr (alexandria:make-keyword (subseq attribute-name 0 3)))
	  (input (alexandria:make-keyword (format nil "BASE-~A" attribute-name))))
      `(loose-sum
	(aget ,input ,character)
	(get-species-mod character ,atr)))))

(defun calculate-column (column character)
       (loop for item in (loopable (aget :classes character))
	    sum
	    (parse-integer
		 (aget column
		       (nth
			(1- (aget :level item))
			(aget
			 (to-keyword
			  (aget :the-class item)) +class-hash+))))))

(defclassish fc-class
    (the-class :initform :assassin
	       :fixup (lambda (&key value &allow-other-keys) (to-keyword value))
	   :validator (lambda (&key value &allow-other-keys)
			(member value (akeys +class-hash+))))
  (level :initform 1
	 :validator #'integer-validator))

(defun get-abilities-for-level (clss level)
  (when (and (> level 0)
	     (<= level 20))
    (let ((lvlinfo (nth (1- level) (aget clss +class-hash+))))
      (unloopable
       (loop for name in
	    (or (loopable (aget :abilities lvlinfo))
		(loopable (aget :special lvlinfo)))
	  collect (make-ability-info :name name
				     :from (make-fc-class :the-class clss
							  :level level)))))))

(defun get-class-abilities (character)
  (mapcan (lambda (lvlinfo)
	    (mapcan
	     (lambda (i)
	       (get-abilities-for-level (aget :the-class lvlinfo) i))
	     (range 1 (1+ (aget :level lvlinfo)))))
	  (aget :classes character)))

(defun get-specialty-abilities (character)
  (let* ((specialty-name (aget :specialty character))
	 (specialty-data (aget specialty-name +specialty-hash+)))
    (if specialty-data
	(mapcar (lambda (ability)
		  (make-ability-info :name (better-capitalize ability) :from specialty-name))
		(aget :qualities specialty-data))
	(list))))

(defun get-species-abilities (character)
  (let ((species-name
	 (if (equal (aget :species character) "Human")
	     (aget :talent character)
	     (aget :species character)))
	(species-data
	 (if (equal (aget :species character) "Human")
	     (aget (aget :talent character)
		   +talent-hash+) 
	     (aget (aget :species character)
		   +species-hash+))))
    (if species-data
	(mapcar (lambda (ability)
		  (make-ability-info :name (better-capitalize ability) :from species-name))
		(aget :qualities species-data))
	(list))))

(defun get-species-speed (character)
  (let ((species-data
	 (if (equal (aget :species character) "Human")
	     (aget (aget :talent character)
		   +talent-hash+) 
	     (aget (aget :species character)
		   +species-hash+))))
    (if species-data
	(aget :base-speed species-data)
	0)))
    
(defun calculate-abilities (character)
  (let ((abilities
	 (append
	  (get-class-abilities character)
	  (get-specialty-abilities character)
	  (get-species-abilities character))))
    #+ps(ps:new (chain *immutable (*list abilities)))
    #-ps abilities))

(defun ability-equal-p (ab1 ab2)
  (let ((f1 (aget :from ab1))
	(f2 (aget :from ab2)))
    (cond
      ((not (equal (aget :name ab1) (aget :name ab2))) nil)
      ((stringp f1) (equal f1 f2))
      ((and (fc-class-p f1) (fc-class-p f2))
       (and (equal (aget :the-class f1)
		    (aget :the-class f2))
	    (= (aget :level f1)
	       (aget :level f2))))
      (t nil))))

(defun fixup-abilities (character)
  (let ((old-abilities (aget :ability-list character))
	(new-abilities (calculate-abilities character))
	(really-new-abilities (list))
	(result (list)))
    (loop for item in (loopable old-abilities)
	 do
	 ;;TODO Fix this for multiple abilites from diff. sources
	 (when (member item new-abilities
		       :test #'ability-equal-p)
	   (setf result
		 (pappend result (list item)))))
    (loop for item in (loopable new-abilities)
	 do
	 (unless (member item result :test #'ability-equal-p)
	   (setf result
		 (pappend result (list item)))))
    result))

;;;TODO Fix the incoming parser to just use name and from as an ID for
;;;     locating in our calculated ability.  Right now untrusted
;;;     clients could add arbitrary abilities
(defclassish ability-info
    (name :initform ""
	  :validator #'string-validator)
  (from :initform (make-fc-class)
	:fixup (lambda (&key value &allow-other-keys)
		 (if (stringp value)
		     value
		     (fixup-fc-class value)))
	:validator (lambda (&key value &allow-other-keys)
		     (or (stringp value)
			 (fc-class-p value))))
  (parameter :validator #'string-validator
	     :initform "")
  (list-as :validator
	   (lambda (&key value &allow-other-keys)
	     (member value (list :combat :non-combat :spellcasting :none)))
	   :fixup (lambda (&key value &allow-other-keys) (to-keyword value))
	   :initform :non-combat))

(defclassish fudge
    (field :initform ""
	   :validator (lambda (&key value &allow-other-keys)
			(member value (akeys *fields*))))
  (value :validator #'integer-validator
	 :initform 0)
  (notes :initform ""
	 :validator #'string-validator))

(defun list-fixup (&key value &allow-other-keys)
  (if value
      value
      (list)))

(defun keyword-fixup (&key value &allow-other-keys)
  (to-keyword value))

(defclassish weapon-info
    (name :validator #'string-validator
	  :initform "")
  (type :validator
	   (lambda (&key value &allow-other-keys)
	     (member value (append (list :||) '#.+proficiencies+)))
	   :fixup (lambda (&key value &allow-other-keys) (to-keyword value))
	   :initform "")
  (dmg-die :validator #'string-validator
       :initform "")
  (threat :validator
	  #'integer-validator
	  :initform 20)
  (size :validator #'string-validator
	:initform "")
  (hand :validator #'integer-validator
	:initform 1)
  (weight :validator #'number-validator
	  :initform 0)
  (rng :validator (lambda (&key value &allow-other-keys)
		    (or (not value)
			(integerp value)))
       :initform nil)
  (shots :validator (lambda (&key value &allow-other-keys)
		    (or (not value)
			(integerp value)))
       :initform nil)
  (qualities :validator #'string-validator
	     :initform ""))

(defclassish gear-info
    (name :initform ""
	  :validator #'string-validator)
    (effect :initform ""
	  :validator #'string-validator)
    (size :initform :s
	  :validator (lambda (&key value &allow-other-keys)
		       (or (not value)
			   (member value '(:n :f :d :t :s :m :l :h :g :c :e :v))))
	  :fixup (lambda (&key value &allow-other-keys)
		   (and value (to-keyword value))))
    (hand :initform "-"
	  :validator #'string-validator)
    (weight :initform 0
	  :validator #'number-validator))

(defclassish feat-info
    (name :initform ""
	  :validator #'string-validator)
  (notes :initform ""
	 :validator #'string-validator)
  (parameter :validator #'string-validator
	     :initform "")
  (list-as :validator
	   (lambda (&key value &allow-other-keys)
	     (member value (list :combat :non-combat :spellcasting)))
	   :fixup (lambda (&key value &allow-other-keys) (to-keyword value))
	   :initform :non-combat)) 

(defclassish spell-info
    (name :initform "" :validator #'string-validator)
  (level :initform 0
	 :validator
	  (lambda (&key value &allow-other-keys)
	    (or (not value)
		(integerp value))))
  (discipline :initform "" :validator #'string-validator)
  (casting-time :initform "" :validator #'string-validator)
  (distance :initform "" :validator #'string-validator)
  (area :initform "" :validator #'string-validator)
  (duration :initform "" :validator #'string-validator)
  (saving-throw :initform "" :validator #'string-validator)
  (preparation-cost :initform "" :validator #'string-validator)
  (effect :initform "" :validator #'string-validator))

#.`(defclassish fc-character
					;(owner :type (or null string) :initform (
       (character-name :initform ""
		       :validator #'string-validator)
     (player-name :initform ""
		  :validator #'string-validator)
     (species :initform ""
	      :validator #'string-validator)
     (strong-attr :initform :str
		  :fixup (lambda (&key value &allow-other-keys) (to-keyword value))
		  :validator (lambda (&key value &allow-other-keys)
			       (member value (list :str :dex :con :int :wis :cha))))
     (weak-attr :initform :str
		:fixup (lambda (&key value &allow-other-keys) (to-keyword value))
		:validator (lambda (&key value &allow-other-keys)
			     (member value (list :str :dex :con :int :wis :cha))))
     (talent :initform "" :validator #'string-validator)
     (specialty :initform "" :validator #'string-validator)
     (classes :initform (list (make-fc-class))
	      :fixup (lambda (&key value &allow-other-keys)
		       #+ps(unless value (setf value (list)))
		       (mapcar #'fixup-fc-class value))
	      :validator (list-of #'fc-class-p))
     (xp :initform 0
	 :validator #'integer-validator)
     (gender :initform "" :validator #'string-validator)
     (age  :initform "" :validator #'string-validator)
     (height :initform "" :validator #'string-validator)
     (weight :initform "" :validator #'string-validator)
     (eyes :initform "" :validator #'string-validator)
     (hair :initform "" :validator #'string-validator)
     (base-str :initform 8 :validator #'integer-validator)
     (base-dex :initform 8 :validator #'integer-validator)
     (base-con :initform 8 :validator #'integer-validator)
     (base-int :initform 8 :validator #'integer-validator)
     (base-wis :initform 8 :validator #'integer-validator)
     (base-cha :initform 8 :validator #'integer-validator)
     ,@(loop for skill in +skills+
	  collect
	    `(,(intern (format nil "~a-ORIGIN"
			       (ssub "_" " " (string skill))) *package*)
	       :initform #+ps false #-ps nil :validator #'bool-validator)
	  collect
	    `(,(intern (format nil "~a-RANKS" (ssub "_" " " (string skill))) *package*) :initform 0 :validator #'integer-validator)
	  collect
	    `(,(intern (format nil "~a-THREAT" (ssub "_" " " (string skill))) *package*)
	       :initform 20 :validator #'integer-validator))
     (crafting-foci :initform (list)
		    :fixup #'list-fixup
		    :validator (list-of #'stringp))
     (ride-foci :initform (list)
		:fixup #'list-fixup
		:validator (list-of #'stringp))
     (alignment :initform "" :validator #'string-validator)
     (languages :initform (list)
		:fixup #'list-fixup
		:validator (list-of #'stringp))
     (studies :initform (list)
		:fixup #'list-fixup
		:validator (list-of #'stringp))
     (completed-subplots :initform (list)
			 :fixup #'list-fixup
			 :validator (list-of #'stringp))
     (incomplete-subplots :initform (list)
			  :fixup #'list-fixup
			  :validator (list-of #'stringp))
     (coin-in-hand :initform 0 :validator #'integer-validator)
     (stake :initform 0 :validator #'integer-validator)
     (panache :initform 0 :validator #'integer-validator)
     (prudence :initform 0 :validator #'integer-validator)
     (ability-list :initform (list)
		   :fixup (lambda (&key value &allow-other-keys)
			    (setf value (list-fixup :value value))
			    (mapcar #'fixup-ability-info value))
		   :validator (list-of #'ability-info-p))
     (feat-list :initform (list)
		:fixup (lambda (&key value &allow-other-keys)
			 (setf value (list-fixup :value value))
			 (mapcar #'fixup-feat-info value))
		:validator (list-of #'feat-info-p))
     (proficiency-list :initform (list)
		       :fixup
		       (lambda (&key value &allow-other-keys)
			 (setf value (list-fixup :value value))
			 (mapcar #'to-keyword value))
		       :validator (list-of #'keywordp))
     (forte-list :initform (list)
		 :fixup
		 (lambda (&key value &allow-other-keys)
		   (setf value (list-fixup :value value))
		   (mapcar #'to-keyword value))
		 :validator (list-of #'keywordp))
     ;;TODO Better validator!!
     (fudges :initform (let ((val (amake)))
			 (loop for item in (loopable (akeys *fields*))
			    do (setf (aget (to-keyword item) val) (list)))
			 val)
	     :fixup (lambda (&key value &allow-other-keys)
		      (loop for item in (loopable (akeys *fields*))
			 do (setf (aget (to-keyword item) value)
				  (list-fixup :value (aget (to-keyword item) value))))
		      value)
	     :validator (lambda (&key &allow-other-keys) t))
     (critical-injuries :initform ""
			:validator #'string-validator)
     ,@
     (loop for i from 1 to 4
	collect
	  `(,(intern (string (key-fmt :weapon-~d i)) *package*)
	     :initform (make-weapon-info)
	     :fixup
	     (lambda (&key value &allow-other-keys)
	       (fixup-weapon-info value))
	     :validator (lambda (&key value &allow-other-keys)
			  (weapon-info-p value))))
     (armor-type :initform :none
		 :validator (lambda (&key value &allow-other-keys)
			      (member value (list :none :partial :moderate)))
		 :fixup #'keyword-fixup)
     (armor-name :validator #'string-validator
		 :initform "")
     (armor-craftsmanship :validator #'string-validator
			  :initform "")
     (armor-construction :validator #'string-validator
			 :initform "")
     (armor-customizations :validator (list-of #'stringp)
			   :fixup #'list-fixup
			   :initform (list))
     (armor-fittings :validator (lambda (&key value &allow-other-keys)
				  (member value (list :none :light :heavy)))
		     :fixup #'keyword-fixup
		     :initform :none)
     (armor-dr :validator #'integer-validator
	       :initform 0)
     (armor-dp :validator #'integer-validator
	       :initform 0)
     (armor-acp :validator #'integer-validator
	       :initform 0)
     (armor-speed :validator #'integer-validator
		  :initform 0)
     (armor-weight :validator #'integer-validator
		   :initform 0)
     (armor-disguise :validator #'integer-validator
		     :initform 0)
     (reputation :validator #'integer-validator
		 :initform 0)
     (heroic-renown :validator #'integer-validator
		    :initform 0)
     (military-renown :validator #'integer-validator
		      :initform 0)
     (noble-renown :validator #'integer-validator
		   :initform 0)
     (gear :validatior (list-of #'gear-info-p)
	   :initform (list)
	   :fixup #'list-fixup)
     (spells :validator (list-of #'spell-info-p)
	     :fixup #'list-fixup
	     :initform (list))
     (casting-level :validator #'integer-validator
		    :initform 0)
     (spell-points :validator #'integer-validator
		   :initform 0)
     (spellcasting-ranks :validator #'integer-validator
			 :initform 0))

#.
`(progn
   ,@(loop for item in +skills+
	  for skill = (ssub "_" " " (string item))
	collect `(deffield ,(make-keyword (format nil "~a-BONUS" skill)) (character)
		   (+
		    (calculate-field ,(get-skill-attr item) character)
		    (calculate-field ,(make-keyword (format nil "~a-MISC-BONUS" skill)) character)
		    (aget ,(make-keyword (format nil "~a-RANKS" skill)) character)))
	collect `(deffield  ,(make-keyword (format nil "~a-MISC-BONUS" skill)) (character)
		   (declare (ignore character))
		   0)
	collect `(deffield ,(make-keyword (format nil "~A-ATTR-BONUS" skill)) (character)
		   (calculate-field ,(get-skill-attr item) character))))

#.`(deffield :total-ranks (character)
     (+
      ,@(loop for item in +skills+
	     for skill = (ssub "_" " " (string item))
	     collect `(aget ,(make-keyword (format nil "~a-RANKS" skill)) character))))


(deffield :career-level (character)
  (let ((sum 0))
    (loop for item in (loopable (aget :classes character))
	 do (incf sum (aget :level item)))
    sum))

(deffield :next-xp (character)
  (elt (elt fccg::+career-levels+
	    (min 20
		 (1+ (calculate-field :career-level character)))) 1))

(deffield :starting-action-dice (character)
  (when (calculate-field :career-level character)
    (parse-integer
     (elt
      (ssplit #\Space
	      (elt (elt fccg::+career-levels+ (calculate-field :career-level character))
		   2))
      0))))

(deffield :action-dice-type (character)
  (when (calculate-field :career-level character)
    (parse-integer
     (ssub "" "[)(d]"
	   (elt
	    (ssplit #\Space
		    (elt (elt fccg::+career-levels+ (calculate-field :career-level character))
			 2))
	    1)))))

#.`(progn
     ,@(loop for item in (list "STR" "DEX" "CON"
			   "INT" "WIS" "CHA")
	  collect
	    `(deffield ,(make-keyword (format nil "REAL-~A" item))
		 (character)
	       (true-score character ,(make-keyword item)))
	  collect
	    `(deffield ,(make-keyword (format nil "~A-MOD" item)) (character)
	       (floor (- (calculate-field ,(make-keyword (format nil "REAL-~A" item))
					  character) 10) 2))))

(defun calculate-points (attr-score)
  (loop with base-score = (- attr-score 8)
     with result = 0
     for subtractme = 4 then 2
     while (> base-score 0)
     do (incf result base-score)
       (decf base-score subtractme)
     finally (return result)))
       

#.`(deffield :skill-points (character)
     (+
     ,@(loop for item in (list "STR" "DEX" "CON"
		      "INT" "WIS" "CHA")
     collect
       `(calculate-points
	 (aget ,(make-keyword (format nil "BASE-~A" item)) character)))))

(deffield :lifestyle (character)
  (+ (calculate-field :cha-mod character)
     (calculate-column :lifestyle character)))

(deffield :appearance-bonus (character)
  (floor (+ 1 (aget :panache character)) 3))

(deffield :money-saved/earned (character)
  (+ 15 (* 5 (aget :prudence character))))

(deffield :income (character)
  (* (aget :panache character) 10))

(deffield :defense-misc-mod (character)
  #-ps(declare (ignore character))
  0)

(deffield :defense-armor-mod (character)
  (- (aget :armor-dp character)))

(deffield :defense (character)
  (+
   10
   (calculate-field :base-defense character)
   (calculate-field :defense-attr-mod character)
   (calculate-field :defense-size-mod character)
   (calculate-field :defense-armor-mod character)
   (calculate-field :defense-misc-mod character)))

(deffield :base-defense (character)
  (calculate-column :def character))

(deffield :defense-attr-mod (character)
  (calculate-field :dex-mod character))

(deffield :defense-size-mod (character)
  (case (calculate-field :size character)
    (:medium 0)
    (:small 1)
    (:large -1)))

(deffield :defense-misc-mod (character)
  #-ps(declare (ignore character))
  0)

(deffield :initiative (character)
  (+ (calculate-field :base-init character)
     (calculate-field :init-attr-mod character)
     (calculate-field :init-misc-mod character)))

(deffield :base-init (character)
  (calculate-column :init character))

(deffield :init-attr-mod (character)
  (calculate-field :dex-mod character))

(deffield :init-misc-mod (character)
  #-ps(declare (ignore character))
  0)

(deffield :vitality (character)
  (let ((class-vitality
	 (loop for item in (loopable (aget :classes character))
	      sum (* (aget :vitality (aget (aget :the-class item) +class-info-hash+))
		     (aget :level item)))))
    (+ class-vitality
       (* (calculate-field :career-level character)
	  (calculate-field :con-mod character)))))

(deffield :wounds (character)
  (let* ((mult (aget :wound-multiplier
		     (aget (aget :species character)
			   +species-hash+)))
	 (wounds (* (calculate-field :real-con character) mult)))
    (if (> mult 1)
	(floor wounds)
	(ceiling wounds))))

(deffield :bab (character)
  (calculate-column :bab character))

(deffield :melee-bonus (character)
  (+ (calculate-field :bab character)
     (calculate-field :melee-attr-mod character)
     (calculate-field :melee-misc-mod character)))

(deffield :melee-attr-mod (character)
  (calculate-field :str-mod character))

(deffield :melee-misc-mod (character)
  #-ps(declare (ignore character))
  0)

(deffield :ranged-bonus (character)
  (+ (calculate-field :bab character)
     (calculate-field :ranged-attr-mod character)
     (calculate-field :ranged-misc-mod character)))

(deffield :ranged-attr-mod (character)
  (calculate-field :dex-mod character))

(deffield :ranged-misc-mod (character)
  #-ps(declare (ignore character))
  0)

(deffield :unarmed-bonus (character)
  (+ (calculate-field :bab character)
     (calculate-field :unarmed-attr-mod character)
     (calculate-field :unarmed-misc-mod character)))

(deffield :unarmed-attr-mod (character)
  (calculate-field :str-mod character))

(deffield :unarmed-misc-mod (character)
  #-ps(declare (ignore character))
  0)

(deffield :fortitude-bonus (character)
  (+ (calculate-field :fortitude-base character)
     (calculate-field :fortitude-attr-mod character)
     (calculate-field :fortitude-misc-mod character)))

(deffield :fortitude-base (character)
  (calculate-column :fort character))

(deffield :fortitude-attr-mod (character)
  (calculate-field :con-mod character))

(deffield :fortitude-misc-mod (character)
  #-ps(declare (ignore character))
  0)

(deffield :reflex-bonus (character)
  (+ (calculate-field :reflex-base character)
     (calculate-field :reflex-attr-mod character)
     (calculate-field :reflex-misc-mod character)))

(deffield :reflex-base (character)
  (calculate-column :ref character))

(deffield :reflex-attr-mod (character)
  (calculate-field :dex-mod character))

(deffield :reflex-misc-mod (character)
  #-ps(declare (ignore character))
  0)

(deffield :will-bonus (character)
  (+ (calculate-field :will-base character)
     (calculate-field :will-attr-mod character)
     (calculate-field :will-misc-mod character)))

(deffield :will-base (character)
  (calculate-column :will character))

(deffield :will-attr-mod (character)
  (calculate-field :wis-mod character))

(deffield :will-misc-mod (character)
  #-ps(declare (ignore character))
  0)

(deffield :size (character)
  (aget :size (aget (aget :species character) +species-hash+)))

(deffield :footprint (character)
  (aget :footprint (aget (aget :species character) +species-hash+)))

(deffield :reach (character)
  (aget :reach (aget (aget :species character) +species-hash+)))

(deffield :ground-speed (character)
  (get-species-speed character))

(deffield :run-speed (character)
  (* 4 (calculate-field :ground-speed character)))

(deffield :travel-speed (character)
  (ceiling (/ (calculate-field :ground-speed character) 10)))

#.`(progn
    ,@(loop
	 for i from 1 to 6
	 collect
	   `(deffield ,(key-fmt :weapon-~D-atk-bonus i) (character)
	      (let ((base-bonus
		     (cond
		       ((member (aget :type (aget ,(key-fmt :weapon-~d i) character))
				(list :hurled :bows :black-powder :siege-weapons))
			(calculate-field :ranged-bonus character))
		       ((eql (aget :type (aget ,(key-fmt :weapon-~d i) character)) :unarmed)
			(calculate-field :unarmed-bonus character))
		       (t
			(calculate-field :melee-bonus character))))
		    (proficient-bonus
		     (cond
		       ((not (member (aget :type (aget ,(key-fmt :weapon-~d i) character))
				     (aget :proficiency-list character)))
			-4)
		       ((member (aget :type (aget ,(key-fmt :weapon-~d i) character))
				(aget :forte-list character))
			1)
		       (t 0))))
		(+ base-bonus proficient-bonus)))

	 collect `(deffield ,(key-fmt :weapon-~d-dmg-bonus i) (character)
		    (cond
		      ((re-match "finesse" (aget :qualities (aget ,(key-fmt :weapon-~d i)character)))
		       (calculate-field :dex-mod character))
		      ((member (aget :type (aget ,(key-fmt :weapon-~d i)character))
			       (list :bows :black-powder :siege-weapons))
		       0)
		      (t
		       (calculate-field :str-mod character))))))

(deffield :action-attack (character)
  (calculate-field :weapon-1-atk-bonus character))

(deffield :action-bullrush (character)
  (calculate-field :athletics-bonus character))

(deffield :action-coupe-de-grace (character)
  (calculate-field :weapon-1-atk-bonus character))

(deffield :action-disarm (character)
  (calculate-field :weapon-1-atk-bonus character))

(deffield :action-feint (character)
  (calculate-field :prestidigitation-bonus character))

(deffield :action-grapple (character)
 (calculate-field :athletics-bonus character))

(deffield :action-pummel (character)
 (calculate-field :unarmed-bonus character))

(deffield :action-taunt (character)
(calculate-field :sense_motive-bonus character))

(deffield :action-threaten (character)
  (calculate-field :intimidate-bonus character))

(deffield :action-tire (character)
  (calculate-field :resolve-bonus character))

(deffield :action-trip (character)
  (calculate-field :acrobatics-bonus character))

;;This is *almost* a simple formula, but they tweaked it
;;for nicer numbers in the 21-25 range (e.g. 21 == 250 instead of 240
(defun capacity-for-effective-str (str)
  (cond
    ((> str 200) ;Prevent heap exhaution or FP overflow
     (capacity-for-effective-str 200))
    ((< str 10)
     (* 5 str))
    ((<= str 20)
     (* 2 (capacity-for-effective-str (- str 5))))
    ((<= str 25)
     (+ (capacity-for-effective-str 20)
	(* 50 (- str 20))))
    (t
     (multiple-value-bind (val rem)
	 (truncate (- str 20) 5)
       (* (expt 2 val)
	  (capacity-for-effective-str (+ rem 20)))))))
     ;;(* 2 (capacity-for-effective-str (- str 5))))))

(deffield :light-capacity (character)
  (let ((sizemod 
	 (case (calculate-field :size character)
	   (:small -1)
	   (:medium 0)
	   (:large 1)))
	(str (calculate-field :real-str character)))
    (when (member :improved-stability
		  (append
		   (get-species-abilities character)
		   (get-specialty-abilities character)))
      (incf sizemod))
    (if (> sizemod 0)
	(capacity-for-effective-str (+ str (* sizemod 5)))
	(capacity-for-effective-str (+ str (* sizemod 2))))))

(deffield :heavy-capacity (character)
  (* 3 (calculate-field :light-capacity character)))

(deffield :lift (character)
  (* 2 (calculate-field :heavy-capacity character)))

(deffield :push/drag (character)
  (* 2 (calculate-field :heavy-capacity character)))

(deffield :legend (character)
 (calculate-column :legend character)) 

(deffield :renown (character)
 (+ (aget :heroic-renown character) 
    (aget :military-renown character)
    (aget :noble-renown character)))

(deffield :total-studies (character)
  (length (aget :studies character)))

(deffield :spellcasting-total (character)
  (+ (aget :spellcasting-ranks character)
     (calculate-field :int-mod character)
     (calculate-field :spellcasting-misc-mod  character)))

(deffield :spellcasting-misc-mod (character)
  #-ps(declare (ignore character))
  0)
				  
(defclassish character-summary
    (:id :initform -1)
  (:char-name :initform "Nobody")
  (:player-name :initform "Nobody")
  (:career-level :initform -1))

(eval-when (:load-toplevel)
  (defvar *deploy* t "Set to nil for development")
  (time
   (with-open-file (f (asdf:system-relative-pathname 'cl-fccs "build/in.js") :direction :output :if-exists :supersede)
     (let ((*features* (cons :ps *features*)))
       (write-string (ps:ps-compile-file
		      (asdf:system-relative-pathname 'cl-fccs "src/ps-compat.lisp")
		      ) f)
       (write-string (ps:ps-compile-file
		      (asdf:system-relative-pathname 'cl-fccs "src/model.lisp")
		      ) f)
       (write-string (ps:ps-compile-file
		      (asdf:system-relative-pathname 'cl-fccs "src/view-macros.lisp")
		      ) f)
       (write-string (ps:ps-compile-file
		      (asdf:system-relative-pathname 'cl-fccs "src/view.lisp")
					 ) f))))
  (uiop:run-program
   (namestring (uiop:merge-pathnames* "gh/redo/redo" (user-homedir-pathname)))
   :directory (asdf:system-relative-pathname 'cl-fccs "build/")
   :output t :error-output :output))
  ;(time
   ;(uiop:run-program
    ;'("/home/aidenn/gh/react/bin/jsx"
      ;"/home/aidenn/psx/src"
      ;"/home/aidenn/psx/build")
    ;:output t :error-output t))
  ;(uiop:run-program
   ;'("cp" "/home/aidenn/psx/build/foo.js" "/home/aidenn/psx/pub/build/psx.js"))
  ;(time
   ;(uiop:run-program 
    ;'("java" "-jar"
      ;"/home/aidenn/src/closure-compiler.jar"
      ;"/home/aidenn/gh/react/build/react-with-addons.js"
      ;"/home/aidenn/psx/pub/build/lz-string.js"
      ;"/home/aidenn/psx/build/foo.js"
      ;"--js_output_file" "/home/aidenn/psx/pub/foo-min.js") :output t :error t))
  ;(time
   ;(uiop:run-program
    ;'("gzip" "-9" "-f" "-k" "/home/aidenn/psx/pub/foo-min.js") :output t :error t))
  ;|#
