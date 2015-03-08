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
	`(create
	  ,@result)))

(defun calculate-field (fn character)
  (or
   (ignore-errors
     (let ((total-fudge 0)
	   (base-fudge (funcall (aget fn *fields*) character)))
       (cond
	 ((integerp base-fudge)
	  (loop for item in (aget fn (aget :fudges character))
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
    (loop for item in attrs
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
       (loop for item in (aget :classes character)
	    sum
	    (parse-integer
		 (aget column
		       (elt
			(aget
			 (to-keyword
			  (aget :the-class item)) +class-hash+)
			(1- (aget :level item)))))))

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
    (let ((lvlinfo (elt (aget clss +class-hash+) (1- level))))
      (loop for name in
	   (or (aget :abilities lvlinfo)
	       (aget :special lvlinfo))
	   collect (make-ability-info :name name
				      :from (make-fc-class :the-class clss
							   :level level))))))

(defun get-class-abilities (character)
  (loop for lvlinfo in (aget :classes character)
     append
       (loop for i from 1 to (aget :level lvlinfo)
	  append (get-abilities-for-level (aget :the-class lvlinfo) i))))

(defun get-specialty-abilities (character)
  (let* ((specialty-name (aget :specialty character))
	 (specialty-data (aget specialty-name +specialty-hash+)))
    (if specialty-data
	(loop for ability in (aget :qualities specialty-data)
	   collect (make-ability-info :name (better-capitalize ability) :from specialty-name))
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
	(loop for ability in (aget :qualities species-data)
	   collect (make-ability-info :name (better-capitalize ability) :from species-name))
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
  (append
   (get-class-abilities character)
   (get-specialty-abilities character)
   (get-species-abilities character)))

(defun fixup-abilities (character)
  (let ((old-abilities (aget :ability-list character))
	(new-abilities (calculate-abilities character))
	(result (list)))
    (loop for item in old-abilities
	 do
	 (when (member (aget :name item) new-abilities
		       #-ps :test #-ps #'equalp
		       :key (lambda (x) (aget :name x)))
	   (setf result
		 (append result (list item)))))
    (loop for item in new-abilities
	 do
	 (unless
	     (or
	      (member (aget :name item) old-abilities
		      #-ps :test #-ps #'equalp
		      :key (lambda (x) (aget :name x)))
	      (member (aget :name item) result
		      #-ps :test #-ps #'equalp
		      :key (lambda (x) (aget :name x))))
	   (setf result
		 (append result (list item)))))
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
	     (member value '(:combat :non-combat :spellcasting)))
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
  (weight :validator #'integer-validator
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

 (defclassish feat-info
    (name :initform ""
	  :validator #'string-validator)
   (notes :initform ""
	 :validator #'string-validator)
  (parameter :validator #'string-validator
	     :initform "")
  (list-as :validator
	   (lambda (&key value &allow-other-keys)
	     (member value '(:combat :non-combat :spellcasting)))
	   :fixup (lambda (&key value &allow-other-keys) (to-keyword value))
	   :initform :non-combat)) 
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
			     (member value '(:str :dex :con :int :wis :cha))))
   (weak-attr :initform :str
		:fixup (lambda (&key value &allow-other-keys) (to-keyword value))
	      :validator (lambda (&key value &allow-other-keys)
		 (member value '(:str :dex :con :int :wis :cha))))
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
   (interests :initform (list)
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
		       (loop for item in (akeys *fields*)
			  do (setf (aget (to-keyword item) val) (list)))
		       val)
	   :fixup (lambda (&key value &allow-other-keys)
		   (loop for item in (akeys *fields*)
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
			    (member value '(:none :partial :moderate)))
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
				(member value '(:none :light :heavy)))
		   :fixup #'keyword-fixup
		   :initform :none)
   )

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
    (loop for item in (aget :classes character)
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
     ,@(loop for item in '("STR" "DEX" "CON"
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
     ,@(loop for item in '("STR" "DEX" "CON"
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

(deffield :initiative-misc-mod (character)
  #-ps(declare (ignore character))
  0)

(deffield :defense (character)
  (+
   (calculate-field :base-defense character)
   (calculate-field :defense-attr-mod character)
   (calculate-field :defense-size-mod character)
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
	 (loop for item in (aget :classes character)
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
				'(:hurled :bows :black-powder :siege-weapons))
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
			       '(:bows :black-powder :siege-weapons))
		       0)
		      (t
		       (calculate-field :str-mod character))))))



#+(or)(
   (armor-type :type (or null (member :partial :moderate))
	       :initform nil
	       :accessor fc-armor-type)
   (armor-name :type (or null string)
	       :initform nil
	       :accessor fc-armor-name)
   (armor-craftsmanship :type (or string null)
			:initform nil
			:accessor fc-armor-construction)
   (armor-construction :type (or string null)
		       :initform nil
		       :accessor fc-armor-construction)
   (armor-customizations :type list
			 :initform nil
			 :accessor fc-armor-customizations)
   (armor-fittings :type (or string null)
		   :initform nil
		   :accessor fc-armor-fittings)
   (reputation :type (or integer null)
	       :initform nil
	       :accessor fc-reputation)
   (heroic-reknown :type (or integer null)
		   :initform nil
		   :accessor fc-heroic-reknown)
   (military-reknown :type (or integer null)
		     :initform nil
		     :accessor fc-military-reknown)
   (noble-reknown :type (or integer null)
		   :initform nil
		   :accessor fc-noble-reknown)
   (gear :type (satisfies gear-list-p)
	 :initform nil
	 :accessor fc-gear)
   (fudges :type (satisfies fudge-list-p)
	   :accessor fc-fudges))


#|
(defun fc-class-equal (x y)
  (and (equal
	(aget :class x)
	(aget :class y))
       (= (aget :level x)
	  (aget :level y))))

(defun make-fudge (&key (name "Misc") (value 0) (notes ""))
  (let ((obj (amake)))
    (setf (aget :name obj) name
	  (aget :value obj) value
	  (aget notes obj) notes)))

(defclass fc-foci (unbound-initialize)
  ((crafting-1 :initform nil :type (or nil string))
   (crafting-2 :initform nil :type (or nil string))
   (crafting-3 :initform nil :type (or nil string))
   (crafting-4 :initform nil :type (or nil string))))



(defclass feat-info (unbound-initialize)
  ((id :type (or null integer) :accessor fc-feat-id :initarg :id)
    (name :type #.`(member ,@+feat-names+)
	 :accessor fc-feat-name)
   (parameter :type (or string null)
	      :accessor fc-feat-parameter
	      :initform nil)
   (list-as :type (member :combat :non-combat :spellcasting)
	    :initform :combat
	    :accessor fc-feat-type)))

(defclass gear (unbound-initialize)
  ((id :type (or null integer) :accessor fc-gear-id :initarg :id)
   (name :type string
	 :accessor fc-gear-name)
   (override-effect :type (or string null)
	      :accessor fc-gear-effect)
   (override-sz/hand :type (or string null)
		     :accessor fc-gear-sz/hand)
   (override-const :type (or string null)
		   :accessor fc-gear-const)))

(defclass fc-character (unbound-initialize)
  ((id :type (or null integer)
       :accessor fc-id)
   ;(owner :type (or null string) :initform (
   (character-name :type (or null string) :initform nil
		   :accessor fc-character-name)
   (player-name :type (or null string) :initform nil
		:accessor fc-player-name)
   (species :type (or null string) :initform nil
	    :accessor fc-species)
   (strong-attr :accessor fc-strong-attr
		:type (or null (member :str :dex :con :int :wis :cha)))
   (weak-attr :accessor fc-weak-attr
	      :type (or null (member :str :dex :con :int :wis :cha)))
   (talent :type (or null string) :initform nil
	   :accessor fc-talent)
   (specialty :type (or null string) :initform nil
	      :accessor fc-specialty)
   (classes :type list :initform (list (make-instance 'fc-class))
	    :accessor fc-classes)
   (xp :type (or null integer) :initform 0
       :accessor fc-xp)
   (gender :type (member :m :f :n) :initform :n
	   :accessor fc-gender)
   (attributes :type fc-attributes :accessor fc-attributes
	       :initform (make-instance 'fc-attributes))
   (skills :type fc-skills
	   :initform (make-instance 'fc-skills)
	   :accessor fc-skills)
   (crafting-foci :type list
		  :initform nil
		  :accessor fc-crafting-foci)
   (ride-foci :type list
	      :initform nil
	      :accessor fc-ride-foci)
   (alignment :type (or null string)
	      :initform nil
	      :accessor fc-alignment)
   (languages :type list
	      :initform nil
	      :accessor fc-languages)
   (studies :type list
	    :initform nil
	    :accessor fc-studies)
   (completed-subplots :type list
		       :initform nil
		       :accessor fc-completed-subplots)
   (incomplete-subplots :type list
			:initform nil
			:accessor fc-incomplete-subplots)
   (coin-in-hand :type (or null integer)
		 :initform nil
		 :accessor fc-coin-in-hand)
   (stake :type (or null integer)
	  :initform nil
	  :accessor fc-stake)
   (panache :type (or null integer)
	    :initform nil
	    :accessor fc-panache)
   (prudence :type (or null integer)
	     :initform nil
	     :accessor fc-prudence)
   (feats :type (and
		 list
		 (satisfies feat-list-p))
	  :initform nil
	  :accessor fc-feats)
   (class-abilities :type
		    (and
		     list
		     (satisfies ability-list-p))
		    :initform nil
		    :accessor fc-abilities)
   
   
   
   
   (armor-type :type (or null (member :partial :moderate))
	       :initform nil
	       :accessor fc-armor-type)
   (armor-name :type (or null string)
	       :initform nil
	       :accessor fc-armor-name)
   (armor-craftsmanship :type (or string null)
			:initform nil
			:accessor fc-armor-construction)
   (armor-construction :type (or string null)
		       :initform nil
		       :accessor fc-armor-construction)
   (armor-customizations :type list
			 :initform nil
			 :accessor fc-armor-customizations)
   (armor-fittings :type (or string null)
		   :initform nil
		   :accessor fc-armor-fittings)
   (reputation :type (or integer null)
	       :initform nil
	       :accessor fc-reputation)
   (heroic-reknown :type (or integer null)
		   :initform nil
		   :accessor fc-heroic-reknown)
   (military-reknown :type (or integer null)
		     :initform nil
		     :accessor fc-military-reknown)
   (noble-reknown :type (or integer null)
		   :initform nil
		   :accessor fc-noble-reknown)
   (gear :type (satisfies gear-list-p)
	 :initform nil
	 :accessor fc-gear)
   (fudges :type (satisfies fudge-list-p)
	   :accessor fc-fudges)))

(defun fc-class<= (x y)
  (and (string=
	(fc-class-class x)
	(fc-class-class y))
       (<= (fc-class-level x)
	  (fc-class-level y))))

(defun get-abilities-for-level (class-info)
  (with-accessors ((class fc-class-class) (level fc-class-level))
      class-info
    (let* ((table (gethash class +class-hash+))
	   (column
	    (position-if (lambda (x) (or (equalp x "Special")
				      (equalp x "Abilities")))
		      (car table)))
	   (raw-abilities (elt (elt table level) column))
	   (comma (position #\, raw-abilities))
	   (ability-names
	    (if comma
		(list
		 (subseq raw-abilities 0 comma)
		 (subseq raw-abilities (1+ comma)))
		(list raw-abilities))))
      (loop for item in ability-names
	   for item-stripped = (string-trim '(#\Space) item)
	   unless (equalp item "Bonus Feat")
	   collect (make-instance 'ability-info :name item-stripped
				  :from class-info)))))

(defmethod (setf fc-classes) :around (new-value (character fc-character))
  (call-next-method)
  (setf (fc-abilities character)
	(remove-if-not (lambda (x)
			 (some (alexandria:curry #'fc-class<= x)
			       new-value))
		       (fc-abilities character)
		       :key #'fc-ability-from))
  (loop for class in new-value
     do
       (loop for i from 1 to (fc-class-level class)
	  for level = (make-instance 'fc-class
				     :class (fc-class-class class)
				     :level i)
	  unless (member level
			 (fc-abilities character)
			 :key #'fc-ability-from
			 :test #'fc-class-equal)
	  do (loop for ability in (get-abilities-for-level level)
		do (push ability (fc-abilities character))))))





(defun class-name-p (x)
  (member x +class-names+ :test #'equalp))

(deftype a-class () '(and string (satisfies class-name-p)) )

(defmacro define-skills-class ()
    `(defclass fc-skills ()
       ((origin-skills :type t
		       :accessor fc-origin-skills
		       :initform nil)
       ,@(loop for skill in +skills+
	      for askill = 
	    collect `(,(intern (format nil "~a-RANKS" skill) :wl-fccs)
		       :accessor ,(intern (format nil "FC-~A-RANKS" skill) :wl-fccs)
		       :type (or null integer))
	    collect `(,(intern (format nil "~a-MISC-BONUS" skill) :wl-fccs)
		       :accessor ,(intern (format nil "FC-~A-MISC-BONUS" skill))
		       :type (or null integer))
	    collect `(,(intern (format nil "~a-THREAT" skill) :wl-fccs)
		       :accessor ,(intern (format nil "FC-~A-THREAT" skill) :wl-fccs)
		       :type (or null string))))))

(define-skills-class)
	 
#+(or)(defclass fc-skills ()
  ((foo :accessor skill-foo :type (or null integer))
   (bar :accessor skill-bar :type (or null integer))))

(defclass fc-attributes (unbound-initialize)
  ((str :type (or null integer)
	:initform 8
	:accessor fc-attr-str)
   (dex :type (or null integer)
	:initform 8
	:accessor fc-attr-dex)
   (con :type (or null integer)
	:initform 8
	:accessor fc-attr-con)
   (int :type (or null integer)
	:initform 8
	:accessor fc-attr-int)
   (wis :type (or null integer)
	:initform 8
	:accessor fc-attr-wis)
   (cha :type (or null integer)
	:initform 8
	:accessor fc-attr-cha)))

(defclass fc-class ()
  ((id :type (or null integer) :accessor fc-class-id :initarg :id)
   (class :type a-class :initform (first +class-names+)
	  :accessor fc-class-class
	  :initarg :class)
   
   (level :type (or null integer) :initform 1
	  :accessor fc-class-level
	  :initarg :level)))

(defun feat-list-p (x)
  (every (lambda (x) (typep x 'feat-info)) x))

(defun fudge-list-p (x)
  (every (lambda (x) (typep x 'fudge)) x))

(defun ability-list-p (x)
  (every (lambda (x) (typep x 'ability-info)) x))

(defun gear-list-p (x)
  (every (lambda (y) (typep y 'gear)) x))



;;;This should allow us to upgrade by adding slots to fc-character
(defmethod slot-unbound (class (instance unbound-initialize) name)
  ;; when we've got unbound slot
  ;; try reinitializing it with initform via shared initialize
  (shared-initialize instance (list name))
  (if (slot-boundp instance name)
      ;; if it becomes bound, call slot-value once again.
      (slot-value instance name)
      ;; otherwise call next method which signals error
      (call-next-method)))

(defmethod slot-missing (class (obj fc-character) name operation &optional new-val)
  (declare (ignore new-val))
  (cond
    ((eql operation 'slot-boundp)
     nil)
    (t (call-next-method))))

(defun attr-mod (character attr)
  (let ((val (calculate-field attr character)))
    (when val
      (floor (- val 10) 2))))
(defun calculate-column (character column &aux (column (string column)))
  (loop for item in (fc-classes character)
       for (class level) = (list
			    (string (fc-class-class item))
			    (fc-class-level item))
       for class-table = (cdr (assoc class fccg::+class-tables+ :key #'string :test #'equalp))
       for column-id = (position column (first class-table) :test #'equalp)
       sum (parse-integer (elt (elt class-table level) column-id))))

(defun maybe-emit-value (name form stream &optional (fancy (function identity)))
  (let ((value form))
    (when value
      (cl-who:with-html-output (stream)
       (:field :name name
	       (:value (princ (funcall fancy value) stream)))))))

(defun loose-sum (&rest args)
  (when (some #'identity args)
    (apply #'+ (remove nil args))))


(defparameter *path-to-charsheet* "/home/aidenn/Downloads/Fantasy_Craft_Character_Sheets-v6-Fillable.pdf")





(defun fmt-bonus (n)
  (if (> n 0)
      (format nil "+~A" n)
      (format nil "~A" n)))



(defun calculate-skill (skill character &aux (skill (string-upcase (string skill))))
  (let ((ranks (funcall (intern (format nil "FC-~A-RANKS" skill) :wl-fccs)
			(fc-skills character)))
	(attr-bonus (get-skill-attr-bonus skill character))
	(misc-bonus (funcall (intern (format nil "FC-~A-MISC-BONUS" skill) :wl-fccs)
			     (fc-skills character)))
	(threat (funcall (intern (format nil "FC-~a-THREAT" skill) :wl-fccs)
			 (fc-skills character))))
    (values
     (loose-sum ranks attr-bonus misc-bonus) ranks attr-bonus misc-bonus threat)))

(defun emit-skills-fields (skill abbrev character s
			   &aux (skill (string-upcase (string skill))))
  (multiple-value-bind
	(total ranks attr-bonus misc-bonus threat)
      (calculate-skill skill character)
    (maybe-emit-value (format nil "SkillCheck~a" abbrev)
		      (member (alexandria:make-keyword skill)
			      (fc-origin-skills (fc-skills character))
			      :test #'eql)s (constantly "Yes"))
    (maybe-emit-value (format nil "SkillBonus~a" abbrev) total s)
    (maybe-emit-value (format nil "SkillRanks~A" abbrev) ranks s)
    (maybe-emit-value (format nil "SkillAttrMod~A" abbrev) attr-bonus s)
    (maybe-emit-value (format nil "SkillMiscMod~A" abbrev) misc-bonus s)
    (maybe-emit-value (format nil "SkillThreat~A" abbrev) threat s)))

(defun generate-ability-table (character)
  (tt:table (:col-widths '(90 400) :splittable-p t)
    (tt:header-row ()
      (tt:cell ()
	(tt:paragraph () "Feature Name"))
      (tt:cell ()
	(tt:paragraph () "Description")))
    (loop for item in (fc-feats character)
	 do
	 (with-accessors ((name fc-feat-name )
			  (parameter fc-feat-parameter))
	     item
	   (tt:row ()
	     (tt:cell ()
	       (tt:paragraph (:font "times-roman")
		 #-(or)(tt::put-string (format nil "~A~@[ (~A)~]"
					 (string-capitalize
					  (substitute  #\Space #\-
						      (fix-unicode (string name))))
					 parameter))
		 ))
	   (tt:cell ()
	     (loop for paragraph in
		  (split-sequence #\Newline
				  (fix-unicode (third (gethash name +feat-hash+))))
		when (and parameter
			  (search (cl-ppcre:regex-replace " ?\\([^)]*\\)"
							  parameter
							  "")
				  (subseq paragraph 0
					  (min (length paragraph)
					       (+ (length parameter) 3)))))
		do (tt:paragraph (:font "times-bold")
		     (tt:put-string paragraph))
		else
		do (tt:paragraph (:font "times-roman")
		     (tt:put-string paragraph)))))))
    (loop for item in (fc-abilities character)
       do
	 (with-accessors ((name fc-ability-name )
			  (parameter fc-ability-parameter))
	     item
	   (tt:row ()
	     (tt:cell ()
	       (tt:paragraph (:font "times-roman")
		 #-(or)(tt::put-string (format nil "~A~@[ (~A)~]"
					       (string-capitalize
						(substitute  #\Space #\-
							     (fix-unicode (string name))))
					       parameter))
		 ))
	     (tt:cell ()
	       (loop for paragraph in
		    (split-sequence #\Newline
				    (fix-unicode (gethash name +abilites-hash+)))
		  when
		    (and parameter
			 (search (cl-ppcre:regex-replace " ?\\([^)]*\\)"
							 parameter
							 "")
				 (subseq paragraph 0
					 (min (length paragraph)
					      (+ (length parameter) 3)))))
		  do (tt:paragraph (:font "times-bold")
		       (tt:put-string paragraph))
		  else
		  do (tt:paragraph (:font "times-roman")
		       (tt:put-string paragraph)))))))
    ))

(defun fix-unicode (x)
    (map 'string
	 (lambda (x)
	   (case x
	     (#\Right_single_quotation_mark #\')
	     ((#\Left_double_quotation_mark
	       #\Right_double_quotation_mark)
	      #\")
	     ((#\Multiplication_sign) #\x)
	     (t x))) x))

(defun calculate-size-mod (species)
  (let* ((species-info
	 (gethash species +species-hash+))
	(size
	 (cdr (assoc :size species-info))))
    (case size
      (:small 1)
      (:medium 0)
      (:large -1))))

(defun calculate-class-points (character attribute)
  (loop for item in (fc-classes character)
       sum (* (fc-class-level item)
	      (cdr (assoc attribute (gethash (fc-class-class item) +class-info-hash+))))))

(defun calculate-wounds (character)
  (let ((mult 
	 (cdr (assoc :wound-multiplier
	      (gethash (fc-species character) +species-hash+)))))
    (if (< mult 1)
	(ceiling (true-score character :con) (/ mult))
	(floor (true-score character :con) (/ mult)))))

(defun get-species-item (species item)
  (cdr (assoc item
	      (gethash (string species) +species-hash+))))

(defparameter *fudges* nil)

(defmacro deffudge (field (vname) &body b)
  `(progn
    (push ,field *fudges*)
    (defmethod calculate-field ((field (eql ,field)) ,vname)
	,@b)))

(defun get-fudge-total (field character)
  (let ((fudges
	 (remove-if-not (curry #'eql field) (fc-fudges character) :key #'fudge-name)))
    (reduce #'+ fudges :key #'fudge-value)))

(defmethod calculate-field :around (field (character fc-character))
  (let ((orig (call-next-method)))
    (when (numberp orig)
      (+ (get-fudge-total field character) orig))))

(deffudge :unarmed-misc (character)
  (+
   (if (member :unarmed (fc-proficiencies character))
       0 -4)
   (if (member :unarmed (fc-fortes character))
       1 0)))

(deffudge :melee-misc (character)
  0)

(deffudge :ranged-misc (character)
  0)

(defun fc-career-level (character)
  (apply #'loose-sum (mapcar #'fc-class-level (fc-classes character))))

(deffudge :starting-action-dice (character)
  (and (fc-career-level character)
       (parse-integer
	(first (split-sequence #\Space
			       (elt (elt fccg::+career-levels+ (fc-career-level character))
				    2))))))

#.(loop for item in '(:str :dex :con :int :wis :cha)
       collect
       `(deffudge ,item (character)
	  (true-score character ,item)) into defns
       collect
       `(deffudge (make-keyword ,(format nil "~A-~A" item :mod)) (character)
	  (attr-mod character ,item)) into defns
       finally (return `(progn ,@defns)))

(deffudge :total-studies (character)
  (length (fc-studies character)))

(deffudge :base-speed (character)
  (and (fc-species character)
       (get-species-item (fc-species character) :base-speed)))

(deffudge :travel-speed (character)
  (and (calculate-field :base-speed character)
       (ceiling (calculate-field :base-speed character) 10)))

(deffudge :defense-size-mod (character)
  (and (fc-species character)
       (calculate-size-mod (fc-species character))))

(deffudge :reflex-save-misc (character) 0)
(deffudge :fortitude-save-misc (character) 0)
(deffudge :will-save-misc (character) 0)

(defun calculate-armor-info (character attr)
  (with-slots (armor-name armor-craftsmanship armor-customizations armor-type
			  armor-fittings)
      character
    (when (and armor-name armor-type)
      (let ((start
	     (loose-sum
	      (parse-integerish
	       (lookup-gear armor-name ATTR :armor (eql armor-type :moderate)))
	      (when armor-craftsmanship
		(parse-integerish
		 (lookup-gear armor-craftsmanship ATTR :armor-upgrades)))
	      
	      (when armor-fittings
		(parse-integerish
		 (lookup-gear armor-fittings ATTR :armor)))))
	    (custom
	     (loop for item in armor-customizations
		collect (lookup-gear item ATTR :armor-upgrades))))
	(if (some (curry #'find #\%) custom)
	    (loop
	       with product = start
	       for item in (mapcar #'parse-integerish custom)
	       do (setf product (* product (1+ (/ item 100))))
		 finally (return (floor product)))
	    (loop with sum = start
	       for item in (mapcar #'parse-integerish custom)
		 do (incf sum item)
		 finally (return sum)))))))
		 
(deffudge :armor-dp (character)
  (calculate-armor-info character "DP"))

(deffudge :armor-dr (character)
  (calculate-armor-info character "DR"))

(deffudge :armor-acp (character)
  (calculate-armor-info character "ACP"))

(deffudge :armor-speed (character)
  (calculate-armor-info character "Speed"))

(defun get-size (character)
  (let*((species-info
	 (gethash (fc-species character) +species-hash+)))
	 (cdr (assoc :size species-info))))

(deffudge :armor-weight (character)
  (when (and
	 (calculate-armor-info character (curry #'starts-with-subseq "Weight"))
	 (fc-species character))
    (floor (calculate-armor-info character (curry #'starts-with-subseq "Weight"))
	   (let* ((species-info
		   (gethash (fc-species character) +species-hash+))
		  (size
		   (cdr (assoc :size species-info))))
	     (ecase size
	       (:small 2)
	       (:medium 1)
	       (:large 1/3))))))

(deffudge :armor-disguise (character)
  (calculate-armor-info character "Disguise"))
  
(deffudge :standard-attack (character)
  (loose-sum
   (calculate-column character "BAB")
   (calculate-field :melee-misc character)
   (calculate-field :str-mod character)))

(deffudge :pummel (character)
    (loose-sum
     (calculate-column character "BAB")
     (calculate-field :unarmed-misc character)
     (calculate-field :str-mod character)))

(deffudge :bull-rush (character)
  (calculate-skill :Athletics character))

(deffudge :feint (character)
 (calculate-skill :prestidigitation character)) 

(deffudge :grapple (character)
  (calculate-skill :Athletics character))

(deffudge :taunt (character)
  (calculate-skill :|SENSE MOTIVE| character))

(deffudge :threaten (character)
  (calculate-skill :intimidate character))

(deffudge :tire (character)
  (calculate-skill :resolve character))

(deffudge :trip (character)
  (calculate-skill :acrobatics character))

(deffudge :standard-move (character)
  (calculate-field :base-speed character))

(deffudge :run (character)
  (and
   (calculate-field :base-speed character)
   (* (calculate-field :base-speed character) 4)))

(deffudge :total-defense-move (character)
  (calculate-field :standard-move character))

(defun emit-fudge (character pdfname calc-name stream)
  (maybe-emit-value pdfname (calculate-field calc-name character) stream))

(deffudge :light-capacity (character)
  (let ((sizemod 
	 (case (get-size character)
	   (:small -1)
	   (:medium 0)
	   (:large 1)))
	(str (calculate-field :str character)))
    (when (or (member :improved-stability
		      (cdr (assoc :qualities (gethash (fc-species character) +species-hash+))))
	      (member :improved-stability
		      (cdr (assoc :qualities (gethash (fc-talent character) +talent-hash+)))))
      (incf sizemod))
    (if (> sizemod 0)
	(capacity-for-effective-str (+ str (* sizemod 5)))
	(capacity-for-effective-str (+ str (* sizemod 2))))))

(deffudge :heavy-capacity (character)
  (* 3 (calculate-field :light-capacity character)))
		      
(deffudge :lift-capacity (character)
  (* 2 (calculate-field :heavy-capacity character)))

(deffudge :push-drag-capacity (character)
  (* 2 (calculate-field :heavy-capacity character)))

(deffudge :legend (character)
  (calculate-column character "Legend"))

(deffudge :total-reknown (character)
  (loose-sum
   (fc-heroic-reknown character)
   (fc-noble-reknown character)
   (fc-military-reknown character)))

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

(defun genxfdf (character)
  (let ((career-level (apply #'loose-sum (mapcar #'fc-class-level (fc-classes character)))))
    (uiop:with-temporary-file (:stream s :pathname path)
      (cl-who:with-html-output (s nil :prologue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
	(:xfdf :|XML:SPACE| "preserve"
	       (:fields
		(maybe-emit-value "HeroFirstClassLevel" (first (fc-classes character)) s)
		(maybe-emit-value "HeroPlayerName" (fc-player-name character) s)
		(maybe-emit-value "HeroGender" (fc-gender character) s)
		;;(maybe-emit-value "HeroAge" ...)
		;;(maybe-emit-value "HeroHeight" ...)
		(maybe-emit-value "HeroSpeciesTalent" (fc-species character) s
				  (lambda (x) (format nil "~A~@[/~A~]" x (fc-talent character))))
		(maybe-emit-value "HeroSecondClassLevel" (second (fc-classes character)) s))
	       (maybe-emit-value "HeroCurrentXP" (fc-xp character) s)
	       ;;(maybe-emit-value "HeroHair" ... s)
	       (maybe-emit-value "HeroSpeciality" (fc-specialty character) s)
	       (maybe-emit-value "HeroThirdClassLevel" (third (fc-classes character)) s)
	       (maybe-emit-value "HeroNextLevel" career-level s #'calculate-next-xp)
	       ;;(maybe-emit-value "HeroWeight" ... s)
	       ;;(maybe-emit-value "HeroEyes" ... s)
	       (maybe-emit-value "ActionDiceType" career-level s
				 (lambda (x)
				   (cl-ppcre:register-groups-bind
				       (die)
				       ('(:sequence "("
					  (:register 
					   (:greedy-repetition 1 nil
					    (:inverted-char-class #\)))))
					 (elt (elt fccg::+career-levels+ x)
					      2))
				     die)))
	       (emit-fudge character "ActionDiceStarting" :starting-action-dice s)
	       (emit-fudge character "AbilityStrScore" :str s)
	       (emit-fudge character "AbilityStrMod" :str-mod s)
	       ;;Should I have impaired abilities? I think not for now.
	       (emit-fudge character "AbilityDexScore" :dex s)
	       (emit-fudge character "AbilityDexMod" :dex-mod s)
	       (emit-fudge character "AbilityConScore" :con s)
	       (emit-fudge character "AbilityConMod" :con-mod s)
	       (emit-fudge character "AbilityIntScore" :int s)
	       (emit-fudge character "AbilityIntMod" :int-mod s)
	       (emit-fudge character "AbilityWisScore" :wis s)
	       (emit-fudge character "AbilityWisMod" :wis-mod s)
	       (emit-fudge character "AbilityChaScore" :cha s)
	       (emit-fudge character "AbilityChaMod" :cha-mod s)
	       (maybe-emit-value "SkillMaxRanks" career-level s
				 (lambda (x)
				   (elt (elt fccg::+career-levels+ x) 3)))
	       (loop for (skill abbrev) on
		    '("Acrobatics" "Acro"
		      "Athletics" "Ath"
		      "Blend" "Blend"
		      "Bluff" "Bluff"
		      "Crafting" "Craft"
		      "Disguise" "Dis"
		      "Haggle" "Hagg"
		      "Impress" "Imp"
		      "Intimidate" "Intim"
		      "Investigate" "Inv"
		      "Medicine" "Med"
		      "Notice" "Not"
		      "Prestidigitation" "Pres"
		      "Resolve" "Res"
		      "Ride" "Ride"
		      "Search" "Search"
		      "Sense Motive" "Sense"
		      "Sneak" "Sneak"
		      "Survival" "Sur"
		      "Tactics" "Tac"
		      ) by #'cddr
		    do (emit-skills-fields skill abbrev character s))
	       (loop for n from 1 to 4
		    do (maybe-emit-value (format nil "FocusesCrafting~D" n)
					 (nth (1- n) (fc-crafting-foci character)) s))
	       (loop for n from 1 to 4
		  do (maybe-emit-value (format nil "FocusesRide~D" n)
				       (nth (1- n) (fc-ride-foci character)) s))
	       ;;NB despite the name, this is actually the total studies,
	       ;;not the total interests; the value is used for knowledge
	       ;;checks
	       (emit-fudge character "InterestsTotal" :total-studies s)
	       (loop
		    with alignment = (and (fc-alignment character)
					  (list (fc-alignment character)))
		    with languages = (copy-list (fc-languages character))
		    with studies = (copy-list (fc-studies character))
		  for n from 1 to 10
		    do (maybe-emit-value (format nil "Interests~d" n)
					 (or
					  (when alignment
					    (format nil "Alignment: ~A" (pop alignment)))
					  (when languages
					    (format nil "Language: ~A" (pop languages)))
					  (when studies
					    (format nil "Study: ~A" (pop studies))))
					 s))
	       (loop with complete = (fc-completed-subplots character)
		    with incomplete = (fc-incomplete-subplots character)
		    for n from 1 to 6
		    when (and (not incomplete)
			      complete)
		    do (maybe-emit-value (format nil "SubplotsCheck~D" n) "Yes" s)
		    do (maybe-emit-value (format nil "Subplots~D" n)
					 (or (pop incomplete)
					     (pop complete))s))
	       (maybe-emit-value "CoinInHand" (fc-coin-in-hand character)s)
	       (maybe-emit-value "CoinStake" (fc-stake character)s)
	       (maybe-emit-value "LifestyleTotal"
				 (loose-sum (calculate-field  :cha-mod character)
					    (calculate-column character "Lifestyle"))s)
	       (maybe-emit-value "LifestylePanache" (fc-panache character)s)
	       (maybe-emit-value "LifestyleAppBonus" (fc-panache character) s
				 (lambda (x) (floor (1+ x) 3)))
	       (maybe-emit-value "LifestyleIncome" (fc-panache character)
				 s (lambda (x) (* x 10)))
	       (maybe-emit-value "LifestylePru" (fc-prudence character)s)
	       (maybe-emit-value "LifestyleMoneySavEarn" (fc-prudence character)
				 s (lambda (x) (format nil "~D%" (+ 15 (* x 5)))))
	       (maybe-emit-value "DefenseMiscMod" (fc-defense-misc-mod character)s)
	       (emit-fudge character "DefenseArmMod" :armor-dp s)
	       (emit-fudge character "DefenseSizeMod" :defense-size-mod s)
	       (maybe-emit-value "DefenseClassBonus"
				 (and (fc-classes character) character)
				 s
				 (lambda (character)
				   (calculate-column character "Def")))
	       (emit-fudge character "DefenseDexMod" :dex-mod s)
	       (maybe-emit-value "DefenseTotal"
				 (loose-sum
				  10
				  (fc-defense-misc-mod character)
				  (calculate-field :armor-dp character)
				  (calculate-field :dex-mod character)
				  (calculate-field :defense-size-mod character)
				  (calculate-column character "Def"))s)
	       (maybe-emit-value "VitalityTotal" career-level s
				 (lambda (x)
				   (+ (calculate-class-points character :vitality)
				      (* x (calculate-field :con-mod character)))))
	       (maybe-emit-value "WoundsTotal" (and
					       (true-score character :con)
					       (fc-species character)
					       character)
				s
				#'calculate-wounds)
	       (maybe-emit-value "SizeReach" (fc-species character)
				 s (rcurry #'get-species-item :reach))
	       (maybe-emit-value "SizeFootprint" (fc-species character)
				 s
				 (lambda (species)
				   (format nil
					   "~{~A~^x~}"
					   (split-sequence
					    #\Multiplication_sign
					    (string-trim '(#\( #\))
							 (get-species-item species :footprint)))))
				 #+#:}(rcurry #'get-species-item :footprint))
	       (maybe-emit-value "SizeSize" (fc-species character)
				 s (rcurry #'get-species-item :size))
	       (emit-fudge character "GrdSpdBase" :base-speed s)
	       (emit-fudge character "TravelSpdMPH" :travel-speed s)
	       (format *error-output* "~%Prof: ~S" (fc-proficiencies character))
	       (loop for pdf in '("Unarmed" "Blunt" "Edged" "Hurled" "Bows" "BlackPow" "Siege")
		  for key in +proficiencies+
		  do (maybe-emit-value (format nil "Prof~A" pdf)
				       (member key (fc-proficiencies character)) s
				 (lambda (x) (and x "Yes")))
		   (maybe-emit-value (format nil "Prof~AForte" pdf)
				       (member key (fc-fortes character)) s
				 (lambda (x) (and x "Yes"))))
	       (emit-fudge character "InitDexMod" :dex-mod s)
	       (maybe-emit-value "InitMiscMod" (fc-init-misc-mod character) s)
	       (maybe-emit-value "InitTotal"
				 (loose-sum
				  (fc-init-misc-mod character)
				  (calculate-field :dex-mod character)
				  (calculate-column character "Init"))s)
	       (maybe-emit-value "InitClassBonus" (calculate-column character "Init")s)
	       (maybe-emit-value "BaseAttUnarmedTotal" (calculate-column character "BAB")
				 s
				 (lambda (bab)
				   (loose-sum
				    bab
				    (calculate-field :unarmed-misc character)
				    (calculate-field :str-mod character))))
	       (maybe-emit-value "BaseAttUnarmedBAB" (calculate-column character "BAB")s)
	       (emit-fudge character "BaseAttUnarmedAttrMod" :str-mod s)
	       (emit-fudge character "BaseAttUnarmedMiscMod" :unarmed-misc s)
	       (maybe-emit-value "BaseAttMeleeTotal" (calculate-column character "BAB")
				 s
				 (lambda (bab)
				   (loose-sum
				    bab
				    (calculate-field :melee-misc character)
				    (calculate-field :str-mod character))))
	       (maybe-emit-value "BaseAttMeleeBAB" (calculate-column character "BAB")s)
	       (emit-fudge character "BaseAttMeleeAttrMod" :str-mod s)
	       (emit-fudge character "BaseAttMeleeMiscMod" :melee-misc s)
	       (maybe-emit-value "BaseAttRangedTotal" (calculate-column character "BAB")
				 s
				 (lambda (bab)
				   (loose-sum
				    bab
				    (calculate-field :ranged-misc character)
				    (calculate-field :dex-mod character))))
	       (maybe-emit-value "BaseAttRangedBAB" (calculate-column character "BAB")s)
	       (emit-fudge character "BaseAttRangedAttrMod" :str-mod s)
	       (emit-fudge character "BaseAttRangedMiscMod" :ranged-misc s)
	       (maybe-emit-value "SavThrFortTotal" (calculate-column character "Fort")s
				 (lambda (sav)
				   (loose-sum sav
					      (calculate-field :con-mod character)
					      (calculate-field :fortitude-save-misc character))))
	       (maybe-emit-value "SavThrFortBase" (calculate-column character "Fort")s)
	       (emit-fudge character "SavThrFortAttrMod" :con-mod s)
	       (emit-fudge character "SavThrFortMiscMod" :fortitude-save-misc s)
	       (maybe-emit-value "SavThrRefTotal" (calculate-column character "Ref")s
			   (lambda (sav)
			     (loose-sum sav
			     (calculate-field :dex-mod character)
			     (calculate-field :reflex-save-misc character))))
	       (maybe-emit-value "SavThrRefBase" (calculate-column character "Ref")s)
	       (emit-fudge character "SavThrRefAttrMod" :dex-mod s)
	       (emit-fudge character "SavThrRefMiscMod" :reflex-save-misc s)
	       (maybe-emit-value "SavThrWillTotal" (calculate-column character "Will")s
			   (lambda (sav)
			     (loose-sum sav
			     (calculate-field :wis-mod character)
			     (calculate-field :will-save-misc character))))
	       (maybe-emit-value "SavThrWillBase" (calculate-column character "Will")s)
	       (emit-fudge character "SavThrWillAttrMod" :wis-mod s)
	       (emit-fudge character "SavThrWillMiscMod" :will-save-misc s)
	       ;;TODO Arms (see Weapon1Type)
	       (maybe-emit-value "ArmorType" (and (fc-armor-type character)
						  (fc-armor-name character))
				 s
				 (lambda (name)
				   (string-capitalize
				   (format nil "~A ~A" (fc-armor-type character) name))))
	       (emit-fudge character "ArmorDR" :armor-dr s)
	       (emit-fudge character "ArmorDP" :armor-dp s)
	       (emit-fudge character "ArmorACP" :armor-acp s)
	       (emit-fudge character "ArmorSpd" :armor-speed s)
	       (emit-fudge character "ArmorWgt" :armor-weight s)
	       (emit-fudge character "ArmorDis" :armor-disguise s)
	       (emit-fudge character "ActionStandAtt" :standard-attack s)
	       (emit-fudge character "ActionBull" :bull-rush s)
	       (maybe-emit-value "ActionCoup" "N/A" s)
	       (emit-fudge character "ActionFeint" :feint s)
	       (emit-fudge character "ActionGrap" :grapple s)
	       (emit-fudge character "ActionPum" :pummel s)
	       (emit-fudge character "ActionTaunt" :taunt s)
	       (emit-fudge character "ActionThr" :threaten s)
	       (emit-fudge character "ActionTire" :tire s)
	       (emit-fudge character "ActionTrip" :trip s)
	       (emit-fudge character "ActionStandMov" :standard-move s)
	       (emit-fudge character "ActionRun" :run s)
	       (emit-fudge character "ActionTotDef" :total-defense-move s)
	       (loop
		    with n = 1
		  for (name type parameter note) in
		    (append
		     (mapcar (lambda (x) (list
					  (pprint-keyword
					   (fc-feat-name x))
					  (fc-feat-type x)
					  (fc-feat-parameter x)
					  "Feat"))
			     (fc-feats character))
		     (mapcar (lambda (x) (list (fc-ability-name x)
					       (fc-ability-type x)
					       (fc-ability-parameter x)
					       (fc-ability-from x)))
			     (fc-abilities character)))
		    when (eql type :non-combat)
		    do
		    (maybe-emit-value (format nil "NonCombatAbName~D" n)
				      name s)
		    (maybe-emit-value (format nil "NonCombatAbNotes~D" n)
				      (format nil "~@[(~A) ~] ~A" parameter note) s)
		    (incf n))
	      (loop
		    with n = 1
		  for (name type parameter note) in
		    (append
		     (mapcar (lambda (x) (list
					  (pprint-keyword
					   (fc-feat-name x))
					  (fc-feat-type x)
					  (fc-feat-parameter x)
					  "Feat"))
			     (fc-feats character))
		     (mapcar (lambda (x) (list (fc-ability-name x)
					       (fc-ability-type x)
					       (fc-ability-parameter x)
					       (fc-ability-from x)))
			     (fc-abilities character)))
		    when (eql type :combat)
		    do
		    (maybe-emit-value (format nil "CombatAbName~D" n)
				      name s)
		    (maybe-emit-value (format nil "CombatAbNotes~D" n)
				      (format nil "~@[(~A) ~] ~A" parameter note) s)
		    (incf n))
	      (emit-fudge character "CarCapHvy" :heavy-capacity s)
	      (emit-fudge character "CarCapLight" :light-capacity s)
	      (emit-fudge character "CarCapLift" :lift-capacity s)
	      (emit-fudge character "CarCapPushDrag" :push-drag-capacity s)
	      (emit-fudge character "RepRenLegend" :legend s)
	      (maybe-emit-value (fc-reputation character) "RepRenRep" s)
	      (emit-fudge character "RepRenRen" :total-reknown s)
	      (maybe-emit-value (fc-heroic-reknown character) "RepRenHeroRen" s)
	      (maybe-emit-value (fc-military-reknown character) "RepRenMilRen" s)
	      (maybe-emit-value (fc-noble-reknown character) "RepRenNobleRen" s)
	       ))
      :close-stream
      (uiop:with-temporary-file (:stream table-stream :pathname table-pathname)
	(locally
	    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	  (tt:with-document ()
	    (let* ((tt::*default-font* (pdf:get-font "Times-Roman"))
		   (tt::*default-font-size* 12)
		   (tt::*default-page-size* :letter))
	      (tt:draw-pages 
	       (tt:compile-text (:text-style (make-instance 'tt::text-style
							    :font tt::*default-font*
							    :font-size tt::*default-font-size*
							    :h-align :justify))
		 (generate-ability-table character))
	       :margins '(36 36 36 36)
	       :break :after))
	    (pdf:write-document table-stream)))
	:close-stream
	(let ((filled-pdf
	       (uiop:run-program `("pdftk" ,*path-to-charsheet* "fill_form" ,(namestring path) "output" "-")
				 :input "" :output :string :external-format :iso-8859-1)))
	  (uiop:run-program `("pdftk" "-" ,(namestring table-pathname) "cat" "output" "-")
				 :input (make-string-input-stream filled-pdf) :output :string :external-format :iso-8859-1))))))
|#

				  
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
       (write-string (ps:ps-compile-file "/Users/jmiller/src/lisp/cl-fccs/src/ps-compat.lisp") f)
       (write-string (ps:ps-compile-file "/Users/jmiller/src/lisp/cl-fccs/src/model.lisp") f)
       (write-string (ps:ps-compile-file "/Users/jmiller/src/lisp/cl-fccs/src/view-macros.lisp") f)
       (write-string (ps:ps-compile-file "/Users/jmiller/src/lisp/cl-fccs/src/view.lisp") f))))
  (log:info
   (uiop:run-program
    (namestring (uiop:merge-pathnames* "gh/redo/redo" (user-homedir-pathname)))
     :directory (asdf:system-relative-pathname 'cl-fccs "build/")
     :output :string :error-output :output)))
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
