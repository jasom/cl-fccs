(in-package :cl-fccs)
(cl-interpol:enable-interpol-syntax)

(defparameter *path-to-charsheet* "/home/aidenn/Downloads/Fantasy_Craft_Character_Sheets-v6-Fillable.pdf")

(defvar *character*)
(defvar *out-stream*)

(defun emit-field (name fname)
  (emit-value name (calculate-field fname *character*)))

(defun nestring (string)
  (unless (emptyp string)
    string))

(defun emit-item (name fname)
  (let ((val
	 (loop
	    with item
	    for field in (ensure-list fname)
	    for obj = *character* then item
	    do (setf item  (aget field obj))
	    finally (return item))))
    (emit-value name val)))

(defun emit-list (outputter list)
  (if
   (stringp outputter)
   (emit-list (lambda (item n)
		(emit-value (format nil outputter n) item))
	      list)
   (loop for item in list
      for n from 1
	do (funcall outputter item n))))

(defun emit-value (name value)
  (when value
    (cl-who:with-html-output (*out-stream*)
      (:field :name name
	      (:value (princ value *out-stream*))))))

(defun emit-skills-fields (skill abbrev
			   &aux (skill (string-upcase (string skill))))
  (when
      (aget (key-fmt :~A-origin skill) *character*)
    (emit-value (format nil "SkillCheck~a" abbrev) "Yes"))
  (emit-field (format nil "SkillBonus~a" abbrev) (key-fmt :~A-bonus skill))
  (emit-item (format nil "SkillRanks~A" abbrev) (key-fmt :~A-ranks skill))
  (emit-field (format nil "SkillAttrMod~A" abbrev) (key-fmt :~a-attr-bonus skill))
  (emit-field (format nil "SkillMiscMod~A" abbrev) (key-fmt :~a-misc-bonus skill))
  (emit-item (format nil "SkillThreat~A" abbrev) (key-fmt :~a-threat skill)))

(defun fmt-class (cl)
  (and cl
       (format nil "~A/~A"
	       (aget :the-class cl)
	       (aget :level cl))))

(defun genxfdf (character *out-stream*)
  (let ((*character* character))
    (cl-who:with-html-output (*out-stream* nil :prologue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
	(:xfdf :|XML:SPACE| "preserve"
	       (:fields
		(emit-value "HeroFirstClassLevel"
			    (fmt-class (first (aget :classes character))))
		(emit-item "HeroCharacterName" :character-name)
		(emit-item "HeroPlayerName" :player-name)
		(emit-item "HeroGender" :gender)
		(emit-value "HeroSpeciesTalent"
			    (format nil "~A~@[/~A~]"
				    (aget :species character)
				    (nestring (aget :talent character))))
		(emit-value "HeroSecondClassLevel" (second (aget :classes character)))
		(emit-item "HeroCurrentXP" :xp)
		(emit-item "HeroHair" :hair)
		(emit-item "HeroSpeciality" :specialty)
		(emit-value "HeroThirdClassLevel" (fmt-class
						   (third (Aget :classes character))))
		(emit-field "HeroNextLevel" :next-xp)
		(emit-item "HeroWeight"  :weight)
		(emit-item "HeroEyes" :eyes)
		(emit-item "HeroAge" :age)
		(emit-item "HeroHeight" :height)
		(emit-field "ActionDiceType" :action-dice-type)
		(emit-field "ActionDiceStarting" :starting-action-dice)
		(emit-field "AbilityStrScore" :real-str)
		(emit-field "AbilityDexScore" :real-dex)
		(emit-field "AbilityConScore" :real-con)
		(emit-field "AbilityIntScore" :real-int)
		(emit-field "AbilityWisScore" :real-wis)
		(emit-field "AbilityChaScore" :real-cha)
		(emit-field "AbilityStrMod" :str-mod)
		(emit-field "AbilityDexMod" :dex-mod)
		(emit-field "AbilityConMod" :con-mod)
		(emit-field "AbilityIntMod" :int-mod)
		(emit-field "AbilityWisMod" :wis-mod)
		(emit-field "AbilityChaMod" :cha-mod)
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
		       "Sense_Motive" "Sense"
		       "Sneak" "Sneak"
		       "Survival" "Sur"
		       "Tactics" "Tac"
		       ) by #'cddr
		   do (emit-skills-fields skill abbrev))
		(emit-value "SkillMaxRanks"
			    (elt (elt fccg::+career-levels+
				      (calculate-field :career-level character)) 3))
		(emit-list "FocusesCrafting~D" (aget :crafting-foci character))
		(emit-list "FocusesRide~D" (aget :ride-foci character))
		(emit-field "InterestsTotal" :total-studies)
		(emit-list "Interests~d"
			   (append
			    (when (not (emptyp (aget :alignment character)))
			      (list #?"Alignment: $((aget :alignment character))"))
			    (mapcar (lambda (x) #?"Language: ${x}")
				    (aget :languages character))
			    (mapcar (lambda (x) #?"Study: ${X}")
				    (aget :studies character))))
		(emit-list "Subplots~d"
			   (append
			    (aget :incomplete-subplots character)
			    (aget :completed-subplots character)))
		(emit-list "SubplotsCheck~d"
			   (append
			    (make-list (length (aget :incomplete-subplots character))
				       :initial-element nil)
			    (make-list (length (aget :completed-subplots character))
				       :initial-element "Yes")))
		(emit-item "CoinInHand" :coin-in-hand)
		(emit-item "CoinStake" :stake)
		(emit-field "LifestyleTotal" :lifestyle)
		(emit-item "LifestylePanache" :panache)
		(emit-field "LifestyleAppBonus" :appearance-bonus)
		(emit-field "LifestyleIncome" :income)
		(emit-item  "LifestylePru" :prudence)
		(emit-item "LifestyleMoneySavEarn" :money-save/earned) 
		(emit-field "DefenseMiscMod" :defense-misc-mod) 
		(emit-field "DefenseArmMod" :defense-armor-mod)
		(emit-field "DefenseSizeMod" :defense-size-mod)
		(emit-field "DefenseClassBonus" :base-defense)
		(emit-field "DefenseDexMod" :defense-attr-mod)
		(emit-field "DefenseTotal" :defense)
		(emit-field "VitalityTotal":vitality)
		(emit-field "WoundsTotal":wounds)
		(emit-field "SizeReach" :reach)
		(emit-field "SizeFootprint" :footprint)
		(emit-field "SizeSize" :size)
		(emit-field "GrdSpdBase" :ground-speed)
		(emit-field "TravelSpdMPH" :travel-speed)
		(loop for pdf in '("Unarmed" "Blunt" "Edged" "Hurled" "Bows" "BlackPow" "Siege")
		   for key in +proficiencies+
		   when (member key (aget :proficiency-list character))
		   do (emit-value (format nil "Prof~a" pdf) "Yes")
		   when (member key (aget :forte-list character))
		   do (emit-value (format nil "Prof~aForte" pdf) "Yes"))
		   
		(emit-field "InitDexMod" :init-attr-modA)
		(emit-field "InitMiscMod" :init-misc-mod)
		(emit-field "InitTotal" :initiative)
		(emit-field "InitClassBonus" :base-init)
		(emit-field "BaseAttUnarmedTotal" :unarmed-bonus)
		(emit-field "BaseAttUnarmedBAB" :bab)
		(emit-field "BaseAttUnarmedAttrMod" :unarmed-attr-mod)
		(emit-field "BaseAttUnarmedMiscMod" :unarmed-misc-mod)
		(emit-field "BaseAttMeleeTotal" :melee-bonus)
		(emit-field "BaseAttMeleeBAB" :bab)
		(emit-field "BaseAttMeleeAttrMod" :melee-attr-mod)
		(emit-field "BaseAttMeleeMiscMod" :melee-misc-mod)
		(emit-field  "BaseAttRangedTotal"  :ranged-bonus)
		(emit-field "BaseAttRangedBAB" :bab)
		(emit-field "BaseAttRangedAttrMod" :ranged-attr-mod)
		(emit-field "BaseAttRangedMiscMod" :ranged-misc-mod)
		(emit-field "SavThrFortTotal" :fortitude-bonus)
		(emit-field "SavThrFortBase" :fortitude-base)
		(emit-field "SavThrFortAttrMod" :fortitude-attr-mod)
		(emit-field "SavThrFortMiscMod" :fortitude-misc-mod)
		(emit-field "SavThrRefTotal" :reflex-bonus)
		(emit-field "SavThrRefBase" :reflex-base)
		(emit-field "SavThrRefAttrMod" :reflex-attr-mod)
		(emit-field "SavThrRefMiscMod" :reflex-misc-mod)
		(emit-field "SavThrWillTotal" :will-bonus)
		(emit-field "SavThrWillBase" :will-base)
		(emit-field "SavThrWillAttrMod" :will-attr-mod)
		(emit-field "SavThrWillMiscMod" :will-misc-mod)
		(emit-value "ArmorType"
			    (format  nil "~A~@[ ~A~]"
				     (better-capitalize (aget :armor-type character))
				     (aget :armor-name character)))
		;;TODO Arms (see Weapon1Type)
		(emit-field "ArmorDR" :armor-dr)
		(emit-field "ArmorDR" :armor-dr)
		(emit-field "ArmorDP" :armor-dp)
		(emit-field "ArmorACP" :armor-acp)
		(emit-field "ArmorSpd" :armor-speed)
		(emit-field "ArmorWgt" :armor-weight)
		(emit-field "ArmorDis" :armor-disguise)
		(emit-field "ActionStandAtt" :action-attack)
		(emit-field "ActionBull" :action-bullrush)
		(emit-field  "ActionCoup" :action-coupe-de-grace)
		(emit-field "ActionFeint" :action-feint)
		(emit-field "ActionGrap" :action-grapple)
		(emit-field "ActionPum" :action-pummel)
		(emit-field "ActionTaunt" :action-taunt)
		(emit-field "ActionThr" :action-threaten)
		(emit-field "ActionTire" :action-tire)
		(emit-field "ActionTrip" :action-trip)
		(emit-field "ActionStandMov" :ground-speed)
		(emit-field "ActionRun" :run-speed)
		(emit-field "ActionTotDef" :ground-speed)
		(multiple-value-bind
		      (nc-name nc-notes c-name c-notes s-name s-notes)
			   (loop for item in
				(append (aget :feat-list character)
					(aget :ability-list character))
			      for desc = 
				(cond
				  ((aget :notes item)
				   (aget :notes item))
				  ((stringp (aget :from item))
				   (aget :from item))
				  (t (fmt-class (aget :from item))))
			      when (eql (aget :list-as item) :non-combat)
			      collect (aget :name item) into nc-name
			      and collect desc into nc-notes
			      when (eql (aget :list-as item) :combat)
			      collect (aget :name item) into c-name
			      and collect desc into c-notes
			      when (eql (aget :list-as item) :spellcasting)
			      collect (aget :name item) into s-name
			      and collect desc into s-notes
			      finally (return (values nc-name nc-notes
						      c-name c-notes
						      s-name s-notes)))
		  (emit-list "NonCombatAbName~D" nc-name)
		  (emit-list "NonCombatAbNotes~D" nc-notes)
		  (emit-list "CombatAbName~D" c-name)
		  (emit-list "CombatAbNotes~D" c-notes)
		  (emit-list "SpellAbName~D" s-name)
		  (emit-list "SpellAbNotes~D" s-notes))
		(emit-field "CarCapHvy" :heavy-capacity)
		(emit-field "CarCapLight" :light-capacity)
		(emit-field "CarCapLift" :lift)
		(emit-field "CarCapPushDrag" :push/drag)
		(emit-field "RepRenLegend" :legend)
		(emit-item "RepRenRep" :reputation)
		(emit-field "RepRenRen" :renown)
		(emit-item "RepRenHeroRen" :heroic-renown)
		(emit-item "RepRenMilRen" :military-renown)
		(emit-item "RepRenNobleRen" :noble-renown)
		(emit-list "GearName~D" (mapcar (curry #'gethash :name)
						(aget :gear character)))
		(emit-list "GearEffects~D" (mapcar (curry #'gethash :effect)
						(aget :gear character)))
		(emit-list
		 (lambda (item n)
		   (emit-value #?"GearSzHand${n}" #?"\u$((aget :size item))/$((aget :hand item))"))
		 (aget :gear character))
		(emit-list "GearWgt~D" (mapcar (curry #'gethash :weight)
						(aget :gear character)))
		)))))

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

(defun generate-ability-table (character)
  (tt:table (:col-widths '(90 400) :splittable-p t)
    (tt:header-row ()
      (tt:cell ()
	(tt:paragraph () "Feature Name"))
      (tt:cell ()
	(tt:paragraph () "Description")))
    (loop for item in (aget :feat-list character)
       for name = (aget :name item)
       for parameter = (nestring (aget :parameter item))
       do
	 (tt:row ()
	   (tt:cell ()
	     (tt:paragraph (:font "times-roman")
	       #-(or)(tt::put-string (format nil "~A~@[ (~A)~]"
					     (better-capitalize
					      (substitute  #\Space #\-
							   (fix-unicode (string name))))
					     parameter))
	       ))
	   (tt:cell ()
	     (loop for paragraph in
		  (split-sequence #\Newline
				  (fix-unicode (third (gethash (to-keyword name) +feat-hash+))))
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
		     (tt:put-string paragraph))))))
  #-(or)(loop for item in (aget :ability-list character)
	     for name = (substitute  #\Space #\-  (aget :name item))
	     for parameter = (nestring (aget :parameter item))
	   ;do (log:info name)
	   do  (tt:row ()
		 (tt:cell ()
		   (tt:paragraph (:font "times-roman")
		     (tt::put-string (format nil "~A~@[ (~A)~]"
					     (better-capitalize
							   (fix-unicode (string name)))
					     parameter))))
		 (tt:cell ()
		   (loop for paragraph in
			#-(or)(split-sequence #\Newline
					(fix-unicode (gethash name +abilites-hash+)))
			#+(or)(list "p1" "p2")
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
			   (tt:put-string paragraph))))))))

(defun fill-pdf (character)
  (uiop:with-temporary-file (:stream s :pathname path)
    (genxfdf character s)
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
	       #+or()
	       #-or(generate-ability-table character))
	     :margins '(36 36 36 36)
	     :break :after))
	  (pdf:write-document table-stream)))
      :close-stream
      (let ((filled-pdf
	     (handler-bind
		 ((uiop:subprocess-error
		   (lambda (condition)
		     (uiop:run-program `("cp" ,(namestring path) "/home/aidenn/error.tmp")))))
	       (uiop:run-program `("pdftk" ,*path-to-charsheet* "fill_form" ,(namestring path) "output" "-")
				 :input "" :output :string :external-format :iso-8859-1))))
(uiop:run-program `("pdftk" "-" ,(namestring table-pathname) "cat" "output" "-")
			  :input (make-string-input-stream filled-pdf) :output :string :external-format :iso-8859-1 :error-output :string :ignore-error-status t)
	  ))))



(cl-interpol:disable-interpol-syntax)
