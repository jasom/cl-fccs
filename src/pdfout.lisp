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
		(emit-field "InitTotal" :initiative))))))
		#|
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
|#

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
	       #-or()
	       #+or(generate-ability-table character))
	     :margins '(36 36 36 36)
	     :break :after))
	  (pdf:write-document table-stream)))
      :close-stream
      (let ((filled-pdf
	     (uiop:run-program `("pdftk" ,*path-to-charsheet* "fill_form" ,(namestring path) "output" "-")
			       :input "" :output :string :external-format :iso-8859-1)))
	(uiop:run-program `("pdftk" "-" ,(namestring table-pathname) "cat" "output" "-")
			  :input (make-string-input-stream filled-pdf) :output :string :external-format :iso-8859-1)))))



(cl-interpol:disable-interpol-syntax)
