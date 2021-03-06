(in-package :cl-fccs)
(declaim (optimize (speed 0) (debug 3)))
(cl-interpol:enable-interpol-syntax)

(defvar *path-to-charsheet*)

(defvar *character*)
(defvar *out-stream*)

(defparameter +formfill-path+ (asdf:system-relative-pathname :cl-fccs "build/formfill"))
(defparameter +unite-path+ (asdf:system-relative-pathname :cl-fccs "build/unitepages"))

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
   (loop for item across list
      for n from 1
	do (funcall outputter item n))))

(defun emit-value (name value)
  (when value
    (format *out-stream* "~A~C~A~C" name (code-char 0) value (code-char 0))))

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
	       (better-capitalize (aget :the-class cl))
	       (aget :level cl))))

(defun abbreviate-spell-info (string)
  (setf string (cl-ppcre:regex-replace " per " string "/"))
  (setf string (cl-ppcre:regex-replace "Casting Level" string "CL"))
  (setf string (cl-ppcre:regex-replace "[pP]enetrating" string "pnt."))
  (setf string (cl-ppcre:regex-replace "dismissible" string "D"))
  (setf string (cl-ppcre:regex-replace "enduring" string "E"))
  (setf string (cl-ppcre:regex-replace "[hH]ours?" string "hr."))
  (setf string (cl-ppcre:regex-replace "[mM]inutes?" string "min."))
  (setf string (cl-ppcre:regex-replace "[cC]oncentration" string "con."))
  (setf string (cl-ppcre:regex-replace "[rR]ound" string "rnd."))
  (setf string (cl-ppcre:regex-replace "sphere" string "sph."))
  (setf string (cl-ppcre:regex-replace "negates" string "neg."))
  (setf string (cl-ppcre:regex-replace "partial" string "part"))
  (setf string (cl-ppcre:regex-replace "repeatable" string "rep."))
  (setf string (cl-ppcre:regex-replace "terminal" string "term."))
  (setf string (cl-ppcre:regex-replace "Reflex" string "Ref"))
  (setf string (cl-ppcre:regex-replace "Fortitude" string "Fort"))
  (setf string (cl-ppcre:regex-replace "disbelief" string "disb"))
  (setf string (cl-ppcre:regex-replace "damage" string "dmg."))
  (setf string (cl-ppcre:regex-replace " ft." string "'"))
  )

(defun generate-form-file (character *out-stream*)
  (let ((*character* character))
    (emit-value "HeroFirstClassLevel"
		(fmt-class (elt (aget :classes character) 0)))
    (emit-item "HeroCharacterName" :character-name)
    (emit-item "HeroPlayerName" :player-name)
    (emit-item "HeroGender" :gender)
    (emit-value "HeroSpeciesTalent"
		(format nil "~A~@[/~A~]"
			(aget :species character)
			(nestring (aget :talent character))))
    (emit-value "HeroSecondClassLevel" (fmt-class (second (coerce (aget :classes character) 'list))))
    (emit-item "HeroCurrentXP" :xp)
    (emit-item "HeroHair" :hair)
    (emit-item "HeroSpeciality" :specialty)
    (emit-value "HeroThirdClassLevel" (fmt-class
				       (third (coerce (aget :classes character) 'list))))
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
	       (concatenate 'vector
			    (when (not (emptyp (aget :alignment character)))
			      (list #?"Alignment: $((aget :alignment character))"))
			    (mapcar* (lambda (x) #?"Language: ${x}")
				     (aget :languages character))
			    (mapcar* (lambda (x) #?"Study: ${X}")
				     (aget :studies character))))
    (emit-list "Subplots~d"
	       (concatenate 'vector
			    (aget :incomplete-subplots character)
			    (aget :completed-subplots character)))
    (emit-list "SubplotsCheck~d"
	       (concatenate 'vector
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
    (emit-value "DefenseArmMod"
		(-(calculate-field :defense-armor-mod character)))
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
       when (position key (aget :proficiency-list character))
       do (emit-value (format nil "Prof~a" pdf) "Yes")
       when (position key (aget :forte-list character))
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
		(format  nil "~@[~A ~]~A~@[ ~A~]"
			 (aget :armor-craftsmanship character)
			 (char
			  (better-capitalize (aget :armor-type character))
			  0)
			 (aget :armor-name character)))
    ;;TODO Arms (see Weapon1Type)
    (emit-item "ArmorDR" :armor-dr)
    (emit-item "ArmorDR" :armor-dr)
    (emit-item "ArmorDP" :armor-dp)
    (emit-item "ArmorACP" :armor-acp)
    (emit-item "ArmorSpd" :armor-speed)
    (emit-item "ArmorWgt" :armor-weight)
    (emit-item "ArmorDis" :armor-disguise)
    (emit-value "ArmorUp"
		(format nil "~{~a~^, ~}"
			(coerce (aget :armor-customizations character) 'list)))
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
	(loop
	   with nc-name = (array*)
	   with nc-notes = (array*)
	   with c-name = (array*)
	   with c-notes = (array*)
	   with s-name = (array*)
	   with s-notes = (array*)
	   for item in
	     (concatenate 'list
			  (aget :feat-list character)
			  (aget :ability-list character)
			  (get-trick-abilities character))
	   for desc = 
	     (cond
	       ((aget :notes item)
		(aget :notes item))
	       ((stringp (aget :from item))
		(aget :from item))
	       (t (fmt-class (aget :from item))))
	   when (eql (aget :list-as item) :non-combat)
	   do (vector-push-extend (aget :name item) nc-name)
	     (vector-push-extend desc nc-notes)
	   when (eql (aget :list-as item) :combat)
	   do (vector-push-extend (aget :name item) c-name)
	     (vector-push-extend desc c-notes)
	   when (eql (aget :list-as item) :spellcasting)
	   do (vector-push-extend (aget :name item) s-name)
	     (vector-push-extend desc s-notes)
	   finally (return (values nc-name nc-notes
				   c-name c-notes
				   s-name s-notes)))
      (emit-list "NonCombatAbName~D" nc-name)
      (emit-list "NonCombatAbNotes~D" nc-notes)
      (emit-list "CombatAbName~D" c-name)
      (emit-list "CombatAbNotes~D" c-notes)
      (emit-list "SpellAbName~D" s-name)
      (emit-list "SpellAbNotes~D" s-notes))
    (let ((weapons (list
		    (aget :weapon-1 character)
		    (aget :weapon-2 character)
		    (aget :weapon-3 character)
		    (aget :weapon-4 character))))
      (emit-list "Weapon~DType"
		 (map 'vector (curry #'gethash :name) weapons))
      (emit-list "Weapon~DAtk"
		 (array*
		  (calculate-field :weapon-1-atk-bonus character)
		  (calculate-field :weapon-2-atk-bonus character)
		  (calculate-field :weapon-3-atk-bonus character)
		  (calculate-field :weapon-4-atk-bonus character)))
      (emit-list "Weapon~DDMG"
		 (array*
		  (format nil "~A~:[+~;~]~A"
			  (aget :dmg-die (elt weapons 0))
			  (<
			   (calculate-field :weapon-1-dmg-bonus character)
			   0)
			  (calculate-field :weapon-1-dmg-bonus character))
		  (format nil "~A~:[+~;~]~A"
			  (aget :dmg-die (elt weapons 1))
			  (<
			   (calculate-field :weapon-2-dmg-bonus character)
			   0)
			  (calculate-field :weapon-2-dmg-bonus character))
		  (format nil "~A~:[+~;~]~A"
			  (aget :dmg-die (elt weapons 2))
			  (<
			   (calculate-field :weapon-3-dmg-bonus character)
			   0)
			  (calculate-field :weapon-3-dmg-bonus character))
		  (format nil "~A~:[+~;~]~A"
			  (aget :dmg-die (elt weapons 3))
			  (<
			   (calculate-field :weapon-4-dmg-bonus character)
			   0)
			  (calculate-field :weapon-4-dmg-bonus character))))
      (emit-list "Weapon~DThreat"
		 (map 'vector (curry #'gethash :threat) weapons))
      (emit-list "Weapon~DSzHand"
		 (map 'vector
		      (lambda (x)
			(format nil "~A/~A"
				(aget :size x)
				(aget :hand x)))
		      weapons))
      (emit-list "Weapon~DWgt"
		 (mapcar* (curry #'gethash :weight) weapons))
      (emit-list "Weapon~DRng"
		 (mapcar* (curry #'gethash :rng) weapons))
      (emit-list "Weapon~DShots"
		 (mapcar* (curry #'gethash :shots) weapons))
      (emit-list "Weapon~DQualUp"
		 (mapcar* (curry #'gethash :qualities) weapons))
      )
    
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
    (emit-list "GearName~D" (map 'vector
				 (curry #'gethash :name)
				 (aget :gear character)))
    (emit-list "GearEffects~D" (map 'vector (curry #'gethash :effect)
				    (aget :gear character)))
    (emit-list
     (lambda (item n)
       (emit-value #?"GearSzHand${n}" #?"\u$((aget :size item))/$((aget :hand item))"))
     (aget :gear character))
    (emit-list "GearWgt~D" (mapcar* (curry #'gethash :weight)
				    (aget :gear character)))
    (emit-item "MountName" :mount-name)
    (emit-item "MountSz" :mount-size)
    (emit-item "MountFoot" :mount-footprint)
    (emit-item "MountReach" :mount-reach)
    (emit-item "MountSpd" :mount-speed)
    (emit-item "MountTrav" :mount-travel)
    (emit-item "MountAttr" :mount-attributes)
    (emit-item "MountInit" :mount-init)
    (emit-item "MountAtk" :mount-atk)
    (emit-item "MountDef" :mount-def)
    (emit-item "MountRes" :mount-res)
    (emit-item "MountHealth" :mount-health)
    (emit-item "MountComp" :mount-comp)
    (emit-item "MountSkills" :mount-skills)
    (emit-item "MountQual" :mount-qualities)
    (emit-item "MountAttacks" :mount-attacks)
    (emit-item "VehicleName" :vehicle-name)
    (emit-item "VehicleSpd" :vehicle-speed)
    (emit-item "VehicleTravel" :vehicle-Travel)
    (emit-value "VehicleSzDef" (format nil "~A/~A"
				       (aget :vehicle-size character)
				       (aget :vehicle-def character)))
    (emit-value "VehicleOccLoad" (format nil "~A/~A"
					 (aget :vehicle-occupancy character)
					 (aget :vehicle-load character)))
    
    (emit-item "VehicleConst" :vehicle-const)
    (emit-item "VehicleQual" :vehicle-qualities)
    (emit-list "Contact~DName"
	       (mapcar* (curry #'gethash :name)
			(aget :contacts character)))
    (emit-list "Contact~DTrust"
	       (mapcar* (curry #'gethash :trust)
			(aget :contacts character)))
    (emit-list "Contact~DSzReach"
	       (mapcar* 
		(lambda (x)
		  (format nil "~A/~A" (aget :size x) (aget :reach x)))
		(aget :contacts character)))
    (emit-list "Contact~DSpd"
	       (mapcar* (curry #'gethash :speed)
			(aget :contacts character)))
    (emit-list "Contact~DAttr"
	       (mapcar* (curry #'gethash :attributes)
			(aget :contacts character)))
    (emit-list "Contact~DRepCost"
	       (mapcar* (curry #'gethash :rep-cost)
			(aget :contacts character)))
    (emit-list "Contact~DInit"
	       (mapcar* (curry #'gethash :init)
			(aget :contacts character)))
    (emit-list "Contact~DAtk"
	       (mapcar* (curry #'gethash :atk)
			(aget :contacts character)))
    (emit-list "Contact~DDef"
	       (mapcar* (curry #'gethash :Def)
			(aget :contacts character)))
    (emit-list "Contact~DRes"
	       (mapcar* (curry #'gethash :Res)
			(aget :contacts character)))
    (emit-list "Contact~DHealth"
	       (mapcar* (curry #'gethash :Health)
			(aget :contacts character)))
    (emit-list "Contact~DComp"
	       (mapcar* (curry #'gethash :comp)
			(aget :contacts character)))
    (emit-list "Contact~DSkills"
	       (mapcar* (curry #'gethash :skills)
			(aget :contacts character)))
    (emit-list "Contact~DQual"
	       (mapcar* (curry #'gethash :qualities)
			(aget :contacts character)))
    (emit-list "Contact~DAttacks"
	       (mapcar* (curry #'gethash :attacks)
			(aget :contacts character)))
    (emit-list "Contact~DGear"
	       (mapcar* (curry #'gethash :gear)
			(aget :contacts character)))
    (emit-list "Holding~DName"
	       (mapcar* (curry #'gethash :name)
			(aget :holdings character)))
    (emit-list "Holding~DScale"
	       (mapcar* (curry #'gethash :scale)
			(aget :holdings character)))
    (emit-list "Holding~DGuests"
	       (mapcar* (curry #'gethash :guests)
			(aget :holdings character)))
    (emit-list "Holding~DMax"
	       (mapcar* (curry #'gethash :max-guests)
			(aget :holdings character)))
    (emit-list "Holding~DUpgrades1"
	       (mapcar* (curry #'gethash :upgrades)
			(aget :holdings character)))
    (emit-list "Holding~DRepCost"
	       (mapcar* (curry #'gethash :Rep-Cost)
			(aget :holdings character)))
    (emit-list "MagicItemName~D"
	       (mapcar* (curry #'gethash :name)
			(aget :magic-items character)))
    (emit-list "MagicItemLvl~D"
	       (mapcar* (curry #'gethash :level)
			(aget :magic-items character)))
    (emit-list "MagicItemEssences~D"
	       (mapcar* (curry #'gethash :essences)
			(aget :magic-items character)))
    (emit-list "MagicItemCharms~D"
	       (mapcar* (curry #'gethash :charms)
			(aget :magic-items character)))
    (emit-list "MagicItemRepCost~D"
	       (mapcar* (curry #'gethash :rep-cost)
			(aget :magic-items character)))
    (emit-item "SpellPoints" :spell-points)
    (emit-item "CastingLevel" :casting-level)
    (emit-field "CastingBonusTotal" :spellcasting-total)
    (emit-field "CastingMiscMod" :spellcasting-misc-mod)
    (emit-field "CastingIntMod" :int-mod)
    (emit-item "CastingRanks" ::spellcasting-ranks)
    (emit-field "SpellsKnownTotal" :spells-known)
    (emit-field "SpellsKnownWisScore" :real-wis)
    (emit-field "SpellsKnownMiscMod" :spells-known-misc-mod)
    (emit-item "SpellsKnownRanks" :spellcasting-ranks)
    (emit-field "SaveDCTotal" :save-dc)
    (emit-field "SaveDCChaMod" :cha-mod)
    (emit-item "SaveDCFeats" :spellcasting-feats)
    (emit-list "SpellListNameSchool~D"
	       (mapcar*
		(lambda (x)
		  (format nil "~A/~A" (aget :name x) (aget :discipline x)))
		(aget :spells character)))
    (emit-list "SpellListLvl~D"
	       (mapcar* (curry #'gethash :level) (aget :spells character)))
    (emit-list "SpellListCasting~D"
	       (mapcar* (curry #'gethash :casting-time) (aget :spells character)))
    (emit-list "SpellListDist~D"
	       (mapcar* (curry #'gethash :distance) (aget :spells character)))
    (emit-list "SpellListArea~D"
	       (mapcar*
		(compose #'abbreviate-spell-info (curry #'gethash :area))
		(aget :spells character)))
    (emit-list "SpellListDur~D"
	       (mapcar*
		(compose #'abbreviate-spell-info (curry #'gethash :duration))
		(aget :spells character)))
    (emit-list "SpellListSav~D"
	       (mapcar*
		(compose #'abbreviate-spell-info (curry #'gethash :saving-throw))
		(aget :spells character)))
    (emit-list "SpellListPrep~D"
	       (mapcar*
		(compose #'abbreviate-spell-info (curry #'gethash :preparation-cost))
		(aget :spells character)))
    (emit-list "SpellListEffect~D"
	       (mapcar*
		(compose #'abbreviate-spell-info (curry #'gethash :effect))
		(aget :spells character)))))
  

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

(defun generate-bio-notes (character)
  (tt:with-style (:font "times-bold")
    (tt:paragraph () "Biography"))
    (loop for p in (split-sequence #\Newline (aget :biography character))
	 do (tt:paragraph ()
	   (tt:put-string p)))
  (tt:with-style (:font "times-bold")
    (tt:paragraph () "Notes"))
    (loop for p in (split-sequence #\Newline (aget :notes character))
	 do (tt:paragraph ()
	   (tt:put-string p))))

(defun generate-spell-table (character)
  (tt:table (:col-widths '(90 400) :splittable-p t)
    (tt:header-row ()
      (tt:cell ()
	(tt:paragraph () "Spell Name"))
      (tt:cell ()
	(tt:paragraph () "Description"))
      (loop for item across (aget :spells character)
	 do
	   (tt:row ()
	     (tt:cell ()
	       (tt:paragraph ()
		 (tt:put-string (fix-unicode (aget :name item)))))
	     (tt:cell ()
	       (tt:paragraph ()
		 (tt:with-style (:font "times-bold")
		   (tt:put-string "Level:"))
		 (tt:put-string (fix-unicode
				 (format nil "~D" (aget :level item)))))
	       (tt:paragraph ()
		 (tt:with-style (:font "times-bold")
		   (tt:put-string "Discipline:"))
		 (tt:put-string (fix-unicode (aget :discipline item))))
	       (tt:paragraph ()
		 (tt:with-style (:font "times-bold")
		   (tt:put-string "Distance:"))
		 (tt:put-string (fix-unicode (aget :distance item))))
	       (tt:paragraph ()
		 (tt:with-style (:font "times-bold")
		   (tt:put-string "Casting Time:"))
		 (tt:put-string (fix-unicode (aget :casting-time item))))
	       (when (not (emptyp (aget :area item)))
		 (tt:paragraph ()
		   (tt:with-style (:font "times-bold")
		     (tt:put-string "Area:"))
		   (tt:put-string (fix-unicode (aget :area item)))))
	       (tt:paragraph ()
		 (tt:with-style (:font "times-bold")
		   (tt:put-string "Duration:"))
		 (tt:put-string (fix-unicode (aget :duration item))))
	       (when (not (emptyp (aget :saving-throw item)))
		 (tt:paragraph ()
		   (tt:with-style (:font "times-bold")
		     (tt:put-string "Saving Throw:"))
		   (tt:put-string (fix-unicode (aget :saving-throw item)))))
	       (when (not (emptyp (aget :preparation-cost item)))
		 (tt:paragraph ()
		   (tt:with-style (:font "times-bold")
		     (tt:put-string "Preparation Cost:"))
		   (tt:put-string (fix-unicode (aget :preparation-cost item)))))
	       (tt:paragraph ()
		 (tt:with-style (:font "times-bold")
		   (tt:put-string "Effect:"))
		 (tt:put-string (fix-unicode (aget :effect item))))
	       ))))))
		 
	   
(defun generate-ability-table (character)
  (tt:with-style (:font "times-bold")
    (tt:paragraph () "Abilities & Feats"))
  (tt:table (:col-widths '(90 400) :splittable-p t)
    (tt:header-row ()
      (tt:cell ()
	(tt:paragraph () "Feature Name"))
      (tt:cell ()
	(tt:paragraph () "Description")))
    (loop for item across (aget :feat-list character)
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
					     parameter))))
	   (tt:cell ()
	     (loop for paragraph in
		  (split-sequence #\Newline
				  (fix-unicode (third (gethash (to-keyword name) +feat-hash+))))
		when (and parameter
			  (search
			   (cl-ppcre:regex-replace " ?\\([^)]*\\)"
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
  #-(or)(loop for item across (aget :ability-list character)
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
    (generate-form-file character s)
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
	       (generate-bio-notes character)
	       (generate-ability-table character)
	       (when (aget :spells character)
		 (generate-spell-table character)))
	     :margins '(36 36 36 36)
	     :break :after))
	  (pdf:write-document table-stream)))
      :close-stream
      (uiop:with-temporary-file (:stream s :pathname filled-pathname)
	(declare (ignore s))
	(handler-bind
	    ((uiop:subprocess-error
	      (lambda (condition)
		(declare (ignorable condition))
		(uiop:run-program `("cp" ,(namestring path) "/tmp/error.tmp")))))
	  (uiop:run-program `(,+formfill-path+ ,*path-to-charsheet* ,(namestring path) ,(namestring filled-pathname))
			    :input "")
	  (uiop:with-temporary-file (:stream s :pathname final-pathname)
	    (declare (ignore s))
	    :close-stream
	    (uiop:run-program `(,+unite-path+ ,(namestring filled-pathname)
					      ,(namestring table-pathname)
					      ,(namestring final-pathname))
			      :input "")
	    (with-open-file (in final-pathname :external-format :iso-8859-1)
	      (with-output-to-string (out)
		(uiop:slurp-input-stream out in)))))))))



(cl-interpol:disable-interpol-syntax)
