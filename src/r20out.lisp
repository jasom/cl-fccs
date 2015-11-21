(defpackage :cl-fccs-r20
  (:use :cl :alexandria)
  (:import-from :cl-fccs
		:calculate-field
		:aget
		:loopable
		:better-capitalize
		:key-fmt
		:+proficiencies+
		))
(in-package :cl-fccs-r20)

(cl-interpol:enable-interpol-syntax)

(defvar *character*)
(defvar *out-stream*)

(defun emit-field (name fname)
  (emit-value name (calculate-field fname *character*)))

(defun nestring (string)
  (unless (emptyp string)
    string))

(defun simple-emit-item (fname)
  (emit-item (string-downcase (substitute #\_ #\- (symbol-name fname))) fname))

(defun simple-emit-field (fname)
  (emit-field (string-downcase (substitute #\_ #\- (symbol-name fname))) fname))

(defun emit-item (name fname)
  (let ((val
	 (loop
	    with item
	    for field in (ensure-list fname)
	    for obj = *character* then item
	    do (setf item  (aget field obj))
	    finally (return item))))
    (emit-value name val)))

(defun emit-list (outputter list &optional (index 0))
  (if
   (stringp outputter)
   (emit-list (lambda (item n)
		(emit-value (format nil outputter n) item))
	      list
	      index)
   (loop for item in list
      for n from index 
	when item
	do (funcall outputter item n))))

(defun emit-repeating (prefix-name field-name list)
  (when list
    (emit-value field-name (car list))
    (emit-list #?"${prefix-name}_~d_${field-name}" (cdr list))))

(defun at-substitute (value)
  (if (stringp value)
      (cl-ppcre:regex-replace-all "@" value "----at----")
      value))

(defun emit-value (name value)
  (setf (gethash name *out-stream*)
	(at-substitute value)))

(defun emit-skills-fields (skill abbrev
			   &aux (skill (string-upcase (string skill))))
  (when
      (aget (key-fmt :~A-origin skill) *character*)
    (emit-value (format nil "~a_origin" abbrev) (if (equal skill "ACROBATICS") "on" 0)))
  (emit-item (format nil "~a_ranks" abbrev) (key-fmt :~A-ranks skill))
  (emit-field (format nil "~a_misc_mod" abbrev) (key-fmt :~a-misc-bonus skill))
  (emit-item (format nil "~a_threat_mod" abbrev) (key-fmt :~a-threat skill)))

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

(defun genlist (character)
  (let ((*out-stream* (make-hash-table :test #'equal))
	(*character* character))
    (emit-value "first_class" (better-capitalize
			       (aget :the-class (first (aget :classes character)))))
    (emit-value "first_class_level"
		(aget :level (first (aget :classes character))))
    ;;(emit-item "HeroCharacterName" :character-name)
    (simple-emit-item :player-name)
    (simple-emit-item :gender)
    (emit-value "species_talent"
		(format nil "~A~@[/~A~]"
			(aget :species character)
			(nestring (aget :talent character))))
    (emit-list "repeating_multiclass_~D_name"
	       (loop for item in (cdr (aget :classes character))
		  collect (aget :the-class item)))
    (emit-list "repeating_multiclass_~D_level"
	       (loop for item in (cdr (aget :classes character))
		  collect (aget :level item)))
    (emit-item "current_xp" :xp)
    (simple-emit-item :hair)
    (simple-emit-item :specialty)
    (emit-field "next_level" :next-xp)
    (simple-emit-item :weight)
    (simple-emit-item :eyes)
    (simple-emit-item :age)
    (simple-emit-item :height)
    (emit-field "die_type" :action-dice-type)
    (emit-field "starting_dice" :starting-action-dice)
    (emit-field "str_score" :real-str)
    (emit-field "dex_score" :real-dex)
    (emit-field "con_score" :real-con)
    (emit-field "int_score" :real-int)
    (emit-field "wis_score" :real-wis)
    (emit-field "cha_score" :real-cha)
    (loop for skill in
	 '("acrobatics"
	   "athletics"
	   "blend"
	   "bluff"
	   "crafting"
	   "disguise"
	   "haggle"
	   "impress"
	   "intimidate"
	   "investigate"
	   "medicine"
	   "notice"
	   "prestidigitation"
	   "resolve"
	   "ride"
	   "search"
	   "sense_motive"
	   "sneak"
	   "survival"
	   "tactics")
       do (emit-skills-fields skill skill))
    (emit-value "max_skill_ranks"
		(elt (elt fccg::+career-levels+
			  (calculate-field :career-level character)) 3))
    (emit-list "repeating_focuses_~d_focuses_skill"
	       (nconc (loop for item in (aget :crafting-foci character)
			 collect "Crafting")
		      (loop for item in (aget :ride-foci character)
			   collect "Ride")))
    (emit-list "repeating_focuses_~d_focuses_focus"
	       (nconc (loop for item in (aget :crafting-foci character)
			 collect item)
		      (loop for item in (aget :ride-foci character)
			 collect item)))
    (simple-emit-field :total-studies)
    (simple-emit-item :alignment)
    #+(or)
    (emit-list "repeating_interests_~D_interest"
	       (append
		(when (not (emptyp (aget :alignment character)))
		  (list #?"Alignment: $((aget :alignment character))"))
		(mapcar (lambda (x) #?"Language: ${x}")
			(aget :languages character))
		(mapcar (lambda (x) #?"Study: ${X}")
			(aget :studies character))))
    #+#:not-in-roll20
    (emit-list "Subplots~d"
	       (append
		(aget :incomplete-subplots character)
		(aget :completed-subplots character)))
    #+#:not-in-roll20
    (emit-list "SubplotsCheck~d"
	       (append
		(make-list (length (aget :incomplete-subplots character))
			   :initial-element nil)
		(make-list (length (aget :completed-subplots character))
			   :initial-element "Yes")))
    (emit-item "coin" :coin-in-hand)
    (simple-emit-item :stake)
    (emit-field "total_lifestyle" :lifestyle)
    (simple-emit-item :panache)
    (simple-emit-field :appearance-bonus)
    (simple-emit-field :income)
    (simple-emit-item :prudence)
    (emit-item "money_save_earn" :money-save/earned) 
    (emit-field "defense_misc" :defense-misc-mod) 
    (emit-field "defense_gear" :defense-armor-mod)
    (emit-field "defense_class" :base-defense)
    (simple-emit-field :vitality)
    (simple-emit-field :wounds)
    #+#:not-in-roll20
    (emit-field "SizeReach" :reach)
    #+#:not-in-roll20
    (emit-field "SizeFootprint" :footprint)
    (emit-value "sz_sel"
		(getf
		 '(:tiny -2
		   :small -1
		   :medium 0
		   :large 1
		   :huge 2)
		 (calculate-field :size character)))
    (emit-field "groundspeed" :ground-speed)
    (loop for pdf in '("unarmed" "blunt" "edged" "hurled" "bows" "blackpowder" "siegeweapons")
       for key in +proficiencies+
       do (emit-value (format nil "~a_proficiency" pdf)
		      (cond 
			((member key (aget :forte-list character)) 1)
			((member key (aget :proficiency-list character)) 0)
			(t -4))))
    (emit-field "initiative_misc" :init-misc-mod)
    (emit-field "initiative_class" :base-init)
    (emit-field "base_attack_bonus" :bab)
    (emit-field "unarmed_misc" :unarmed-misc-mod)
    (emit-field "melee_misc" :melee-misc-mod)
    (emit-field "ranged_misc" :ranged-misc-mod)
    (emit-field "fortitude_save" :fortitude-base)
    (emit-field "fortitude_misc" :fortitude-misc-mod)
    (emit-field "reflex_save" :reflex-base)
    (emit-field "reflex_misc" :reflex-misc-mod)
    (emit-field "will_save" :will-base)
    (emit-field "will_misc" :will-misc-mod)
    (emit-value "armor_type"
		(format  nil "~@[~A ~]~A~@[ ~A~]"
			 (aget :armor-craftsmanship character)
			 (char
			  (better-capitalize (aget :armor-type character))
			  0)
			 (aget :armor-name character)))
    (simple-emit-item :armor-dr)
    (simple-emit-item :armor-dr)
    (simple-emit-item :armor-dp)
    (simple-emit-item :armor-acp)
    (simple-emit-item :armor-speed)
    (simple-emit-item :armor-weight)
    (simple-emit-item :armor-disguise)
    #+#:bug-in-roll20-sheet
    (emit-value "ArmorUp"
		(format nil "~{~a~^, ~}"
			(aget :armor-customizations character)))


    (multiple-value-bind
	  (nc-name nc-notes c-name c-notes s-name s-notes)
	(loop for item in
	     (append (aget :feat-list character)
		     (aget :ability-list character)
		     (cl-fccs::get-trick-abilities character))
	   for desc = 
	     (cond
	       ((aget :notes item)
		(aget :notes item))
	       ((stringp (aget :from item))
		(aget :from item))
	       (t (cl-fccs::fmt-class (aget :from item))))
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
      (setf c-name (nconc c-name s-name))
      (setf c-notes (nconc c-notes s-notes))
      (emit-value "nc_ability_name" (car nc-name))
      (emit-list "repeating_noncombat_~d_nc_ability_name" (rest nc-name))
      (emit-value "nc_ability_desc" (car nc-notes))
      (emit-list "repeating_noncombat_~d_nc_ability_desc" (cdr nc-notes))
      (emit-value "com_ability_name" (car c-name))
      (emit-list "repeating_combat_~D_com_ability_name" (cdr c-name))
      (emit-value "com_ability_desc" (car c-notes))
      (emit-list "repeating_combat_~d_com_ability_desc" (cdr c-notes)))

    (let ((weapons (list
		    (aget :weapon-1 character)
		    (aget :weapon-2 character)
		    (aget :weapon-3 character)
		    (aget :weapon-4 character))))
      (emit-list "weapon_name_~d"
		 (mapcar (curry #'gethash :name) weapons)
		 1)
      (emit-list "weapon_attack_misc_~D"
		 (loop for i from 1 to 4
		    collect (loop for item in (loopable (aget (key-fmt :weapon~d-atk-bonus i) (aget :fudges character)))
			       sum (aget :value item)))
		 1)
      (emit-list "weapon_damage_~d"
		 (list
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
			  (calculate-field :weapon-4-dmg-bonus character)))
		 1)
      (emit-list "weapon_threat_~d"
		 (mapcar (curry #'gethash :threat) weapons)
		 1)
      (emit-list "weapon_size_~d"
		 (loop for weapon in weapons
		    collect (case (make-keyword (aget :size weapon))
				(:n -4)
				(:f -2)
				(:d 0)
				(:t 2)
				(:s 4)
				(:m 6)
				(:l 8)
				(:h 10)
				(:g 12)))
		 1)
      (emit-list "weapon_hand_~d"
		 (loop for weapon in weapons
		    collect (aget :hand weapon))
		 1)
      #+#:not-in-roll20
      (emit-list "Weapon~DWgt"
		 (mapcar (curry #'gethash :weight) weapons))
      #+#:not-in-roll20
      (emit-list "Weapon~DRng"
		 (mapcar (curry #'gethash :rng) weapons))
      #+#:not-in-roll20
      (emit-list "Weapon~DShots"
		 (mapcar (curry #'gethash :shots) weapons))
      (emit-list "weapon_proficiency_bonus_~D"
		 (loop for weapon in weapons
		    ;;do (format t "~s~%" (string-downcase
					 ;;(substitute #\_ #\- (symbol-name (aget :type weapon)))))
		    collect 
		      (when (not (eql :|| (aget :type weapon)))
			  (format nil "@{~a_proficiency}"
				  (string-downcase
				   (substitute #\_ #\- (symbol-name (aget :type weapon)))))))
		 1)
      (emit-list "weapon_qualities_~d"
		 (mapcar (curry #'gethash :qualities) weapons)
		 1))

    
    (simple-emit-field :legend)
    (simple-emit-item :reputation)
    #+#:bug-in-roll20-sheet
    (emit-item "RepRenHeroRen" :heroic-renown)
    #+#:bug-in-roll20-sheet
    (emit-item "RepRenMilRen" :military-renown)
    #+#:bug-in-roll20-sheet
    (emit-item "RepRenNobleRen" :noble-renown)

    (emit-repeating "repeating_gear" "gear_name"
		    (mapcar (curry #'gethash :name)
			    (aget :gear character)))
    (emit-repeating "repeating_gear" "gear_effect"
		    (mapcar (curry #'gethash :effect)
			    (aget :gear character)))
    (emit-repeating "repeating_gear" "gear_size"
		    (loop for item in (aget :gear character)
		       collect #?"\u$((aget :size item))/$((aget :hand item))"))
    (emit-repeating "repeating_gear" "gear_weight"
		    (mapcar (curry #'gethash :weight)
			    (aget :gear character)))

    #+#:not-in-roll-20
    (emit-item "MountName" :mount-name)
    #+#:not-in-roll-20
    (emit-item "MountSz" :mount-size)
    #+#:not-in-roll-20
    (emit-item "MountFoot" :mount-footprint)
    #+#:not-in-roll-20
    (emit-item "MountReach" :mount-reach)
    #+#:not-in-roll-20
    (emit-item "MountSpd" :mount-speed)
    #+#:not-in-roll-20
    (emit-item "MountTrav" :mount-travel)
    #+#:not-in-roll-20
    (emit-item "MountAttr" :mount-attributes)
    #+#:not-in-roll-20
    (emit-item "MountInit" :mount-init)
    #+#:not-in-roll-20
    (emit-item "MountAtk" :mount-atk)
    #+#:not-in-roll-20
    (emit-item "MountDef" :mount-def)
    #+#:not-in-roll-20
    (emit-item "MountRes" :mount-res)
    #+#:not-in-roll-20
    (emit-item "MountHealth" :mount-health)
    #+#:not-in-roll-20
    (emit-item "MountComp" :mount-comp)
    #+#:not-in-roll-20
    (emit-item "MountSkills" :mount-skills)
    #+#:not-in-roll-20
    (emit-item "MountQual" :mount-qualities)
    #+#:not-in-roll-20
    (emit-item "MountAttacks" :mount-attacks)
    #+#:not-in-roll-20
    (emit-item "VehicleName" :vehicle-name)
    #+#:not-in-roll-20
    (emit-item "VehicleSpd" :vehicle-speed)
    #+#:not-in-roll-20
    (emit-item "VehicleTravel" :vehicle-Travel)
    #+#:not-in-roll-20
    (emit-value "VehicleSzDef" (format nil "~A/~A"
    #+#:not-in-roll-20
    (aget :vehicle-size character)
    #+#:not-in-roll-20
    (aget :vehicle-def character)))
    #+#:not-in-roll-20
    (emit-value "VehicleOccLoad" (format nil "~A/~A"
    #+#:not-in-roll-20
    (aget :vehicle-occupancy character)
    #+#:not-in-roll-20
    (aget :vehicle-load character)))
    #+#:not-in-roll-20
    (emit-item "VehicleConst" :vehicle-const)
    #+#:not-in-roll-20
    (emit-item "VehicleQual" :vehicle-qualities)
    #+#:not-in-roll-20
    (emit-list "Contact~DName"
	       (mapcar (curry #'gethash :name)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DTrust"
	       (mapcar (curry #'gethash :trust)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DSzReach"
	       (mapcar 
		(lambda (x)
		  (format nil "~A/~A" (aget :size x) (aget :reach x)))
		(aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DSpd"
	       (mapcar (curry #'gethash :speed)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DAttr"
	       (mapcar (curry #'gethash :attributes)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DRepCost"
	       (mapcar (curry #'gethash :rep-cost)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DInit"
	       (mapcar (curry #'gethash :init)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DAtk"
	       (mapcar (curry #'gethash :atk)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DDef"
	       (mapcar (curry #'gethash :Def)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DRes"
	       (mapcar (curry #'gethash :Res)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DHealth"
	       (mapcar (curry #'gethash :Health)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DComp"
	       (mapcar (curry #'gethash :comp)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DSkills"
	       (mapcar (curry #'gethash :skills)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DQual"
	       (mapcar (curry #'gethash :qualities)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DAttacks"
	       (mapcar (curry #'gethash :attacks)
		       (aget :contacts character)))
    #+#:not-in-roll-20
    (emit-list "Contact~DGear"
	       (mapcar (curry #'gethash :gear)
		       (aget :contacts character)))
    (emit-repeating "repeating_holding" "holding_name"
	       (mapcar (curry #'gethash :name)
		       (aget :holdings character)))
    (emit-repeating "repeating_holding" "holding_scale"
	       (mapcar (curry #'gethash :scale)
		       (aget :holdings character)))
    (emit-repeating "repeating_holding" "holding_max_guests"
		    (mapcar (curry #'gethash :max-guests)
			    (aget :holdings character)))
    (emit-repeating "repeating_holding" "holding_upgrades"
	       (mapcar (curry #'gethash :upgrades)
		       (aget :holdings character)))
    (emit-repeating "repeating_holding" "holding_cost"
	       (mapcar (curry #'gethash :Rep-Cost)
		       (aget :holdings character)))
    #+#:not-in-roll-20
    (emit-list "MagicItemName~D"
	       (mapcar (curry #'gethash :name)
		       (aget :magic-items character)))
    #+#:not-in-roll-20
    (emit-list "MagicItemLvl~D"
	       (mapcar (curry #'gethash :level)
		       (aget :magic-items character)))
    #+#:not-in-roll-20
    (emit-list "MagicItemEssences~D"
	       (mapcar (curry #'gethash :essences)
		       (aget :magic-items character)))
    #+#:not-in-roll-20
    (emit-list "MagicItemCharms~D"
	       (mapcar (curry #'gethash :charms)
		       (aget :magic-items character)))
    #+#:not-in-roll-20
    (emit-list "MagicItemRepCost~D"
	       (mapcar (curry #'gethash :rep-cost)
		       (aget :magic-items character)))
    (emit-item "spellpoints" :spell-points)
    (emit-item "caster_level" :casting-level)
    (simple-emit-item :spellcasting-ranks)
    (simple-emit-field :spellcasting-misc-mod)
    (emit-field "spells_known" :spells-known)
    (emit-field "spellsave_dc" :save-dc)
    (emit-item "spellcasting_feats" :spellcasting-feats)

    (loop
       with all-spells = (loop for item in (aget :spells character)
			    with output = (make-hash-table)
			      do (push item (gethash (aget :level item) output))
			    finally (return output))
       for level from 0 to 9
       for spells = (gethash level all-spells)
       do
	 (emit-repeating #?"repeating_spellbooklevel${level}" "spellname"
			 (mapcar (curry #'gethash :name) spells))
	 (emit-repeating #?"repeating_spellbooklevel${level}" "spellschool"
			 (mapcar (curry #'gethash :discipline) spells))
	 (emit-repeating #?"repeating_spellbooklevel${level}" "spellcasttime"
			 (mapcar (curry #'gethash :casting-time) spells))
       ;;TODO make sure all the ranges work
	 (emit-repeating #?"repeating_spellbooklevel${level}" "spellrange"
			 (loop for item in spells
			    for distance = (aget :distance item)
			    collect
			      (cond
				((equalp distance "Personal or Touch") "P+T")
				(t distance))))
	 (emit-repeating #?"repeating_spellbooklevel${level}" "spelltarget"
			 (mapcar (curry #'gethash :area) spells))
	 (emit-repeating #?"repeating_spellbooklevel${level}" "spellduration"
			 (mapcar (curry #'gethash :duration) spells))
	 (emit-repeating #?"repeating_spellbooklevel${level}" "spellsave"
			 (mapcar (lambda (x)
				   (if (equal ""
					      (gethash :saving-throw x))
				       "(Save)"))
				 spells))
	 (emit-repeating #?"repeating_spellbooklevel${level}" "spellprepcost"
			 (mapcar (curry #'gethash :preparation-cost) spells))
	 (emit-repeating #?"repeating_spellbooklevel${level}" "spelldescription"
			 (mapcar (curry #'gethash :effect) spells)))
    *out-stream*))

#+(or)(defun generate-bio-notes (character)
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

#+(or)(defun generate-spell-table (character)
	(tt:table (:col-widths '(90 400) :splittable-p t)
	  (tt:header-row ()
	    (tt:cell ()
	      (tt:paragraph () "Spell Name"))
	    (tt:cell ()
	      (tt:paragraph () "Description"))
	    (loop for item in (aget :spells character)
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
		 
	   
#+(or)(defun generate-ability-table (character)
	(tt:with-style (:font "times-bold")
	  (tt:paragraph () "Abilities & Feats"))
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

(defun generate-json (character)
  (json:encode-json-alist-to-string
   `((:name ,@(aget :character-name character))
     (:abilities ,@(genlist character)))))

(cl-interpol:disable-interpol-syntax)
