;;;; cl-fccs.asd

(asdf:defsystem #:cl-fccs
  :description "Character Sheet manager for FC"
  :author "Jason Miller <aidenn0@geocities.com>"
  :license "MIT/X11"
  :depends-on (;; #:clack-v1-compat
               #:clack
               #:lack-middleware-static
               #:quri
               #:parenscriptx
	       #:parse-number
               #:alexandria
               #:log4cl
	       #:cl-redis
	       #:cl-interpol
	       #:cl-typesetting
	       #:cl-store
	       #:optima.ppcre
	       #:crypto-shortcuts
	       #:dissect
	       #:cl-json
	       #:lambda-fiddle
               #:cl-who)
  :serial t
  :components ((:file "package")
	       (:module "data"
		:serial t
		:components ((:file "data")
			     (:file "data2")))
	       (:module "src"
		:serial t
		:components
		((:file "route")
		 (:file "ps-compat")
		 (:file "util")
		 (:file "view-macros")
		 (:file "store")
		 (:file "schema")
		 (:file "users")
		 (:file "pdfout")
		 (:file "session")
		 (:file "r20out")
		 (:static-file "view.lisp")
		 (:file "model" :depends-on ("view.lisp"))
		 (:file "completions")
		 (:file "admin")
		 (:file "clackup")
		 (:file "genparenscript")))))

