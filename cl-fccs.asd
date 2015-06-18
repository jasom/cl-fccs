;;;; cl-fccs.asd

(asdf:defsystem #:cl-fccs
  :description "Character Sheet manager for FC"
  :author "Jason Miller <aidenn0@geocities.com>"
  :license "MIT/X11"
  :depends-on (#:clack
	       #:clack-middleware-auth-basic
	       #:clack-handler-mongrel2
               #:parenscriptx
	       #:parse-number
               #:alexandria
               #:log4cl
	       #:cl-redis
	       #:cl-interpol
	       #:cl-typesetting
	       #:cl-store
	       #:lzstring
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
		((:file "ps-compat")
		 (:file "util")
		 (:file "view-macros")
		 ;(:file "closure-server")
		 (:file "store")
		 (:file "pdfout")
		 (:file "clackup")
		 (:file "users")
		 (:static-file "view.lisp")
		 (:file "model" :depends-on ("view.lisp"))))))

