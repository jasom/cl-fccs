;;;; cl-fccs.asd

(asdf:defsystem #:cl-fccs
  :description "Describe cl-fccs here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:clack
	       #:clack-middleware-auth-basic
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
		 (:file "view-macros")
		 ;(:file "closure-server")
		 (:file "store")
		 (:file "pdfout")
		 (:file "clackup")
		 (:file "model")))))

