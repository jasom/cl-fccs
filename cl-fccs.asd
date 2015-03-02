;;;; cl-fccs.asd

(asdf:defsystem #:cl-fccs
  :description "Describe cl-fccs here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:clack
	       #:clack-middleware-auth-basic
               #:parenscriptx
               #:alexandria
	       #:cl-redis
	       #:cl-store
	       #:optima.ppcre
	       #:dissect
	       #:cl-json
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
		 (:file "closure-server")
		 (:file "clackup")
		 (:file "store")
		 (:file "model")))))

