(in-package :cl-fccs)

#-dev(defvar *prepend-path* "/fccs2/")
#+dev(defvar *prepend-path* "/fccs2dev/")
  
(defun strip-path-prefix (path)
  (if (starts-with-subseq *prepend-path* path)
      (subseq path (1- (length *prepend-path*)))
      (error "Path ~A not in application" path)))

(defun fixup-path (path)
    (format nil "~A~A"
	    *prepend-path*
	    (if (eql (char path 0) #\/)
		(subseq path 1)
		path)))

(defmacro render-page ((stream-name) &body body)
  `(cl-who:with-html-output-to-string (,stream-name nil :prologue "<!DOCTYPE html>")
    (:html
     (:head
      (:meta :charset "utf-8")
      ;(:script :src (fixup-path "/pub/build/react-with-addons.js"))
      ;(:script :src (fixup-path "/pub/build/lz-string.min.js"))
      (:link :rel "stylesheet" :href (fixup-path "/pub/all.css"))
      ;(:link :rel "stylesheet" :href (fixup-path "/pub/style/pure-min.css"))
      ;(:link :rel "stylesheet" :href (fixup-path "/pub/style/grid.css"))
      ;(:link :rel "stylesheet" :href (fixup-path "/pub/style/local.css"))
      (:meta :name "viewport" :content "width=device=width, intial-scale=1")
      (:body #+(or)(:script :src (fixup-path "/pub/build/psx.js"))
	     ;(:img :src (fixup-path "/pub/images/fc.png"))
	     (:script :src (fixup-path "/pub/all.js"))
	     (:div :hidden t
		    :id "csrf"
		    (esc (session-csrf-token session)))
	      (:div :id "react-content")
	     ,@body)))))

(defun login-page ()
  (cl-who:with-html-output-to-string  (s)
    (:html
     (:head
      (:meta :charset "utf-8"))
     (:body
      (:form
       :action "."
       :method :post
       (:p
	(:label :html-for "username" "Username")
	(:input :id "username"
		:name "username"))
       (:p
	(:label :html-for "password" "Password")
	(:input :id "password"
		:type "password"
		:name "password"))
       (:input
	:type :submit
	:value "Login"
	:class-name "pure-button"))))))

(defun parse-json-body (body)
  (let*
      ((obj-state)
       (json:*beginning-of-object-handler*
	(lambda ()
	  (push (make-hash-table) obj-state)))
       (json:*end-of-object-handler*
	(lambda ()
	  (pop obj-state)))
       (json:*object-key-handler*
	(lambda (key)
	  (push (to-keyword key) obj-state)))
       (json:*object-value-handler*
	(lambda (val)
	  (let ((key (pop obj-state)))
	    (setf (gethash key (car obj-state)) val)))))
    (json:decode-json-from-string (babel:octets-to-string body :encoding :utf-8))))

(defmacro with-error-logging ((error-vname &optional debug) error-result &body b)
  `(let ((stack-at-error nil))
     (handler-case
	 (handler-bind ((error
			 (lambda (err)
			   (declare (ignore err))
			   (setf stack-at-error (dissect:stack)))))
	   ,@b)
       (,(if debug nil 'error)
	   (,error-vname)
	 (log:error "Error processing request:~%~A~%~A" stack-at-error ,error-vname)
	 ,error-result))))

(defun encode-classish (obj stream)
  (let ((json:*lisp-identifier-name-to-json*
	 (lambda (id)
	   (string-downcase (string id)))))
    (json:encode-json obj stream)))

(defparameter *max-length* (* 1024 1024))

(defun app (env)
  (log4cl:log-info env)
  (log4cl:log-info "Headers: ~S" (alexandria:hash-table-plist (getf env :headers)))
  (setf (getf env :path-info)
	(strip-path-prefix (getf env :path-info)))
  (let ((content-length (getf env :content-length))
	(body)
	(session (getf env :session)))
    (log:info content-length)
    (cond
      ((null content-length))
      ((> content-length *max-length*)
       `(413
	 (:content-type "text/html")
	 (,(cl-who:with-html-output-to-string
	    (s)
	    (htm (:html (:head) (:body "Request entity too large")))))))
      (t
       (setf body (slurp-body env))))
    (with-error-logging (err)
	`(500
	  (:content-type "text/html")
	  (,(with-html-output-to-string (s)
					(:html (:body (:p "Error"))))))
      (match env
	((property :path-info (ppcre "^/complete/(.*)" thing))
	 (fccs-complete thing body))
	((property :path-info "/lztest/")
	 (let ((codes
		(map 'list #'char-code
		     (babel:octets-to-string body :encoding :utf-8))))
	   (log:info "Compressed-Body-Len ~D" (length body))
	   (log:debug codes)
	   (lzstring::decompress body
	    #+(or)(make-array (length codes) :element-type '(unsigned-byte 16)
			:initial-contents codes)))
	 `(200
	   ()
	   ()))
	((property :path-info "/")
	 (with-db-connection
	 `(200
	   (:content-type "text/html")
	     (,(render-page
	      (s)
	      (:script
	       (princ "React.render(React.createElement(CharacterList, {value : " s)
	       (princ "Immutable.fromJS(" s)
	       (encode-classish
		(loop for id in (get-all-character-ids)
		   for character = (get-character id)
		   collect (make-character-summary
			    :id id
			    :char-name
			    (aget :character-name character)
			    :player-name
			    (aget :player-name character)
			    :career-level
			    (calculate-field :career-level character)))
		s)
	       (princ ")}),document.getElementById(\"react-content\"))" s)))))))
	((and (property :request-method :POST)
	      (property :path-info "/new-character/"))
	 (with-db-connection
	   (let ((id (new-character)))
	     `(303
	       (:location ,(fixup-path (format nil "/character/~A" id))
			  :content-type "text/html")
	       (,(cl-who:with-html-output-to-string
		  (s)
		  (:htm (:head) (:body "See Other"))))))))
	((property :path-info (ppcre "^/pdf-character/(\\d*)$" id))
	 (let ((character
		(with-db-connection
		  (get-character id))))
	   (when character
	     (let*
		 ((pdf (fill-pdf character))
		  (bin-pdf (make-array (length pdf)
				       :element-type '(unsigned-byte 8))))
	       (map-into bin-pdf #'char-code pdf)
	       `(200
		 (:content-length ,(length bin-pdf)
				  :content-type "application/pdf")
		 ,bin-pdf)))))
	((and (property :request-method :POST)
	      (property :path-info (ppcre "^/save-character/(\\d*)$" id)))
	 (log:info "Uncompressed-Body-Len ~D" (length body))
	 (with-db-connection
	   (if (get-character id) ;;TODO just check if id is valid
	       (let* ((parsed-body (parse-json-body body))
		     (fixed-char (fixup-fc-character parsed-body)))
		 (when (fc-character-p fixed-char)
		   (log:info "We have a character!")
		   (save-character id fixed-char))
		 `(200
		   (:content-type "text/html")))
	       `(404
		 (:content-type "text/html")
		 (cl-who:with-html-output-to-string (s)
		   (:htm (:head) (:body "Couldn't locate character")))))))
	((property :path-info (ppcre "^/character/(\\d*)$" id))
	 (with-db-connection
	   (let ((character (get-character id)))
	     (if character
		 `(200
		   (:content-type "text/html")
		   (,(render-page
		      (s)
		      (:script
		       (format s "React.render(React.createElement(CharacterMenu, {characterId: ~D, defaultValue : " id)
		       (princ "fixupFcCharacter(Immutable.fromJS(" s)
		       (encode-classish character s)
		       (princ "))}),document.getElementById(\"react-content\"))" s)))))
		 `(404
		   (:content-type "text/html")
		   (,(cl-who:with-html-output-to-string (s)
							(:htm (:head)
							      (:body (:P "Error: could-not-find character"))))))))))
	((and
	  (property :path-info "/account/")
	  (property :remote-user username))
	 (with-db-connection
	   `(200
	     (:content-type "text/html")
	     (,(render-page
		(s)
		(:script
		 (format s
			 "React.render(React.createElement(AccountSettings, {user: \"~A\"}),document.getElementById(\"react-content\"))" username)))))))
	((and
	  (property :path-info "/set-password/")
	  (property :request-method :post)
	  (property :remote-user username))
	 (change-user-password username
			       (flexi-streams:octets-to-string body :external-format :utf-8))
	 `(200
	   (:content-type "text/html")))
	(_
	 `(404
	   (:content-type "text/html")
	   (,(cl-who:with-html-output-to-string (s)
						(:htm (:head) (:body (:p "Not found")))))))))))

(defvar *myapp* nil)

(defun authenticate (user pass)
  (validate-user user pass))

(defun start ()
  (setf *myapp*
	    (clack:clackup
	 (clack.builder:builder
	  ;;clack.middleware.logger:<clack-middleware-logger>
	  (authenticated-session
	   :root-path *prepend-path*
	   :after-login (lambda (env)
			  (declare (ignorable env))
			  `(303
			    (:location ,*prepend-path*)
			    ("")))
	   :render-login
	   (lambda (env)
	     (declare (ignorable env))
	     `(200
	       (:content-type "text/html; charset=utf-8")
	       (,(login-page))))
	   :login-url (format nil "~Alogin/" *prepend-path*)
	   :authenticator (lambda (user pass)
			    (funcall 'authenticate user pass)))
	  (clack.middleware.static:<CLACK-MIDDLEWARE-STATIC>
	   :path (format nil "~apub/" *prepend-path*)
	   :root (asdf:system-relative-pathname 'cl-fccs "build/pub/"))
	  (lambda (env) (funcall 'app env)))
	 :server :mongrel2
	 :sub-addr #+dev"tcp://127.0.0.1:9997"
	           #-dev"tcp://127.0.0.1:9993"
	 :pub-addr #+dev"tcp://127.0.0.1:9996"
	           #-dev"tcp://127.0.0.1:9992"
	 :port #+dev 5001 #-dev 5000
	 :use-thread nil
	 :use-default-middlewares nil
	 )))

(defun stop ()
  (when *myapp*
  (clack:stop *myapp*)))

(defun restart-server ()
  (stop) (Start))


(defun complete-from-list (partial list)
  (when (>= (length partial) 1)
    (loop for item in list
       when (alexandria:starts-with-subseq (string-downcase partial) (string-downcase item))
       collect item)))

(defun encode-json-list-to-string (list)
  (let ((json::*json-list-encoder-fn* #'json::encode-json-list-explicit-encoder))
    (json:encode-json-to-string (cons :list list))))

(defun fccs-complete (thing body)
  (let ((partial (json:decode-json-from-string (babel:octets-to-string body))))
    `(200
      (:content-type "application/json")
      (,(encode-json-list-to-string
	 (cond
	   ((equalp thing "spell-info")
	    (assoc partial fccg::+spells+ :test #'equalp))
	   ((equalp thing "gear")
	    (complete-from-list partial +all-gear-names+))
	   ((equalp thing "gear-info")
	    (lookup-gear partial))
	   ((equalp thing "spell")
	    (complete-from-list partial (mapcar
					 (compose #'better-capitalize #'car)
					 fccg::+spells+)))
	   ((equalp thing "species")
	    (complete-from-list partial
	     (hash-table-keys +species-hash+)))
	   ((equalp thing "specialty")
	    (complete-from-list partial
	     (hash-table-keys +specialty-hash+)))
	   ((equalp thing "class")
	    (complete-from-list
	     partial
	     (hash-table-keys +class-hash+)))
	   ((equalp thing "feat")
	    (complete-from-list
	     partial
	     (mapcar #'better-capitalize
		     (hash-table-keys +feat-hash+))))
	   ((equalp thing "talent")
	    (complete-from-list partial
	     (hash-table-keys +talent-hash+)))
	   (t nil)))))))

(defun lookup-gear (name)
  (loop for item in fccg::+gear+
     when (member name (mapcar #'car (cddr item)) :test #'equalp)
     do
       (let ((info (find name (cddr item) :key #'car :test #'equalp))
	     (effect-idx (position "Effect" (cadr item) :test #'equal))
	     (size-idx (position "SZ/Hand" (cadr item) :test #'equal))
	     (weight-idx (position "Weight" (cadr item) :test #'equal)))
	 (return
	   `(,name ,(if effect-idx
			(elt info effect-idx)
			"")
		   ,@(if size-idx
			 (destructuring-bind (size hand)
			     (split-sequence #\/ (elt info size-idx))
			   (list (string-downcase size) hand))
			 '("" ""))
		   ,(if weight-idx
			(let ((weight-str (elt info weight-idx)))
			  (parse-number (subseq weight-str 0
						(position #\Space weight-str))))
			""))))))

(in-package clack.app.file)
(defmethod serve-path
    ((this <clack-app-file>) env (file pathname) (encoding string))
  (let ((content-type (or (mime-lookup file) "text/plain"))
	(univ-time (or (file-write-date file)
		       (get-universal-time))))
    (when (text-file-p content-type)
      (setf content-type
	    (format nil "~A;charset=~A"
		    content-type encoding)))
    (let* ((gzfile
	    (and 
	     (cl-ppcre:scan "gzip" (gethash "accept-encoding" (getf env :headers)))
	     (probe-file (pathname (format nil "~A.gz" (namestring file))))))
	   (file (or gzfile file)))
      (log:info gzfile)
      (log:info (alexandria:hash-table-plist (getf env :headers)))
      (with-open-file (stream file
			      :direction :input
			      :if-does-not-exist nil)
	`(200
	  (:content-type ,content-type
			 :content-length ,(file-length stream)
			 ,@(when gzfile '(:content-encoding "gzip"))
			 :last-modified
			 ,(format-rfc1123-timestring nil
						     (universal-to-timestamp univ-time)))
	  ,file)))))
