(in-package :cl-fccs)

(defvar *prepend-path* "/")
(defvar *clack-args* '(:port 5000))
(defvar *myapp* nil)
  
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

(defun load-configuration ()
  (load (asdf:system-relative-pathname :cl-fccs "config.lisp")))

(defmacro render-page ((stream-name) &body body)
  `(cl-who:with-html-output-to-string (,stream-name nil
						    :indent t
						    :prologue "<!DOCTYPE html>")
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
	     (:script
	      (format ,stream-name "var PREPENDPATH = '~a'" *prepend-path*))
	     (:div :hidden t
		    :id "csrf"
		    (esc (session-csrf-token (getf env :session))))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *app-rules* (make-hash-table)))

(defmacro defapprule (name pattern &body b)
  (let ((vars (optima::pattern-variables (optima::parse-pattern pattern))))
    `(progn
       (defun ,name ,(append '(env body) vars)
	 (declare (ignorable env body))
	 ,@b)
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (gethash ',name *app-rules*) ',pattern)))))

(defmacro expand-app-rules (env body &body default-rule)
  (once-only (env body)
    `(optima:match ,env
       ,@(loop for fname being the hash-keys of *app-rules*
	    using (hash-value pattern)
	  collect `(,pattern
		    (,fname ,env ,body
			    ,@(optima::pattern-variables
			       (optima::parse-pattern pattern)))))
       (_ ,@default-rule))))

(defapprule autocomplete (property :path-info (ppcre "^/complete/(.*)" thing))
  (fccs-complete thing body))

(defapprule lztest (property :path-info "/lztest/")
  (let ((codes
	 (map 'list #'char-code
	      (babel:octets-to-string body :encoding :utf-8))))
    (log:debug "Compressed-Body-Len ~D" (length body))
    (log:debug codes)
    (lzstring::decompress body))
  `(200
    ()
    ()))

(defapprule root-rule (property :path-info "/")
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

(defapprule new-character-rule (and (property :request-method :POST)
				    (property :remote-user username)
				    (property :path-info "/new-character/"))
  (with-db-connection
    (let ((id (new-character username)))
      `(303
	(:location ,(fixup-path (format nil "/character/~A" id))
		   :content-type "text/html")
	(,(cl-who:with-html-output-to-string
	   (s)
	   (:htm (:head) (:body "See Other"))))))))


(defapprule pdf-rule (property :path-info (ppcre "^/pdf-character/(\\d*)$" id))
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

(defapprule save-char-rule (and (property :request-method :POST)
				(property :path-info (ppcre "^/save-character/(\\d*)$" id))
				(property :remote-user username))
  (log:info "Uncompressed-Body-Len ~D" (length body))
  (with-db-connection
    (cond
      ((null (get-character id))
       `(404			; No such character
	 (:content-type "text/html")
	 (cl-who:with-html-output-to-string (s)
	   (:htm (:head) (:body "Couldn't locate character")))))
      ((not (user-can-edit-character-p id username))
       `(403			;Wrong Permissions
	 (:content-type "text/html")
	 (cl-who:with-html-output-to-string (s)
	   (:htm (:head) (:body "Forbidden")))))
      (t				; Chaacter exists and We have permisison to edit it
       (let* ((parsed-body (parse-json-body body))
	      (fixed-char (fixup-fc-character parsed-body)))
	 (when (fc-character-p fixed-char)
	   (log:debug "We have a character!")
	   (save-character id fixed-char))
	 `(200
	   (:content-type "text/plain")
	   ("")))))))

(defapprule view-char-rule (property :path-info (ppcre "^/view-character/(\\d*)$" id))
  (with-db-connection
    (let ((character (get-character id)))
      (if character
	  `(200
	    (:content-type "text/html")
	    (,(render-page
	       (s)
	       (:script
		(format s (ps:ps (setf *view-only* t)))
		(format s "React.render(React.createElement(CharacterMenu, {
characterId: ~D, defaultValue : " id)
		(princ "fixupFcCharacter(Immutable.fromJS(" s)
		(encode-classish character s)
		(princ "))}),document.getElementById(\"react-content\"))" s)))))
	  `(404
	    (:content-type "text/html")
	    (,(cl-who:with-html-output-to-string (s)
						 (:htm (:head)
						       (:body (:P "Error: could-not-find character"))))))))))


(defapprule roll20-export-rule (property :path-info (ppcre "^/roll20-export/(\\d*)$" id))
  (with-db-connection
    (let ((character (get-character id)))
      (if character
	  `(200
	    (:content-type "text/plain; charset=utf-8")
	    ,(babel:string-to-octets (cl-fccs-r20::generate-json character)))
	  `(404
	    (:content-type "text/html")
	    (,(cl-who:with-html-output-to-string (s)
						 (:htm (:head)
						       (:body (:P "Error: could-not-find character"))))))))))


(defapprule edit-char-rule (and
			    (property :path-info (ppcre "^/character/(\\d*)$" id))
			    (property :remote-user username))
  (with-db-connection
    (if (user-can-edit-character-p id username)
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
							   (:body (:P "Error: could-not-find character"))))))))
	`(303
	  (:location ,(fixup-path (format nil "/view-character/~d" id)))
	  (())))))


(defapprule logout-rule (and
			 (property :path-info "/logout/")
			 (property :session session))
  (invalidate-session session)
  `(303
    (:location ,(fixup-path "/"))
    (())))

(defapprule account-rule (and
			  (property :path-info "/account/")
			  (property :remote-user username))
  (with-db-connection
    `(200
      (:content-type "text/html; charset=UTF-8")
      (,(render-page
	 (s)
	 (:script
	  (format s
		  "React.render(React.createElement(AccountSettings, {user: \"~A\"}),document.getElementById(\"react-content\"))" username)))))))

(defapprule set-password-rule (and
			       (property :path-info "/set-password/")
			       (property :request-method :post)
			       (property :remote-user username))
  (change-user-password username
			(flexi-streams:octets-to-string body :external-format :utf-8))
  `(200
    (:content-type "text/plain")
    ("")))

(defun app (env)
  (log:debug env)
  (log:debug "Headers: ~S" (alexandria:hash-table-plist (getf env :headers)))
  (log:info "Request to ~S from ~A" (getf env :request-uri) (getf env :remote-addr))
  (setf (getf env :path-info)
	(strip-path-prefix (getf env :path-info)))
  (let ((content-length (getf env :content-length)) (body))
    (log:debug content-length)
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
      (expand-app-rules env body
			`(404
			  (:content-type "text/html")
			  (,(cl-who:with-html-output-to-string (s)
							       (:htm (:head) (:body (:p "Not found"))))))))))


(defun authenticate (user pass)
  (validate-user user pass))

(defun start ()
  (load-configuration)
  (setf *myapp*
	(apply #'clack:clackup
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
	 :use-default-middlewares nil
	 *clack-args*)))

(defun stop ()
  (when *myapp*
    (clack:stop *myapp*)
    (setf *myapp* nil)))

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
			   (list
			    (if (> (length size) 1)
				"m"
				(string-downcase size))
			    hand))
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
      (log:debug gzfile)
      (log:debug (alexandria:hash-table-plist (getf env :headers)))
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
