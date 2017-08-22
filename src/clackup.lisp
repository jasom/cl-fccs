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

(defparameter *max-length* (* 1024 1024))

(defun app (env)
  (log:debug env)
  (log:debug "Headers: ~S" (alexandria:hash-table-plist (getf env :headers)))
  (log:info "Request to ~S from ~A" (getf env :request-uri) (getf env :remote-addr))
  (setf (getf env :path-info)
	(strip-path-prefix (getf env :path-info)))
  (let ((content-length (getf env :content-length)) (body))
    (log:debug content-length)
    (cond
      ((null content-length)
       `(413
	(:content-type "text/html")
	(,(cl-who:with-html-output-to-string
	   (s)
	   (htm (:html (:head) (:body "Requests without contentl-length unsupported")))))))
      ((> content-length *max-length*)
       `(413
	 (:content-type "text/html")
	 (,(cl-who:with-html-output-to-string
	    (s)
	    (htm (:html (:head) (:body "Request entity too large")))))))
      (t
       (setf (getf env :raw-body) (slurp-body env))))
    (with-error-logging (err)
	`(500
	  (:content-type "text/html")
	  (,(with-html-output-to-string (s)
					(:html (:body (:p "Error"))))))
      (log:debug body)
      (route env))))

(defun authenticate (user pass)
  (validate-user user pass))

(defun start ()
  (load-configuration)
  (setf *myapp*
	(apply #'clack:clackup
	 (lack.builder:builder
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
	  (:static
	   :path (format nil "~apub/" *prepend-path*)
	   :root (asdf:system-relative-pathname 'cl-fccs "build/pub/"))
	  'app)
	 :use-default-middlewares nil
	 *clack-args*)))

(defun stop ()
  (when *myapp*
    (clack:stop *myapp*)
    (setf *myapp* nil)))

(defun restart-server ()
  (stop) (Start))


(defmacro render-page ((stream-name) &body body)
  `(cl-who:with-html-output-to-string (,stream-name nil
						    :indent t
						    :prologue "<!DOCTYPE html>")
     (:html
      (:head
       (:meta :charset "utf-8")
       (:link :rel "stylesheet" :href (fixup-path "/pub/all.css"))
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:body 
	(:script :src (fixup-path "/pub/all.js"))
	(:script
	 (format ,stream-name "var PREPENDPATH = '~a'" *prepend-path*))
	(:div :hidden t
	      :id "csrf"
	      (esc (session-csrf-token (getf env :session))))
	(:div :id "mithril-content")
	,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun render-mithril (class options stream)
    (let ((element "document.getElementById(\"mithril-content\")"))
      (format stream "
<script>
 m.mount(~a,
         {view : function (){
            return m.component(~a, {
               ~{~A: ~A~^,~%~}
            })}})
</script>"
       element class options))))

(defapprule root-rule (property :path-info "/")
  (with-db-connection ()
    `(200
      (:content-type "text/html")
      (,(render-page
	 (s)
	 (render-mithril "CharacterList"
			 `("value" ,
			   (with-output-to-string (s)
			     
			   (princ "Immutable.fromJS(" s)
			   (encode-classish
			    (coerce
			     (loop with result = (array*)
				for id in (get-all-character-ids)
				for character = (get-character id)
				do (vector-push-extend
				    (make-character-summary
				     :id id
				     :char-name
				     (aget :character-name character)
				     :player-name
				     (aget :player-name character)
				     :career-level
				     (calculate-field :career-level character))
				    result)
				finally (return result))
			     'vector)
			    s)
			   (princ ")" s)))
			 s))))))
	  
(defapprule new-character-rule (and (property :request-method :POST)
				    (property :remote-user username)
				    (property :path-info "/new-character/"))
  (with-db-connection ()
    (let ((id (new-character username)))
      `(303
	(:location ,(fixup-path (format nil "/character/~A/" id))
		   :content-type "text/html")
	(,(cl-who:with-html-output-to-string
	   (s)
	   (:htm (:head) (:body "See Other"))))))))

(defapprule pdf-rule (property :path-info (ppcre "^/pdf-character/(\\d*)$" id))
  (let ((character
	 (with-db-connection ()
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
				(property :raw-body body)
				(property :remote-user username))
  (log:info "Uncompressed-Body-Len ~D" (length body))
  (with-db-connection ()
    (cond
      ((null (get-character id))
       `(404				; No such character
	 (:content-type "text/html")
	 (cl-who:with-html-output-to-string (s)
	   (:htm (:head) (:body "Couldn't locate character")))))
      ((not (user-can-edit-character-p id username))
       `(403				; Wrong Permissions
	 (:content-type "text/html")
	 (cl-who:with-html-output-to-string (s)
	   (:htm (:head) (:body "Forbidden")))))
      (t	   			; Chaacter exists and We have permisison to edit it
       (let* ((parsed-body (parse-json-body body))
	      (fixed-char (fixup-fc-character parsed-body)))
	 (if (fc-character-p fixed-char)
	     (progn
	       (log:debug "We have a character!")
	       (save-character id fixed-char)
	       `(200
		 (:content-type "text/plain")
		 ("")))
	     `(500
	       (:content-type "text/plain")
	       ("Error saving character"))))))))

(defapprule delete-char-rule (and
			      (property :request-method :post)
			      (property :path-info (ppcre "^/character/(\\d*)/delete/$" id))
			      (property :remote-user username))
  (with-db-connection ()
    (if (user-can-edit-character-p id username)
	(progn
	  (delete-character id)
	  `(303
	    (:location ,(fixup-path "/"))
	    (())))
	`(403				; Wrong Permissions
	  (:content-type "text/html")
	  (cl-who:with-html-output-to-string (s)
	    (:htm (:head) (:body "Forbidden")))))))

(defapprule view-char-rule (property :path-info (ppcre "^/character/(\\d*)/view/$" id))
  (with-db-connection ()
    (let ((character (get-character id)))
      (if character
	  `(200
	    (:content-type "text/html")
	    (,(render-page
	       (s)
	       (:script
		(format s (ps:ps (setf *view-only* t))))
	       (render-mithril
		"CharacterMenu"
		`("characterId"
		  ,id
		  "defaultValue"
		  ,(with-output-to-string (s)
					  (princ "fixupFcCharacter(Immutable.fromJS(" s)
					  (encode-classish character s)
					  (princ "))" s)))
		s))))
	  `(404
	    (:content-type "text/html")
	    (,(cl-who:with-html-output-to-string (s)
						 (:htm (:head)
						       (:body (:P "Error: could-not-find character"))))))))))


(defapprule roll20-export-rule (property :path-info (ppcre "^/roll20-export/(\\d*)$" id))
  (with-db-connection ()
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
			    (property :path-info (ppcre "^/character/(\\d*)/$" id))
			    (property :remote-user username))
  (with-db-connection ()
    (if (user-can-edit-character-p id username)
	(let ((character (get-character id)))
	  (if character
	      `(200
		(:content-type "text/html")
		(,(render-page
		   (s)
		   (render-mithril
		    "CharacterMenu"
		    `("characterId"
		      ,id
		      "defaultValue"
		      ,(with-output-to-string (s)
					      (princ "fixupFcCharacter(Immutable.fromJS(" s)
					      (encode-classish character s)
					      (princ "))" s)))
		    s))))
	      `(404
		(:content-type "text/html")
		(,(cl-who:with-html-output-to-string (s)
						     (:htm (:head)
							   (:body (:P "Error: could-not-find character"))))))))
	`(303
	  (:location ,(fixup-path (format nil "/character/~d/view/" id)))
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
  (with-db-connection ()
    `(200
      (:content-type "text/html; charset=UTF-8")
      (,(render-page
	 (s)
	 (render-mithril "AccountSettings" `("user" ,(format nil "'~a'" username)) s))))))

(defapprule set-password-rule (and
			       (property :path-info "/set-password/")
			       (property :request-method :post)
			       (property :raw-body body)
			       (property :remote-user username))
  (change-user-password username
			(flexi-streams:octets-to-string body :external-format :utf-8))
  `(200
    (:content-type "text/plain")
    ("")))

#+#:lztest(defapprule lztest (property :path-info "/lztest/")
  (let ((codes
	 (map 'list #'char-code
	      (babel:octets-to-string body :encoding :utf-8))))
    (log:debug "Compressed-Body-Len ~D" (length body))
    (log:debug codes)
    (lzstring::decompress body))
  `(200
    ()
    ()))


(in-package lack.app.file)
(defun serve-path2 (env file encoding)
  (let ((content-type (mimes:mime file "text/plain"))
        (univ-time (or (file-write-date file)
                       (get-universal-time))))
    (when (starts-with-subseq "text" content-type)
      (setf content-type
            (format nil "~A~:[~;~:*; charset=~A~]"
                    content-type encoding)))
    (let* ((gzfile
	    (and 
	     (cl-ppcre:scan "gzip" (gethash "accept-encoding" (getf env :headers)))
	     (probe-file (pathname (format nil "~A.gz" (namestring file))))))
	   (file (or gzfile file)))
      (log:debug gzfile)
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

(defun make-app (&key file (root #P"./") (encoding "utf-8"))
  (lambda (env)
    (handler-case
        (serve-path2
	 env
         (locate-file (or file
                          ;; remove "/"
                          (subseq (getf env :path-info) 1))
                      root)
         encoding)
      (bad-request ()
        '(400 (:content-type "text/plain"
               :content-length 11)
          ("Bad Request")))
      (not-found ()
        '(404 (:content-type "text/plain"
               :content-length 9)
          ("Not Found"))))))
