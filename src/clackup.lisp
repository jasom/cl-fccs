(in-package :cl-fccs)

(defvar *prepend-path* "/fccs2/")

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
	     ,@body)))))

(defun read-exactly-n (stream n)
  (let ((output (make-array n :element-type 'flexi-streams:octet))
	(bytes-read 0))
    (loop while (< bytes-read n)
	 do (incf bytes-read
	       (read-sequence output stream :start bytes-read)))
    output))
    

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
  (let ((content-length (getf env :content-length))
	(body))
    (cond
      ((null content-length))
      ((> content-length *max-length*)
       `(413
	 (:content-type "text/html")
	 (,(cl-who:with-html-output-to-string
	    (s)
	    (htm (:html (:head) (:body "Request entity too large")))))))
      (t
       (setf body (read-exactly-n (getf env :raw-body) content-length))))
    (with-error-logging (err)
	`(500
	  (:content-type "text/html")
	  (,(with-html-output-to-string (s)
					(:html (:body (:p "Error"))))))
      (match env
	((property :path-info "/fccs2/")
	 (with-db-connection
	 `(200
	   (:content-type "text/html")
	     (,(render-page
	      (s)
	      (:script
	       (princ "React.render(React.createElement(CharacterList, {value : " s)
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
	       (princ "}),document.body)" s)))))))
	#+(or)((property :path-info "/fccs2/error/")
	       (error "hi"))
	((and (property :request-method :POST)
	      (property :path-info "/fccs2/new-character/"))
	 (with-db-connection
	   (let ((id (new-character)))
	     `(303
	       (:location ,(format nil "/fccs2/character/~A" id)
			  :content-type "text/html")
	       (,(cl-who:with-html-output-to-string
		  (s)
		  (:htm (:head) (:body "See Other"))))))))
	((and (property :request-method :POST)
	      (property :path-info (ppcre "^/fccs2/save-character/(\\d*)$" id)))
	 (with-db-connection
	   (if (get-character id) ;;TODO just check if id is valid
	       (let ((parsed-body (parse-json-body body)))
		 (fixup-fc-character parsed-body)
		 (when (fc-character-p parsed-body)
		   (log:info "We have a character!")
		   (save-character id parsed-body))
		 `(200
		   (:content-type "text/html")))
	       `(404
		 (:content-type "text/html")
		 (cl-who:with-html-output-to-string (s)
		   (:htm (:head) (:body "Couldn't locate character")))))))
	((property :path-info (ppcre "^/fccs2/character/(\\d*)$" id))
	 (with-db-connection
	   (let ((character (get-character id)))
	     (if character
		 `(200
		   (:content-type "text/html")
		   (,(render-page
		      (s)
		      (:script
		       (format s "React.render(React.createElement(Character, {id: ~D, defaultValue : " id)
		       (princ "fixupFcCharacter(" s)
		       (encode-classish character s)
		       (princ ")}),document.body)" s)))))
		 `(404
		   (:content-type "text/html")
		   (,(cl-who:with-html-output-to-string (s)
							(:htm (:head)
							      (:body (:P "Error: could-not-find character"))))))))))
	(_
	 `(404
	   (:content-type "text/html")
	   (,(cl-who:with-html-output-to-string (s)
						(:htm (:head) (:body (:p "Not found.")))))))
			  
	))))


(defvar *myapp* nil)

(defun authenticate (user pass)
  (or
   (and (equal user "jeremie")
	(equal pass "charactersforgreatjustice"))
   #+(or)(equal user "test890")))

(defun start ()
  (setf *myapp*
	(clack:clackup
	 (clack.builder:builder
	  ;clack.middleware.logger:<clack-middleware-logger>
	  (clack.middleware.auth.basic:<CLACK-MIDDLEWARE-AUTH-BASIC>
	   :authenticator (lambda (user pass)
			    (funcall 'authenticate user pass)))
	  (clack.middleware.static:<CLACK-MIDDLEWARE-STATIC>
	   :path (format nil "~apub/" *prepend-path*)
	   :root #p"/home/aidenn/psx/pub/")
	  (lambda (env) (funcall 'app env)))
	 :server :hunchentoot
	 :use-default-middlewares nil
	 )))

(defun stop ()
  (clack:stop *myapp*))

(defun restart-server ()
  (stop) (Start))



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
