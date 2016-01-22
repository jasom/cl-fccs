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
      (log:debug body)
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
