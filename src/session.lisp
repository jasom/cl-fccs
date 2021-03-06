(in-package :cl-fccs)
(defparameter *session-unauthenticated-expiry-time* (* 60 60)) ;; 1 hou
(defparameter *session-expiry-time* (* 60 60 24) ;; 1 day
  "Session Expiration time in seconds") 

(defgeneric call (obj env))

(defun generate-session-id ()
  ;; 12 bytes of random data gives us ~6e-12 chance of collision on 1e9 sessions
  (random-base64 12))

(defun generate-csrf-token ()
  (random-base64 8))

(defclass session ()
  ((session-id :type string :initform (generate-session-id)
	       :reader session-id )
   (csrf-token :type string :initform (generate-csrf-token)
	       :reader session-csrf-token)
   (username :type (or string null) :initarg :username :initform nil
	     :accessor session-username)))

(defun session-cookie (session path)
  "Generate value for set-cookie for given session"
  (format nil "session-id=~A; path=~a" (session-id session) path))

(defun make-session (&optional username)
  (with-db-connection ()
    (let ((session (make-instance 'session :username username)))
      (set-session (session-id session) session)
      (expire-session (session-id session) *session-unauthenticated-expiry-time*)
      session)))

(defmethod (setf session-username) :around (new-value object) 
  (prog1
      (call-next-method)
    ;; Prevent session-fixation attacks
    (setf (slot-value object 'csrf-token)
	  (generate-csrf-token))
    ;;Persist value
    (update-session object)))

(defun update-session (session)
  (with-db-connection ()
    (set-session (session-id session) session)
    (expire-session (session-id session)
		    (if (session-username session)
			*session-expiry-time*
			*session-unauthenticated-expiry-time*))))

(defun invalidate-session (session)
  (with-db-connection ()
    (del-session (session-id session))))
  
(defun validate-session (env)
  (with-db-connection ()
    (let ((cookies (split-sequence #\; (gethash "cookie" (getf env :headers)))))
      ;(log:debug env)
      ;(log:debug (alexandria:hash-table-plist (getf env :headers)))
      ;(log:debug cookies)
      (when cookies
	(let ((sid
	       (loop for cookie in cookies
		  for sid = (ppcre:register-groups-bind (sid) ("^session-id=(.*)" cookie) sid)
		  when sid return it)))
	  (when sid
	    (let ((session (get-session sid)))
	      (when session
		(expire-session sid *session-expiry-time*)
		session))))))))

(defun read-until-value (stream value)
  (with-output-to-string (s)
    (loop for char = (read-char stream)
       until (eql char value) 
       do (write-char char s))))

(defun parse-user-pass (body)
  (let ((params (quri:url-decode-params body)))
    (when (and (= (length params) 2)
	       (member "username" params :key #'car :test #'equal)
	       (member "password" params :key #'car :test #'equal))
      (values (cdr (assoc "username" params :test #'equal))
	      (cdr (assoc "password" params :test #'equal))))))

(defclass authenticated-session  ()
     ((authenticator :type function
                     :initarg :authenticator
                     :initform (lambda (user pass)
                                   (declare (ignore user pass))
                                   nil)
                     :accessor authenticator)
      (render-login :type function
		    :initarg :render-login)
      (after-login :type function
		   :initarg :after-login)
      (root-path :type string
		 :initarg :root-path
		 :initform "/")
      (login-url :type string
		 :initarg :login-url
		 :initform "/login")
      (app :type function
	   :initarg :app))
  (:documentation "Clack Middleware to authenticate."))

(defun call-next (obj env) (funcall (slot-value obj 'app) env))

(defun authenticated-session (&rest args)
  (lambda (app)
    (let ((obj (apply #'make-instance 'authenticated-session :app app args)))
      (lambda (env)
	(call obj env)))))

(defmethod call ((this authenticated-session) env)
  (log:debug (validate-session env))
  (let ((session (or (validate-session env) (make-session))))
    (log:debug (session-username session))
    (with-slots (render-login login-url authenticator after-login root-path) this
      (log:debug (getf env :request-method))
      (log:debug (getf env :request-uri))
      (cond
	((session-username session)
	 (setf (getf env :remote-user) (session-username session)
	       (getf env :session) session)
	 (if (member (getf env :request-method) '(:head :get))
	     (call-next this env)
	     (cond
	       ((equal (session-csrf-token session)
		       (gethash "x-csrf-token" (getf env :headers)))
		(log:debug "CSRF Token Header Match")
		(call-next this env))
	       ((equal 
		 "application/x-www-form-urlencoded"
		 (gethash "content-type" (getf env :headers)))
		(let* ((body (slurp-body env))
		       (params (and body (quri:url-decode-params body))))
		  (log:debug
		   (list
		    (session-csrf-token session)
		    (cdr (assoc "csrf-token" params :test #'equal))))
		  (if
		   (equal
		    (session-csrf-token session)
		    (cdr (assoc "csrf-token" params :test #'equal)))
		   (progn
		     (setf (getf env :raw-body)
			   (flexi-streams:make-in-memory-input-stream body))
		     (call-next this env))
		   `(403
		     ()
		     ("CSRF Token validation failed")))))
	       (t
		`(403
		  ()
		  ("CSRF Token validation failed"))))))
	((and (eql (getf env :request-method) :post)
	      (equal (getf env :request-uri) login-url))
	 (let ((body (slurp-body env 1024)))
	   (if body
	       (multiple-value-bind (username password)
		   (parse-user-pass body)
		 (if (funcall authenticator
			      username password)
		     (progn
		       (setf (session-username session) username
			     (getf env :session) session
			     (getf env :remote-user) username)
		       (funcall after-login env))
		     (funcall render-login env)))
	       (funcall render-login env))))
	((and (eql (getf env :request-method) :get)
	      (equal (getf env :request-uri) login-url))
	 (funcall render-login env))
	(t
	 `(307
	   (:set-cookie
	    ,(session-cookie session root-path)
	    :location ,login-url)
	   ("")))))))
