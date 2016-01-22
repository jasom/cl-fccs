(in-package :cl-fccs)

(defun encode-classish (obj stream)
  (let ((json:*lisp-identifier-name-to-json*
	 (lambda (id)
	   (string-downcase (string id)))))
    (json:encode-json obj stream)))

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
	    (setf (gethash key (car obj-state)) val))))
       (json:*json-array-type* 'vector))
    (json:decode-json-from-string (babel:octets-to-string body :encoding :utf-8))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *app-rules* (make-hash-table)))

(defun list-app-rules ()
  (let ((*print-case* :downcase)
	(*package* (find-package :cl-fccs)))
    (loop for fname being the hash-keys of *app-rules*
       using (hash-value pattern)
       do (format t "~&~A: ~s" fname pattern))))

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

(defmacro defapprule (name pattern &body b)
  (let ((vars (optima::pattern-variables (optima::parse-pattern pattern))))
    `(progn
       (defun ,name ,(append '(env body) vars)
	 (declare (ignorable env body))
	 ,@b)
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (gethash ',name *app-rules*) ',pattern)))))

(defapprule root-rule (property :path-info "/")
  (with-db-connection ()
    `(200
      (:content-type "text/html")
      (,(render-page
	 (s)
	 (:script
	  (princ "React.render(React.createElement(CharacterList, {value : " s)
	  (princ "Immutable.fromJS(" s)
	  (encode-classish
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
	   s)
	  (princ ")}),document.getElementById(\"react-content\"))" s)))))))

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

