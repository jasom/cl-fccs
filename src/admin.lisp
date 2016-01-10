(in-package :cl-fccs)

(defvar *html-output-stream*)
(defvar *label-counter* 0)
(defun genlabel () (format nil "g~d" *label-counter*))


(defapprule modify-user (and (property :path-info "/admin/moduser/")
			  (property :request-method :post)
			  (property :remote-user username))
  (if (not (member :administrator (user-roles (get-user username))))
      '(403
	(:content-type "text/plain; charset=utf-8")
	("Forbidden"))
      (let ((args (alexandria:alist-hash-table (quri:url-decode-params body)
					       :test #'equal)))
	    (progn
	      (let ((user (gethash "username" args)))
		(format t "~&Username: ~A~%" user)
		(when (not (emptyp (gethash "password" args)))
		  (change-user-password user
					(gethash "password" args)))
		(if (gethash "administrator" args)
		    (add-user-role user
				   :administrator)
		    (del-user-role user
				   :administrator)))
	      `(200
		(:content-type "text/plain; charset=utf-8")
		("success")))
	  #+(or)(t ()
	    `(500
	      (:content-type "text/plain; charset=utf-8")
	      ("error"))))))

(defapprule new-user (and (property :path-info "/admin/newuser/")
			  (property :request-method :post)
			  (property :remote-user username))
  (if (not (member :administrator (user-roles (get-user username))))
      '(403
	(:content-type "text/plain; charset=utf-8")
	("Forbidden"))
      (let ((args (alexandria:alist-hash-table (quri:url-decode-params body)
					       :test #'equal)))
	(handler-case
	    (progn
	      (make-user (gethash "username" args)
			 (gethash "password" args))
	      (when (gethash "administrator" args)
		(add-user-role (gethash "username" args) :administrator))
	      `(200
		(:content-type "text/plain; charset=utf-8")
		("success")))
	  (t ()
	    `(500
	      (:content-type "text/plain; charset=utf-8")
	      ("error")))))))

(defapprule admin-panel (and (property :path-info "/admin/")
			     (property :request-method :get)
			     (property :session session)
			     (property :remote-user username))
  (if (not (member :administrator (user-roles (get-user username))))
      '(403
	(:content-type "text/plain; charset=utf-8")
	("Forbidden"))
      `(200
	(:content-type "text/html; charset=utf-8")
	(,(cl-who:with-html-output-to-string
	   (*html-output-stream* nil :indent t :prologue "<!DOCTYPE html>")
	   (:html
	    (:head
	     (:meta :charset "utf-8")
	     (:script
	      (str
	       (ps:ps*
		`(defun fixup-admin (value)
		   (cond
		     ,@(loop for user in (get-usernames)
			 collect `((= value ,user)
				   (setf
				    (chain document
					   (get-element-by-id "user-is-administrator")
					   checked)
				    ,(and (member :administrator
						  (user-roles (get-user user))) "on")))))))))
	     (:link :rel "stylesheet" :href (fixup-path "/pub/all.css"))
	     (:title "Admin Panel")
	     (:meta :name "viewport" :content "width=device=width, intial-scale=1"))
	    (:body
	     (:h2 "Admin Tools")
	     (:form
	      :method "post"
	      :action (fixup-path "/admin/newuser/")
	      :class "pure-form pure-form-aligned"
	      (:legend "New User")
	      (:input :name "csrf-token" :hidden t :value (session-csrf-token session))
	      (html-input :label "New Username" :name "username")
	      (html-input :label "New User Password" :name "password")
	      (html-input :label "Administrator?" :input-type "checkbox"
			  :name "administrator")
	      (:button :type "submit" :class "pure-button" "Create"))
	     (:form
	      :method "post"
	      :action (fixup-path "/admin/moduser/")
	      :class "pure-form pure-form-aligned"
	      (:input :name "csrf-token" :hidden t :value (session-csrf-token session))
	      (let ((users (get-usernames)))
		(html-select users
			     :onchange (ps:ps (progn (ps:chain console (log event)) (or)(fixup-admin (ps:@ event target value))))
			     :name "username"
			     :label "Username"))
	      (html-input :label "New Password" :name "password")
	      (html-input :label "Administrator?"
			  :name "administrator"
			  :input-type "checkbox" 
			  :id "user-is-administrator")
	      (:button :type "submit" :class "pure-button" "Modify")))))))))

(defun html-select (values
		    &key
		      onchange
		      initial-value
		      (class-name "pure-control-group")
		      input-class
		      name
		      label
		      (id (genlabel))
		      &allow-other-keys)
  (cl-who:with-html-output (*html-output-stream*)
    (:div
     :class class-name
     (when label
       (htm
	(:label
	 :for id
	 (esc label))))
     (:select
      :id id
      :onchange onchange
      :name name
      :class input-class
      (loop for item in values
	 do
	   (htm
	    (:option
	     :selected (eql item initial-value)
	     (esc item))))))))

(defun html-input (&key
		     initial-value
		     (class-name "pure-control-group")
		     input-class
		     (id (genlabel))
		     name
		     input-type
		     label)
  (cl-who:with-html-output (*html-output-stream*)
    (:div
     :class class-name
     (when label
       (htm
	(:label
	 :for id
	 (esc label))))
     (:input
      :id id
      :value initial-value
      :name name
      :class input-class
      :type input-type))))
       
      
