(in-package :cl-fccs)

(defclass user ()
  ((username :type string :initarg :name)
   (pw-hash :type string :initarg :hash)
   (pw-salt :type string :initarg :salt)
   (pw-algo :type keyword :initarg :algo)
   (roles :type list :initform nil :accessor user-roles)
   (pw-iterations :type integer :initarg :iterations)))

(defparameter *hash-iterations* 8192)

(defun validate-user (username password)
  (with-db-connection ()
    (let ((user (get-user username)))
      (when user
	(with-slots (pw-hash pw-salt pw-algo pw-iterations) user
	  (when (equal pw-hash
		       (crypto-shortcuts:pbkdf2-hash password pw-salt
						     :digest pw-algo
						     :iterations pw-iterations))
	    user))))))

(defun make-user (username password)
  (when (position #\" username)
    (error "Invalid username: ~S" username))
  (with-db-connection ()
    (when (get-user username)
      (error "User ~S already exists" username))
    (let* ((saltb (random-bytes 4)))
      (multiple-value-bind 
	    (hash salt algo iterations)
	  (crypto-shortcuts:pbkdf2-hash password (crypto-shortcuts:to-base64 saltb) :iterations *hash-iterations*)
	(let ((user
	       (make-instance 'user
			      :name username
			      :hash hash
			      :salt salt
			      :algo algo
			      :iterations iterations)))
	  (set-user username user)
	  user)))))

;; TODO currently completely un-threadsafe
(defun change-user-password (username password)
  (with-db-connection ()
    (let ((user (get-user username)))
      (unless user (error "Invalid user: ~A" username))
      (let* ((saltb (random-bytes 4)))
	(multiple-value-bind 
	      (hash salt algo iterations)
	    (crypto-shortcuts:pbkdf2-hash password (crypto-shortcuts:to-base64 saltb) :iterations *hash-iterations*)
	  (with-slots (pw-hash pw-salt pw-algo pw-iterations) user
	    (setf pw-hash hash
		  pw-salt salt
		  pw-algo algo
		  pw-iterations iterations))))
      (set-user username user))))

;; TODO currently completely un-threadsafe
(defun add-user-role (username role)
  (with-db-connection ()
    (let ((user (get-user username)))
      (pushnew role (slot-value user 'roles))
      (set-user username user))))

(defun del-user-role (username role)
  (with-db-connection ()
    (let ((user (get-user username)))
      (with-slots (roles) user
	(setf roles (remove role roles))
	(set-user username user)))))
