(in-package :cl-fccs)

(defun dumb-byte-char (v)
  (declare (type (simple-array (unsigned-byte 8) (*)) v))
  "This is a dumb way to convert a byte-vector to a string.
        However, it is 100% reversible, so you can re-encode correctly if needed."
  (let ((s (make-string (length v))))
    (map-into s #'code-char v)))

(defun dumb-char-byte (s)
  (declare (type simple-string s))
  (let ((v (make-array (list (length s)) :element-type '(unsigned-byte 8)
		       :initial-element 0)))
    (dotimes (i (length s) v)
      (setf (aref v i) (char-code (aref s i))))))

(defmacro with-db-connection (&body b)
  `(let ((redis::*connection*))
     (unwind-protect
	  (progn
	    (redis:connect)
	    ,@b)
       (redis:disconnect))))

(defun ensure-connected ()
  (unless (redis:connected-p)
    (error "Redis not connected")))

(defgeneric serialize (obj))
(defgeneric deserialize (obj))

(defmethod serialize (obj)
  (flexi-streams:with-output-to-sequence (s :element-type 'character
					    :transformer #'code-char)
    (cl-store:store obj s)))

(defmethod deserialize (str)
  (when (stringp str)
    (flexi-streams:with-input-from-sequence (s (dumb-char-byte str))
      (cl-store:restore s))))

(defun get-character (id)
  (ensure-connected)
  (deserialize (red:hget "characters" id)))

(defun get-all-character-ids ()
  (ensure-connected)
  (red:hkeys "characters"))

(defun save-character (id character)
  (ensure-connected)
  (red:hset "characters" id (serialize character)))

(defun new-character (owner)
  (ensure-connected)
  (let ((id (red:incr "character-id")))
    (save-character id (make-fc-character))
    (add-user-to-character id owner)
    id))

(defun delete-character (id)
  (ensure-connected)
  (red:hdel "characters" id))

(defun get-user (username)
  (ensure-connected)
  (deserialize (red:hget "users" username)))

(defun set-user (username obj)
  (ensure-connected)
  (red:hset "users" username (serialize obj)))

(defun set-session (sid obj)
  (ensure-connected)
  (red:set (format nil "session-~A" sid) (serialize obj)))

(defun get-session (sid)
  (ensure-connected)
  (deserialize (red:get (format nil "session-~A" sid))))

(defun del-session (sid)
  (ensure-connected)
  (red:del (format nil "session-~A" sid)))

(defun expire-session (sid seconds)
  (ensure-connected)
  (red:expire (format nil "session-~a" sid) seconds))

(defun user-can-edit-character-p (character-id username)
  (red:sismember (format nil "character-users-~d" character-id) username))

(defun add-user-to-character (character-id username)
  (red:sadd (format nil "character-users-~d" character-id) username))
  
