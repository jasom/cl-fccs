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

(defun ensure-connected ()
  (unless (redis:connected-p)
    (redis:connect)))

(defun serialize (obj)
  (flexi-streams:with-output-to-sequence (s :element-type 'character
					    :transformer #'code-char)
    (cl-store:store obj s)))

(defun deserialize (str)
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

(defun new-character ()
  (ensure-connected)
  (let ((id (red:incr "character-id")))
    (save-character id (make-fc-character))
    id))

(defun username-id (username)
  (ensure-connected)
  (red:hget "usernames" username))

(defun get-user (id)
  (ensure-connected)
  (deserialize (red:get (format nil "user-~D" id))))
