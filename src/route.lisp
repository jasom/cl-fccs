(in-package :cl-fccs)

(defvar *route-fn*)
(defvar *prepend-path*)

(defvar *default-rule*
  (lambda (env)
    `(404
      (:content-type "text/html")
      (,(cl-who:with-html-output-to-string (s)
					   (:htm (:head) (:body (:p "Not found"))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *app-rules* (make-hash-table)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro expand-app-rules ()
    `(lambda (env)
       (declare (ignorable env))
       (optima:match env
	 ,@(loop for fname being the hash-keys of *app-rules*
	      using (hash-value pattern)
	      collect `(,pattern
			(,fname env
				,@(optima::pattern-variables
				   (optima::parse-pattern pattern)))))
	 (_ (funcall *default-rule* env))))))

(defun list-app-rules ()
  (let ((*print-case* :downcase)
	(*package* (find-package :cl-fccs)))
    (loop for fname being the hash-keys of *app-rules*
       using (hash-value pattern)
       do (format t "~&~A: ~s" fname pattern))))



(defmacro defapprule (name pattern &body b)
  (let ((vars (optima::pattern-variables (optima::parse-pattern pattern))))
    `(progn
       (defun ,name ,(append '(env) vars)
	 (declare (ignorable env))
	 ,@b)
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (gethash ',name *app-rules*) ',pattern
	       *route-fn* (expand-app-rules))))))
