(eval-when (:compile-toplevel)
  (time
   (with-open-file (f (asdf:system-relative-pathname 'cl-fccs "build/in.js") :direction :output :if-exists :supersede)
     (let ((*features* (cons :ps *features*))
	   (ps:*ps-print-pretty* t)
	   (ps:*indent-num-spaces* 2))
       (write-string (ps:ps-compile-file
		      (asdf:system-relative-pathname 'cl-fccs "src/ps-compat.lisp")
		      ) f)
       (write-string (ps:ps-compile-file
		      (asdf:system-relative-pathname 'cl-fccs "src/model.lisp")
		      ) f)
       (write-string (ps:ps-compile-file
		      (asdf:system-relative-pathname 'cl-fccs "src/view-macros.lisp")
		      ) f)
       (write-string (ps:ps-compile-file
		      (asdf:system-relative-pathname 'cl-fccs "src/view.lisp")
		      ) f)))))

(eval-when (:load-toplevel)
  (uiop:run-program
   (format nil "~A" (namestring (asdf:system-relative-pathname 'cl-fccs "build/do")))
   :directory (asdf:system-relative-pathname 'cl-fccs "build/")
   :output :string :error-output :output))
