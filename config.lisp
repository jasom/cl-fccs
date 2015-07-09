(in-package :cl-fccs)

(asdf:load-system '#:clack-handler-mongrel2)
(setf *prepend-path* "/fccs2dev/")
(setf *clack-args*
      '(
	:server :mongrel2
	:sub-addr "tcp://127.0.0.1:9997"
	:pub-addr "tcp://127.0.0.1:9996"
	:worker-entry redis:connect
	:worker-entry redis:disconnect
	:use-thread nil))

;;; Hunchentoot Configuration
;; (asdf:load-system '#:clack-handler-hunchentoot)
;; (setf *clack-args* '(:server :hunchentoot :port 5000))
