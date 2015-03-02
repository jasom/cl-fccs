;;;; package.lisp

(defpackage #:cl-fccs
  (:use #:cl #:alexandria #:parenscriptx #:split-sequence
	#:cl-who #:optima #:optima.ppcre)
  (:import-from :parenscript :chain :create :getprop))

(defpackage #:fccg (:use #:cl))


