;;;; package.lisp

(defpackage #:cl-fccs
  (:use #:cl #:alexandria #:parenscriptx #:split-sequence
	#:cl-who #:optima #:optima.ppcre #:parse-number)
  (:import-from :parenscript :chain :create :getprop :this))

(defpackage #:fccg (:use #:cl))


