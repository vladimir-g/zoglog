;;;; package.lisp

(defpackage #:zoglog
  (:use #:cl #:split-sequence)
  (:export #:start
           #:stop
	   #:*acceptor*
	   #:*logger-threads*
	   #:log-server
	   #:start-web
	   #:start-logging))
