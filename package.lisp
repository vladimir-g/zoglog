;;;; package.lisp

(defpackage #:zoglog
  (:use #:cl #:split-sequence)
  (:export #:start
           #:stop
           #:*acceptor*
           #:*logger-instances*
           #:start-logging-thread
           #:stop-logging-thread
           #:start-logging
           #:stop-logging
           #:log-server
           #:start-web
           #:stop-web))
