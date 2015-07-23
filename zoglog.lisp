;;;; zoglog.lisp

(in-package #:zoglog)

;;; "zoglog" goes here. Hacks and glory await!

(defun test (x)
  (print x))

(defmacro send-cmd (socket tpl &rest args)
  `(format (usocket:socket-stream ,socket)
           (concatenate 'string ,tpl "~C~C")
           ,@args #\return #\linefeed))

(defun connect (server port nick channel)
  (let ((irc-socket (usocket:socket-connect server port)))
    (send-cmd irc-socket "NICK ~a" nick)
    (send-cmd irc-socket "USER ~a ~:*~a ~:*~a :~:*~a" nick)
    (send-cmd irc-socket "JOIN #~a" channel)
    (finish-output (usocket:socket-stream irc-socket))
    irc-socket))

(defun log-server (server port nick channel)
  (let ((sock (connect server port nick channel)))
    (do ((line
          (read-line (usocket:socket-stream sock) nil)
          (read-line (usocket:socket-stream sock) nil)))
        ((not line))
      (format t "~A~C" line #\newline))))
