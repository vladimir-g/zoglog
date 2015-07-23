;;;; zoglog.lisp

(in-package #:zoglog)

;;; "zoglog" goes here. Hacks and glory await!

(defmacro send-cmd (socket tpl &rest args)
  `(format (usocket:socket-stream ,socket)
           (concatenate 'string ,tpl "~C~C")
           ,@args #\return #\linefeed))

(defun connect (server port nick channels)
  (let ((irc-socket (usocket:socket-connect server port)))
    (send-cmd irc-socket "NICK ~a" nick)
    (send-cmd irc-socket "USER ~a ~:*~a ~:*~a :~:*~a" nick)
    (send-cmd irc-socket "JOIN ~{#~a~^,~}" channels)
    (finish-output (usocket:socket-stream irc-socket))
    irc-socket))

(defun string-prefix-p (string line)
  "Check if LINE starts with STRING."
  (let ((pos (search string line)))
    (and pos (= pos 0))))

(defun send-pong (socket line)
  "Send 'PONG' in answer to 'PING' line throuck SOCK."
  (let ((data (string-trim '(#\space #\return #\newline)
                           (cadr (split-sequence #\colon line :count 2)))))
    (send-cmd socket "PONG :~a" data)))

(defun log-server (server port nick channels)
  (let ((sock (connect server port nick channels)))
    (do ((line
          (read-line (usocket:socket-stream sock) nil)
          (read-line (usocket:socket-stream sock) nil)))
        ((not line))
      (format t "Received: ~A~%" line)
      (if (string-prefix-p "PING" line)
          (send-pong sock line)
          (progn
            (format t "Not ping~%"))))))
