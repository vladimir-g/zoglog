;;;; zoglog.lisp

(in-package #:zoglog)

;;; "zoglog" goes here. Hacks and glory await!

(defmacro send-cmd (socket tpl &rest args)
  "Send IRC command through SOCKET."
  `(format (usocket:socket-stream ,socket)
           (concatenate 'string ,tpl "~C~C")
           ,@args #\return #\linefeed))

(defun connect (server port nick channels)
  "Connect to IRC server and return socket."
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

(defun split-once (line seq)
  "Split string LINE by sequence SEQ once."
  (let ((pos (search seq line)))
    (list (subseq line 0 pos) (subseq line (+ pos (length seq))))))

(defun make-message (line)
  "Create IRC message from received line."
  (let ((prefix "")
        (s line)
        (args '())
        (command ""))
    (when (eq (char s 0) #\colon)
        (let ((splitted (split-once s " ")))
          (setf s (cadr splitted))
          (setf prefix (subseq (car splitted) 1))))
    (if (search ":" s)
        (let ((splitted (split-once s " :")))
          (setf s (car splitted))
          (setf args (split-sequence #\space s))
          (nconc args (cadr splitted)))
        (setf args (split-sequence #\space s)))
    (setf command (pop args))
    (list prefix command args)))

(defun log-server (server port nick channels)
  "Run logging loop for specified server."""
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
