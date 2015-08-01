;;;; zoglog.lisp

(in-package #:zoglog)

;; Helper functions

(defmacro send-cmd (socket tpl &rest args)
  "Send IRC command through SOCKET."
  `(progn
     (format (usocket:socket-stream ,socket)
              (concatenate 'string ,tpl "~C~C")
              ,@args #\return #\linefeed)
     (finish-output (usocket:socket-stream ,socket))))

(defun connect (server port nick channels)
  "Connect to IRC server and return socket."
  (let ((irc-socket (usocket:socket-connect server port)))
    (send-cmd irc-socket "NICK ~a" nick)
    (send-cmd irc-socket "USER ~a ~:*~a ~:*~a :~:*~a" nick)
    (send-cmd irc-socket "JOIN ~{#~a~^,~}" channels)
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

;; IRC message classes

(defclass irc-message ()
  ((prefix
    :initarg :prefix
    :accessor prefix
    :documentation "IRC message prefix (first word).")
   (command
    :initarg :command
    :accessor command
    :documentation "IRC message command (second word).")
   (args
    :initarg :args
    :initform '()
    :accessor args
    :documentation "IRC message args (third word and other).")
   (raw
    :initarg :raw
    :accessor raw
    :documentation "IRC message source.")
   (date
    :initarg :date
    :initform (get-universal-time)
    :accessor date
    :documentation "Message date in lisp universal date format")))

(defgeneric date-fmt (irc-message)
  (:documentation  "Format message date in UTC."))

(defmethod date-fmt ((msg irc-message))
  (multiple-value-bind (sec min hour day month year day-of-week dst-p tz)
      (decode-universal-time (date msg) 0)
    (declare (ignore day-of-week dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d UTC"
            year month day hour min sec)))

(defclass privmesg-message (irc-message)
  ((channel
    :initarg :channel
    :accessor channel
    :documentation "IRC channel to which message is sent.")
   (message
    :initarg :message
    :accessor message
    :documentation "Message contents.")))

(defmethod initialize-instance :after ((msg privmesg-message) &key)
  "Initialize PRIVMESG object, parse CHANNEL and MESSAGE."
  (with-accessors ((prefix prefix)
                   (command command)
                   (args args)) msg
    (setf (channel msg) (pop args))
    (setf (message msg) (car args))))

(defmethod print-object ((msg irc-message) stream)
  "Print generic irc-message object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: PREFIX: '~a' CMD: '~a' ARGS: (~{~a~^ ~})"
            (date-fmt msg)
            (prefix msg)
            (command msg)
            (args msg))))

(defmethod print-object ((msg privmesg-message) stream)
  "Print PRIVMESG object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: PREFIX: '~a' CHANNEL: '~a' MSG: '~a'"
            (date-fmt msg)
            (prefix msg)
            (channel msg)
            (message msg))))

;; Parsing and message creating functions 

(defun make-message (prefix command args raw)
  "Create generic irc message object or it's subclass."
  (flet ((init-instance (type)
           (make-instance type
                          :prefix prefix
                          :command command
                          :args args
                          :raw raw)))
    (cond 
      ((string= command "PRIVMSG") (init-instance 'privmesg-message))
      (t (init-instance 'irc-message)))))

(defun parse-message (line)
  "Create IRC message from received line."
  (let ((prefix "")
        (s (string-right-trim '(#\newline #\return) line))
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
          (nconc args (cdr splitted)))
        (setf args (split-sequence #\space s)))
    (setf command (pop args))
    (make-message prefix command args line)))

;; Server logging

(defun log-server (server port nick channels)
  "Run logging loop for specified server."
  (let ((sock (connect server port nick channels)))
    (do ((line
          (read-line (usocket:socket-stream sock) nil)
          (read-line (usocket:socket-stream sock) nil)))
        ((not line))
      (format t "Received: ~A~%" line)
      (if (string-prefix-p "PING" line)
          (send-pong sock line)
          (let ((message (parse-message line)))
            (format t "Message: ~a~%" message))))))
