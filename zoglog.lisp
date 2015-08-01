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

(defun set-nick (socket nick)
  "Set user name for server."
  (send-cmd socket "NICK ~a" nick)
  (send-cmd socket "USER ~a ~:*~a ~:*~a :~:*~a" nick))

(defun connect (server port nick channels)
  "Connect to IRC server and return socket."
  (let ((irc-socket (usocket:socket-connect server port)))
    (set-nick irc-socket nick)
    ;; (send-cmd irc-socket "NICK ~a" nick)
    ;; (send-cmd irc-socket "USER ~a ~:*~a ~:*~a :~:*~a" nick)
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

(defun numeric-p (string)
  "Check if string contains only digits."
  (not (position-if-not #'digit-char-p string)))

;; Errors
(define-condition nickname-already-in-use (error)
  ((text :initarg :text :reader text)))

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

;; Pretty date formatting
(defgeneric date-fmt (irc-message)
  (:documentation  "Format message date in UTC."))

(defmethod date-fmt ((msg irc-message))
  (multiple-value-bind (sec min hour day month year day-of-week dst-p tz)
      (decode-universal-time (date msg) 0)
    (declare (ignore day-of-week dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d UTC"
            year month day hour min sec)))

;; Main processing
(defgeneric process (irc-message)
  (:documentation  "Process received message."))

(defmethod process ((msg irc-message))
  (format t "~a" msg))

;; Numeric response
(defclass numeric-message (irc-message)
  ((code
    :initarg :code
    :accessor code
    :documentation "Response code.")))

(defmethod initialize-instance :after ((msg numeric-message) &key)
  "Initialize message with CHANNEL."
  (setf (code msg) (parse-integer (command msg))))

(defmethod process ((msg numeric-message))
  (with-accessors ((code code) (args args)) msg
    (when (= code 433)
      (error 'nickname-already-in-use
             :text (format nil "~{~a~^ ~}" args)))))

;; Maybe made subclass with "message" contents?
(defclass quit-message (irc-message)
  ((message
    :initarg :message
    :accessor message
    :documentation "Message contents.")))

(defmethod initialize-instance :after ((msg quit-message) &key)
  "Initialize message with contents."
  (with-accessors ((args args)
                   (message message)) msg
    (setf message (car args))))

(defclass channel-message (irc-message)
  ((channel
    :initarg :channel
    :accessor channel
    :documentation "IRC channel to which command is sent.")))

(defmethod initialize-instance :after ((msg channel-message) &key)
  "Initialize message with CHANNEL."
  (with-accessors ((args args)
                   (channel channel)) msg
    (setf channel (pop args))))

(defclass join-message (channel-message)
  ((channel
    :documentation "IRC channel which user joined.")))

(defclass privmsg-message (channel-message)
  ((channel
    :documentation "IRC channel to which message is sent.")
   (message
    :initarg :message
    :accessor message
    :documentation "Message contents.")))

(defmethod initialize-instance :after ((msg privmsg-message) &key)
  "Initialize simple PRIVMSG object, parse CHANNEL and MESSAGE."
  (with-accessors ((prefix prefix)
                   (command command)
                   (args args)
                   (message message)) msg
    (setf message (car args))))

(defclass part-message (privmsg-message)
  ((channel
    :documentation "IRC channel which user leaves.")))

(defclass action-message (privmsg-message)
  ((action
    :initarg :action
    :accessor action
    :documentation "Action contents.")))

(defmethod initialize-instance :after ((msg action-message) &key)
  "Initialize PRIVMSG action object, parse ACTION."
  (with-accessors ((message message) (action action)) msg
    (setf action (subseq (string-trim '(#\u001) message) 8))))

(defmethod print-object ((msg irc-message) stream)
  "Print generic irc-message object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: PREFIX: '~a' CMD: '~a' ARGS: (~{~a~^ ~})"
            (date-fmt msg)
            (prefix msg)
            (command msg)
            (args msg))))

(defmethod print-object ((msg privmsg-message) stream)
  "Print PRIVMSG object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: PREFIX: '~a' CHANNEL: '~a' MSG: '~a'"
            (date-fmt msg)
            (prefix msg)
            (channel msg)
            (message msg))))

(defmethod print-object ((msg action-message) stream)
  "Print PRIVMSG ACTION object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: PREFIX: '~a' CHANNEL: '~a' ACTION: '~a'"
            (date-fmt msg)
            (prefix msg)
            (channel msg)
            (action msg))))

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
      ;; Numeric
      ((numeric-p command) (init-instance 'numeric-message))
      ;; PRIVMSG ACTION
      ((and (string= command "PRIVMSG")
            (string-prefix-p (format nil "~CACTION" #\u001) (cadr args)))
       (init-instance 'action-message))
      ;; PRIVMSG
      ((string= command "PRIVMSG") (init-instance 'privmsg-message))
      ;; JOIN
      ((string= command "JOIN") (init-instance 'join-message))
      ;; PART
      ((string= command "PART") (init-instance 'part-message))
      ;; PART
      ((string= command "QUIT") (init-instance 'quit-message))
      ;; Other
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
  (handler-bind ((nickname-already-in-use
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'change-nick))))
    (let ((sock (connect server port nick channels)))
      (do ((line
            (read-line (usocket:socket-stream sock) nil)
            (read-line (usocket:socket-stream sock) nil)))
          ((not line))
        (format t "Received: ~A~%" line)
        (if (string-prefix-p "PING" line)
            (send-pong sock line)
            (restart-case
                (let ((message (parse-message line)))
                  (process message))
              (continue () nil)
              (change-nick ()
                (progn
                  (setf nick (concatenate 'string nick "-"))
                  (set-nick sock nick)))))))))
