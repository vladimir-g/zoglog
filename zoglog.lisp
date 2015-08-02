;;;; zoglog.lisp

(in-package #:zoglog)

;; Helper functions
(defmacro send-cmd (stream tpl &rest args)
  "Send IRC command through STREAM."
  `(progn
     (format ,stream
             (concatenate 'string ,tpl "~C~C")
             ,@args #\return #\linefeed)
     (finish-output ,stream)))

(defun set-nick (stream nick)
  "Set user name for server."
  (send-cmd stream "NICK ~a" nick)
  (send-cmd stream "USER ~a ~:*~a ~:*~a :~:*~a" nick))

(defun string-prefix-p (string line)
  "Check if LINE starts with STRING."
  (let ((pos (search string line)))
    (and pos (= pos 0))))

(defun send-pong (stream line)
  "Send 'PONG' in answer to 'PING' line throuck STREAM."
  (let ((data (string-trim '(#\space #\return #\newline)
                           (cadr (split-sequence #\colon line :count 2)))))
    (send-cmd stream "PONG :~a" data)))

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

(define-condition message-parse-error (error)
  ((text :initarg :text :reader text)
   (raw :initarg :raw :reader raw)))

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
  (format t "~a~%" msg))

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
    (when (or (= code 433)              ;ERRNICKNAMEINUSE
              (= code 441))             ;ERRNICKCOLLISION
      (error 'nickname-already-in-use
             :text (format nil "~{~a~^ ~}" args))))
  (call-next-method))

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
  (handler-case
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
        (make-message prefix command args line))
    (error (c) (error 'message-parse-error :text c :raw line))))

;; Server logging

(defparameter *reconnect-timeout* 10)

(defun log-server (server port nick channels)
  "Run logging loop for specified server."
  (handler-bind ((nickname-already-in-use
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'change-nick)))
                 (message-parse-error
                  #'(lambda (c)
                      (format t "Parse error: ~a, line: ~a~%" c (raw c))
                      (invoke-restart 'continue))))
    (loop do
         (let* ((socket (usocket:socket-connect server port))
                (stream (usocket:socket-stream socket)))
           (unwind-protect
                (progn
                  (set-nick stream nick)
                  (send-cmd stream "JOIN ~{#~a~^,~}" channels)
                  (do ((line
                        (read-line stream nil)
                        (read-line stream nil)))
                      ((not line))
                    (if (string-prefix-p "PING" line)
                        (send-pong stream line)
                        (restart-case
                            (let ((message (parse-message line)))
                              (process message))
                          (continue () nil)
                          (change-nick ()
                            (progn
                              (setf nick (concatenate 'string nick "-"))
                              (set-nick stream nick)))))))
             (close stream)
             (usocket:socket-close socket))
           ;; Do-loop ends when socket disconnected, reconnect after
           ;; timeout
           (format t "Reconnecting~%")
           (sleep *reconnect-timeout*)))))
