;;; Classes and methods
 
(in-package #:zoglog)

;; Errors

(define-condition nickname-already-in-use (error)
  ((text :initarg :text :reader text)))

(define-condition logger-was-kicked (error)
  ((text :initarg :text :reader text)))

(define-condition logger-was-banned (error)
  ((text :initarg :text :reader text)))

(define-condition message-parse-error (error)
  ((text :initarg :text :reader text)
   (raw :initarg :raw :reader raw)))

(define-condition sasl-failed (error)
  ((text :initarg :text :reader text)
   (code :initarg :code :reader code)))

;; Signals for CAP/SASL

(define-condition has-sasl (condition) ())

(define-condition no-sasl (condition) ())

(define-condition auth-allow (condition) ())

(define-condition sasl-success (condition) ())


(defun default-message-date ()
  (local-time:now))

;; Classes

;; Generic IRC message
(defclass irc-message ()
  ((server
    :initarg :server
    :accessor server
    :documentation "IRC server.")
   (channels
    :initarg :channels
    :accessor channels
    :documentation "Subscribed channels.")
   (logger-nick
    :initarg :logger-nick
    :accessor logger-nick
    :documentation "Logger user (this) nick.")
   (prefix
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
    :initform (default-message-date)
    :accessor date
    :documentation "Message date in lisp universal date format")
   (nick
    :initarg :nick
    :initform nil
    :accessor nick
    :documentation "Sender nickname")
   (host
    :initarg :host
    :initform nil
    :accessor host
    :documentation "User and host name parts of nickmask")))

(defmethod initialize-instance :after ((msg irc-message) &key)
  "Initialize base message, parse username and host."
  (with-accessors ((prefix prefix) (nick nick) (host host)) msg
    (let ((splitted (split-once prefix "!")))
      (if (= (length splitted) 1)
          (setf host (car splitted))
          (setf nick (car splitted)
                host (cadr splitted))))))

(defmethod print-object ((msg irc-message) stream)
  "Print generic irc-message object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: PREFIX: '~a' CMD: '~a' ARGS: (~{~a~^ ~})"
            (date-fmt msg)
            (prefix msg)
            (command msg)
            (args msg))))

;; Pretty date formatting
(defgeneric date-fmt (irc-message)
  (:documentation  "Format message date in UTC."))

(defmethod date-fmt ((msg irc-message))
  (local-time:format-timestring nil (date msg)))

;; Process message and do some action
(defgeneric process (irc-message)
  (:documentation  "Process received message."))

(defmethod process ((msg irc-message)))

(defmethod process :after ((msg irc-message))
  (vom:debug "~a" msg))

(defgeneric save-p (irc-message)
  (:documentation  "Check if message must be saved."))

(defmethod save-p ((msg irc-message)) nil)

(defgeneric save (irc-message)
  (:documentation  "Save message to database."))

(defmethod save ((msg irc-message)) nil)

;; Generic saving to database

(defmacro save-instance (msg &rest rest)
  "Save IRC message with additional arguments (REST)."
  `(with-accessors ((date date)
                    (server server)
                    (nick nick)
                    (host host)) ,msg
     (with-db
       (postmodern:insert-dao
        (make-instance 'event
                       :date (date-to-pg date)
                       :nick nick
                       :host host
                       :server server
                       ,@rest)))))

;; AUTHENTICATE response

(defclass auth-message (irc-message) ())

(defmethod print-object ((msg auth-message) stream)
  "Princt AUTH message object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: ARGS: '~a'"
            (date-fmt msg)
            (args msg))))

(defmethod process ((msg auth-message))
  (with-accessors ((args args)) msg
    (when (string= (car args) #\+)
      (signal 'auth-allow))))

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
    (cond ((or (= code 433)              ;ERRNICKNAMEINUSE
               (= code 436))             ;ERRNICKCOLLISION
           (error 'nickname-already-in-use
                  :text (format nil "~{~a~^ ~}" args)))
          ((= code 474)                 ;USER HAS BANNED FOR THIS POST
           (error 'logger-was-banned
                  :text (format nil "~{~a~^ ~}" args)))
          ((= code 353)                 ;RPL_NAMREPLY
           (when (> (length args) 3)
             (let ((channel (elt args 2))
                   (users (split-sequence #\Space (car (slice-list args 3)))))
               (add-to-users-list channel users))))
          ((or (= code 903)             ;RPL_SASLSUCCESS
               (= code 907))            ;ERR_SASLALREADY
           (signal 'sasl-success))
          ((or (= code 902)             ;ERR_NICKLOCKED
               (= code 904)             ;ERR_SASLFAIL
               (= code 905))            ;ERR_SASLTOOLONG
           (error 'sasl-failed
                  :text args
                  :code code)))))

;; CAP message

(defclass cap-message (irc-message)
  ((subcommand
    :initarg :subcommand
    :accessor subcommand
    :documentation "CAP subcommand (LS, ACK etc).")
   (identifiers
    :initarg :identifiers
    :accessor identifiers
    :documentation "Capability identifiers.")))

(defmethod initialize-instance :after ((msg cap-message) &key)
  "Initialize message with SUBCOMMAND and IDENTIFIERS."
  (with-accessors ((args args)
                   (subcommand subcommand)
                   (identifiers identifiers)) msg
    (setf subcommand (second args))
    (setf identifiers
          (split-sequence #\Space (string-trim '(#\Space) (caddr args))))))

(defmethod print-object ((msg cap-message) stream)
  "Princt CAP message object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: SUBCOMMAND: '~a' IDENTIFIERS: '~a'"
            (date-fmt msg)
            (subcommand msg)
            (identifiers msg))))

(defmethod process ((msg cap-message))
  (with-accessors ((subcommand subcommand) (identifiers identifiers)) msg
    (when (member "sasl" identifiers :test #'string=)
      (cond ((string= subcommand "ACK")
             (signal 'has-sasl))
            ((string= subcommand "NAK")
             (signal 'no-sasl))))))

;; NICK message
(defclass nick-message (irc-message)
  ((message
    :initarg :message
    :accessor message
    :documentation "New nick.")
   (user-channels
    :initarg :user-channels
    :accessor user-channels
    :documentation "Channels to which user has joined.")))

(defmethod initialize-instance :after ((msg nick-message) &key)
  "Initialize message with contents."
  (with-accessors ((args args)
                   (nick nick)
                   (message message)
                   (user-channels user-channels)) msg
    (setf message (car args))
    (setf user-channels
          (find-user-channels nick))))

(defmethod save ((msg nick-message))
  (dolist (ch (user-channels msg))
    (save-instance msg
                   :channel ch
                   :message (message msg)
                   :message-type "NICK")))

(defmethod process ((msg nick-message))
  (with-accessors ((nick nick)
                   (server server)
                   (channels user-channels)
                   (new-nick message)) msg
    ;; Add new nick to channel list and remove old
    (dolist (ch channels)
      (remove-from-users-list ch (list nick))
      (add-new-user-to-stats :server server :channel ch :nick new-nick)
      (add-to-users-list ch (list new-nick)))))

(defmethod save-p ((msg nick-message)) t)

(defmethod print-object ((msg nick-message) stream)
  "Print NICK object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: NICK: '~a' HOST: '~a' CHANNELS: '~a' NEW NICK: '~a'"
            (date-fmt msg)
            (nick msg)
            (host msg)
            (channels msg)
            (message msg))))

;; QUIT message
;; Maybe made subclass with "message" contents?
(defclass quit-message (irc-message)
  ((message
    :initarg :message
    :accessor message
    :documentation "Message contents.")
   (quit-channels
    :initarg :quit-channels
    :accessor quit-channels
    :documentation "Channels that user leaves")))

(defmethod initialize-instance :after ((msg quit-message) &key)
  "Initialize message with contents."
  (with-accessors ((args args)
                   (message message)
                   (nick nick)
                   (channels channels)
                   (quit-channels quit-channels)) msg
    (setf message (car args))
    (setf quit-channels
          (find-user-channels nick))))

(defmethod save ((msg quit-message))
  (dolist (ch (quit-channels msg))
    (save-instance msg
                   :channel ch
                   :message (message msg)
                   :message-type "QUIT")))

(defmethod process ((msg quit-message))
  (with-accessors ((channels quit-channels) (nick nick)) msg
    (dolist (ch channels)
      (remove-from-users-list ch (list nick)))))

(defmethod save-p ((msg quit-message)) t)

(defmethod print-object ((msg quit-message) stream)
  "Print QUIT object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: NICK: '~a' HOST: '~a' CHANNELS: '~a' MSG: '~a'"
            (date-fmt msg)
            (nick msg)
            (host msg)
            (channels msg)
            (message msg))))

;; Generic message with channel argument

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

(defmethod save-p ((msg channel-message))
  (when (find (channel msg) (channels msg) :test #'equal)
    t))

(defmethod print-object ((msg channel-message) stream)
  "Print channel-type object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: NICK: '~a' HOST: '~a' CHANNEL: '~a' ARGS: '~a'"
            (date-fmt msg)
            (nick msg)
            (host msg)
            (channel msg)
            (args msg))))

;; JOIN message

(defclass join-message (channel-message)
  ((channel
    :documentation "IRC channel which user joined.")))

(defmethod save ((msg join-message))
  (save-instance msg :channel (channel msg) :message-type "JOIN"))

(defmethod process ((msg join-message))
  (with-accessors ((server server) (nick nick) (channel channel)) msg
    ;; Add user to channel users list
    (add-to-users-list channel (list nick))
    ;; Add user to cached channel stats
    (add-new-user-to-stats :server server :channel channel :nick nick)))

;; PRIVMSG message

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

(defmethod save ((msg privmsg-message))
  (with-accessors ((channel channel) (message message)) msg
    (save-instance msg
                   :channel channel
                   :message message
                   :message-type "PRIVMSG")))

(defmethod process ((msg privmsg-message))
  (with-accessors ((server server)
                   (channel channel)
                   (channels channels)
                   (nick nick)) msg
    ;; Increment message counter
    (when (find (channel msg) (channels msg) :test #'equal)
      (incf-message-count :server server :channel channel :nick nick))))

(defmethod print-object ((msg privmsg-message) stream)
  "Print PRIVMSG object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: NICK: '~a' HOST: '~a' CHANNEL: '~a' MSG: '~a'"
            (date-fmt msg)
            (nick msg)
            (host msg)
            (channel msg)
            (message msg))))

;; NOTICE message

(defclass notice-message (privmsg-message) ())

(defmethod save ((msg notice-message))
  (with-accessors ((channel channel) (message message)) msg
    (save-instance msg
                   :channel channel
                   :message message
                   :message-type "NOTICE")))

;; PART message

(defclass part-message (privmsg-message)
  ((channel
    :documentation "IRC channel which user leaves.")))

(defmethod process ((msg part-message))
  (with-accessors ((channel channel) (nick nick)) msg
    (remove-from-users-list channel (list nick))))

(defmethod save ((msg part-message))
  (with-accessors ((channel channel) (message message)) msg
    (save-instance msg
                   :channel channel
                   :message message
                   :message-type "PART")))

;; PRIVMSG ACTION
(defclass action-message (privmsg-message)
  ((action
    :initarg :action
    :accessor action
    :documentation "Action contents.")))

(defmethod initialize-instance :after ((msg action-message) &key)
  "Initialize PRIVMSG action object, parse ACTION."
  (with-accessors ((message message) (action action)) msg
    (setf action (subseq (string-trim '(#\u001) message) 7))))

(defmethod save ((msg action-message))
  (with-accessors ((channel channel) (action action)) msg
    (save-instance msg
                   :channel channel
                   :message action
                   :message-type "ACTION")))

(defmethod print-object ((msg action-message) stream)
  "Print PRIVMSG ACTION object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: PREFIX: '~a' CHANNEL: '~a' ACTION: '~a'"
            (date-fmt msg)
            (prefix msg)
            (channel msg)
            (action msg))))

;; KICK message

(defclass kick-message (channel-message)
  ((channel
    :documentation "IRC channel where user is kicked.")
   (message
    :initarg message
    :documentation "Kick reason"
    :accessor message)
   (user
    :initarg :user
    :documentation "Who was kicked"
    :accessor user)))

(defmethod initialize-instance :after ((msg kick-message) &key)
  "Initialize KICK object, parse username and message."
  (with-accessors ((message message) (user user) (args args)) msg
    (setf user (car args))
    (setf message (format nil "~{~a~^ ~}" (cdr args)))))

(defmethod process ((msg kick-message))
  (with-accessors ((user user) (ch channel) (logger-nick logger-nick)) msg
    (remove-from-users-list ch (list user))
    (when (string= user logger-nick)
      ;; Save message because condition will break irc loop here
      (save msg)
      (error 'logger-was-kicked
             :text ch))))

(defmethod save ((msg kick-message))
  (with-accessors ((channel channel) (user user) (message message)) msg
    (save-instance msg
                   :channel channel
                   :message (format nil "~a: ~a" user message)
                   :message-type "KICK")))

(defmethod print-object ((msg kick-message) stream)
  "Print channel-type object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: NICK: '~a' HOST: '~a' CHANNEL: '~a' USER: '~a'"
            (date-fmt msg)
            (nick msg)
            (host msg)
            (channel msg)
            (user msg))))

