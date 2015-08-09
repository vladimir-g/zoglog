;;; Classes and methods
 
(in-package #:zoglog)

;; Errors

(define-condition nickname-already-in-use (error)
  ((text :initarg :text :reader text)))

(define-condition message-parse-error (error)
  ((text :initarg :text :reader text)
   (raw :initarg :raw :reader raw)))

;; Classes

(defclass irc-message ()
  ((server
    :initarg :server
    :accessor server
    :documentation "IRC server.")
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
    :initform (get-universal-time)
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

;; Pretty date formatting
(defgeneric date-fmt (irc-message)
  (:documentation  "Format message date in UTC."))

(defmethod date-fmt ((msg irc-message))
  (multiple-value-bind (sec min hour day month year day-of-week dst-p tz)
      (decode-universal-time (date msg) 0)
    (declare (ignore day-of-week dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d UTC"
            year month day hour min sec)))

;; Process message and do some action
(defgeneric process (irc-message)
  (:documentation  "Process received message."))

(defmethod process ((msg irc-message))
  (format t "~a~%" msg))

(defgeneric save-p (irc-message channels)
  (:documentation  "Check if message must be saved."))

(defmethod save-p ((msg irc-message) channels) nil)

(defgeneric save (irc-message)
  (:documentation  "Save message to database."))

(defmethod save ((msg irc-message))
  (format t "Saving ~a~%" msg))

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

(defmethod save-p ((msg quit-message) channels) t)

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

(defmethod save-p ((msg channel-message) channels)
  (when (find (string-left-trim "#" (channel msg)) channels :test #'equal)
    t))

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

(defclass notice-message (privmsg-message) ())

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

(defmethod print-object ((msg channel-message) stream)
  "Print channel-type object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: NICK: '~a' HOST: '~a' CHANNEL: '~a' ARGS: '~a'"
            (date-fmt msg)
            (nick msg)
            (host msg)
            (channel msg)
            (args msg))))

(defmethod print-object ((msg privmsg-message) stream)
  "Print PRIVMSG object."
  (print-unreadable-object (msg stream :type t :identity t)
    (format stream "~a: NICK: '~a' HOST: '~a' CHANNEL: '~a' MSG: '~a'"
            (date-fmt msg)
            (nick msg)
            (host msg)
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
