;;; Parsing and message creating functions

(in-package #:zoglog)

(defparameter *reconnect-timeout* 10)
(defvar *read-timeout* 360)

(defun make-message (prefix command args raw &optional channels server nick)
  "Create generic irc message object or it's subclass."
  (flet ((init-instance (type)
           (make-instance type
                          :server server
                          :channels channels
                          :logger-nick nick
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
      ;; NOTICE
      ((string= command "NOTICE") (init-instance 'notice-message))
      ;; JOIN
      ((string= command "JOIN") (init-instance 'join-message))
      ;; PART
      ((string= command "PART") (init-instance 'part-message))
      ;; QUIT
      ((string= command "QUIT") (init-instance 'quit-message))
      ;; NICK
      ((string= command "NICK") (init-instance 'nick-message))
      ;; KICK
      ((string= command "KICK") (init-instance 'kick-message))
      ;; Other
      (t (init-instance 'irc-message)))))

(defun parse-message (line &optional channels server nick)
  "Create IRC message from received line."
  (handler-case
      (let ((prefix "")
            (s (string-right-trim '(#\newline #\return) line))
            (args '())
            (command ""))
        (when (eq (char s 0) #\:)
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
        (make-message prefix command args line channels server nick))
    (error (c) (error 'message-parse-error :text c :raw line))))

;; Restart handlers

(defun restart-change-nick (c)
  "Invoke CHANGE-NICK restart."
  (declare (ignore c))
  (vom:info "Changing nick")
  (invoke-restart 'change-nick))

(defun restart-kicked (c)
  "Rejoin after kick."
  (vom:info "Joining ~a after kick" c)
  (invoke-restart 'join-after-kick (text c)))

(defun restart-message-parse-error (c)
  "Invoke CONTINUE restart on message parsing error."
  (if (and (slot-exists-p c 'raw) (slot-boundp c 'raw))
      (vom:error "Parse error: ~a, line: ~a" c (raw c))
      (vom:error "Parse error: ~a" c))
  (invoke-restart 'continue))

(defun restart-stream-error (c)
  "Invoke RESTART-LOOP on stream error."
  (vom:error "Stream error: ~a" c)
  (sleep 10)
  (invoke-restart 'restart-loop))

(defun restart-banned (c)
  "Do nothing when logger was banned."
  (vom:info "Logger was banned: ~a" (text c))
  (invoke-restart 'continue))

(defun restart-unknown-error (c)
  "Invoke RESTART-LOOP on other errors."
  (vom:error "Unknown error: ~a" c)
  (sleep 10)
  (invoke-restart 'restart-loop))

(defun restart-encoding (c)
  "Continue on format encoding error."
  (vom:error "Format encoding error: ~a" c)
  (use-value #\?))

(defun make-stream (socket &optional encoding)
  "Create flexi-stream from socket."
  (let* ((encoding (or encoding :utf-8))
         (stream (usocket:socket-stream socket))
         (fmt (flexi-streams:make-external-format encoding :eol-style :crlf)))
    (flexi-streams:make-flexi-stream stream :external-format fmt)))

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

(defun send-pong (stream line)
  "Send 'PONG' in answer to 'PING' line throuck STREAM."
  (let ((data (string-trim '(#\space #\return #\newline)
                           (cadr (split-sequence #\: line :count 2)))))
    (send-cmd stream "PONG :~a" data)))

(defun set-read-timeout (socket stream timeout)
  "Set timeout for read operations on stream. Taken from hunchentoot code."
  (declare (ignorable socket stream))
  #+:ecl
  (setf (sb-bsd-sockets:sockopt-receive-timeout socket) timeout)
  #+:openmcl
  (setf (ccl:stream-input-timeout socket) timeout)
  #+:cmu
  (setf (lisp::fd-stream-timeout stream) (coerce timeout 'integer))
  #+:sbcl
  (setf (sb-impl::fd-stream-timeout stream) (coerce timeout 'single-float))
  #+:abcl
  (java:jcall (java:jmethod "java.net.Socket" "setSoTimeout"  "int")
              socket
              (* 1000 timeout)))

;; Main log loop

(defun log-server (&key server port nick channels
                   extra-commands encoding (socket-timeout 180))
  "Run logging loop for specified server."
  (update-db-channels server channels)
  (loop for channel in channels do (load-statistics server channel))
  (handler-bind ((nickname-already-in-use #'restart-change-nick)
                 (logger-was-kicked #'restart-kicked)
                 (message-parse-error #'restart-message-parse-error)
                 (stream-error #'restart-stream-error)
                 (logger-was-banned #'restart-banned)
                 (flex:external-format-encoding-error #'restart-encoding)
                 (error #'restart-unknown-error))
    (loop do
         (restart-case
             (let* ((socket (usocket:socket-connect server
                                                    port
                                                    :timeout socket-timeout
                                                    :element-type
                                                    'flexi-streams:octet))
                    (stream (make-stream socket encoding))
                    (*users-list* (make-hash-table :test #'equal)))
               (set-read-timeout (usocket:socket socket)
                                 (flex:flexi-stream-stream stream)
                                 *read-timeout*)
               (unwind-protect
                    (progn
                      (set-nick stream nick)
                      (send-cmd stream "JOIN ~{~a~^,~}" channels)
                      (loop for cmd in extra-commands
                         do (send-cmd stream cmd))
                      (vom:info "Connected to: ~{~a~^,~}" channels)
                      (do ((line
                            (read-line stream nil)
                            (read-line stream nil)))
                          ((not line))
                        (if (string-prefix-p "PING" line)
                            (progn
                              (vom:debug1 "Ping: ~a" line)
                              (send-pong stream line))
                            (restart-case
                                (let ((message (parse-message line
                                                              channels
                                                              server
                                                              nick)))
                                  (process message)
                                  (when (save-p message)
                                      (save message)))
                              (continue () nil)
                              (join-after-kick (channel)
                                (progn
                                  (sleep 3)
                                  (send-cmd stream "JOIN ~a" channel)))
                              (change-nick ()
                                (progn
                                  (setf nick (concatenate 'string nick "-"))
                                  (set-nick stream nick)
                                  (send-cmd stream "JOIN ~{~a~^,~}"
                                            channels)))))))
                 (close stream)
                 (usocket:socket-close socket))
               ;; Do-loop ends when socket disconnected, reconnect after
               ;; timeout
               (vom:info "Reconnecting")
               (sleep *reconnect-timeout*))
           (restart-loop () nil)))))
