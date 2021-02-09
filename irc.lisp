;;; Parsing and message creating functions

(in-package #:zoglog)

(defparameter *reconnect-timeout* 20)
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
      ;; CAP
      ((string= command "CAP") (init-instance 'cap-message))
      ;; AUTHENTICATE
      ((string= command "AUTHENTICATE") (init-instance 'auth-message))
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

;; SASL related

(defun server-has-sasl (c)
  "Server has SASL, continue with authorization."
  (declare (ignore c))
  (vom:info "Server has SASL support")
  (invoke-restart 'authenticate-plain))

(defun server-has-no-sasl (c)
  "Server has no SASL support, stop authorization process."
  (declare (ignore c))
  (vom:info "Server has no SASL support")
  (invoke-restart 'sasl-finished))

(defun auth-is-allowed (c)
  "Server returned AUTHENTICATION +, continue with SASL auth."
  (declare (ignore c))
  (vom:info "Server allows auth")
  (invoke-restart 'authenticate-sasl))

(defun auth-is-successful (c)
  "SASL authentication was successful."
  (declare (ignore c))
  (vom:info "Successful SASL authentication")
  (invoke-restart 'sasl-finished))

(defun auth-is-failed (c)
  "SASL authentication was successful."
  (vom:error "SASL auth failed: ~a ~a" (code c) (text c))
  (invoke-restart 'break-loop))

;; Working with streams and sockets

(defun make-stream (socket &optional encoding tls)
  "Create flexi-stream from socket."
  (let* ((encoding (or encoding :utf-8))
         (stream (usocket:socket-stream socket))
         (fmt (flexi-streams:make-external-format encoding :eol-style :crlf)))
    (if tls
        (cl+ssl:make-ssl-client-stream stream :external-format fmt)
        (flexi-streams:make-flexi-stream stream :external-format fmt))))

(defun make-socket (&key server port timeout tls)
  (usocket:socket-connect server
                          port
                          :timeout timeout
                          :element-type
                          (if tls '(unsigned-byte 8) 'flexi-streams:octet)))

(defmacro send-cmd (stream tpl &rest args)
  "Send IRC command through STREAM."
  `(progn
     (format ,stream
             (concatenate 'string ,tpl "~C~C")
             ,@args #\return #\linefeed)
     (finish-output ,stream)
     (vom:debug1 (concatenate 'string "Sent cmd: " ,tpl) ,@args)))

(defun set-nick (stream nick)
  "Set user name for server."
  (send-cmd stream "NICK ~a" nick)
  (send-cmd stream "USER ~a ~:*~a ~:*~a :~:*~a" nick))

(defun send-pong (stream line)
  "Send 'PONG' in answer to 'PING' line through STREAM."
  (let ((data (string-trim '(#\space #\return #\newline)
                           (cadr (split-sequence #\: line :count 2)))))
    (send-cmd stream "PONG :~a" data)))

(defun join-channels (stream channels)
  "Join to list of channels."
  (when channels
    (send-cmd stream "JOIN ~{~a~^,~}" channels)
    (vom:info "Joining to ~{~a~^,~}" channels)))

(defun get-real-stream (stream tls)
  "Get real stream from flexi or ssl stream for setting timeout."
  (let ((stream (flex:flexi-stream-stream stream)))
    (if tls
        (cl+ssl::ssl-stream-socket stream)
        stream)))

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

(defun log-server (&key server port nick channels username password
                     extra-commands encoding (socket-timeout 180) tls (options '()))
  "Run logging loop for specified server."
  (update-db-channels server channels)
  (loop for channel in channels do (load-statistics server channel))
  (defun has-option (option) (member option options))
  (handler-bind ((nickname-already-in-use #'restart-change-nick)
                 (logger-was-kicked #'restart-kicked)
                 (message-parse-error #'restart-message-parse-error)
                 (stream-error #'restart-stream-error)
                 (logger-was-banned #'restart-banned)
                 (flex:external-format-encoding-error #'restart-encoding)
                 ;; SASL
                 (has-sasl #'server-has-sasl)
                 (no-sasl #'server-has-no-sasl)
                 (auth-allow #'auth-is-allowed)
                 (sasl-success #'auth-is-successful)
                 (sasl-failed #'auth-is-failed)
                 ;; Connection
                 (motd-received #'(lambda (c)
                                    (declare (ignore c))
                                    (unless (or (and username password) ; in-sasl
                                                (has-option 'send-on-connect))
                                      (invoke-restart 'connect-event))))
                 ;; Common error
                 (error #'restart-unknown-error))
    (loop do
         (restart-case
             (let* ((socket (make-socket :server server
                                         :port port
                                         :timeout socket-timeout
                                         :tls tls))
                    (stream (make-stream socket encoding tls))
                    (*users-list* (make-hash-table :test #'equal))
                    (in-sasl (and username password)))
               (set-read-timeout (usocket:socket socket)
                                 (get-real-stream stream tls)
                                 *read-timeout*)
               (defun join () (join-channels stream channels))
               (defun extra ()
                 (loop for cmd in extra-commands
                       do (send-cmd stream cmd)))
               (unwind-protect
                    (progn
                      (when in-sasl
                        (send-cmd stream "CAP REQ :sasl"))
                      (set-nick stream nick)
                      (when (and (not in-sasl)
                                 (has-option 'send-on-connect))
                        (vom:debug1 "Joining on connect")
                        (extra)
                        (join))
                      ;; Main loop
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
                              ;; Restarts
                              (continue () nil)
                              (authenticate-plain ()
                                (send-cmd stream "AUTHENTICATE PLAIN"))
                              (sasl-finished ()
                                (progn
                                  (setf in-sasl nil)
                                  (send-cmd stream "CAP END")
                                  (invoke-restart 'connect-event)))
                              (authenticate-sasl ()
                                (send-cmd stream "AUTHENTICATE ~a"
                                          (sasl-credentials username
                                                            password)))
                              (connect-event ()
                                (progn
                                  (vom:debug1 "Connected")
                                  (extra)
                                  (join)))
                              (join-after-kick (channel)
                                (progn
                                  (sleep 3)
                                  (join-channels stream (list channel))))
                              (change-nick ()
                                (progn
                                  (setf nick (concatenate 'string nick "-"))
                                  (set-nick stream nick)
                                  (join)))))))
                 (close stream)
                 (usocket:socket-close socket))
               ;; Do-loop ends when socket disconnected, reconnect after
               ;; timeout
               (vom:info "Reconnecting")
               (sleep *reconnect-timeout*))
           (restart-loop () nil)
           (break-loop () (return))))))
