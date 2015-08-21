;;;; zoglog.lisp

(in-package #:zoglog)

(defparameter *reconnect-timeout* 10)

;; Restart handlers
(defun restart-change-nick (c)
  "Invoke CHANGE-NICK restart."
  (declare (ignore c))
  (log-fmt "Changing nick")
  (invoke-restart 'change-nick))

(defun restart-kicked (c)
  "Rejoin after kick."
  (log-fmt "Joining ~a after kick" c)
  (invoke-restart 'join-after-kick (text c)))

(defun restart-message-parse-error (c)
  "Invoke CONTINUE restart on message parsing error."
  (if (and (slot-exists-p c 'raw) (slot-boundp c 'raw))
      (log-fmt "Parse error: ~a, line: ~a" c (raw c))
      (log-fmt "Stream error: ~a" c))
  (invoke-restart 'continue))

(defun restart-banned (c)
  "Do nothing when logger was banned."
  (log-fmt "Logger was banned: ~a" (text c))
  (invoke-restart 'continue))

(defun restart-unknown-error (c)
  "Invoke RESTART-LOOP on other errors."
  (log-fmt "Unknown error: ~a" c)
  (sleep 1)
  (invoke-restart 'restart-loop))

(defun log-server (server port nick channels &optional extra-commands)
  "Run logging loop for specified server."
  (update-db-channels server channels)
  (handler-bind ((nickname-already-in-use #'restart-change-nick)
		 (logger-was-kicked #'restart-kicked)
                 (message-parse-error #'restart-message-parse-error)
		 (stream-error #'restart-message-parse-error)
		 (logger-was-banned #'restart-banned)
                 (error #'restart-unknown-error))
    (loop do
         (restart-case
             (let* ((socket (usocket:socket-connect server port))
                    (stream (usocket:socket-stream socket)))
               (unwind-protect
                    (progn
                      (set-nick stream nick)
                      (send-cmd stream "JOIN ~{#~a~^,~}" channels)
                      (when extra-commands
                        (loop for cmd in extra-commands
                             do (send-cmd stream cmd)))
                      (do ((line
                            (read-line stream nil)
                            (read-line stream nil)))
                          ((not line))
                        (if (string-prefix-p "PING" line)
                            (send-pong stream line)
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
                                  (set-nick stream nick)))))))
                 (close stream)
                 (usocket:socket-close socket))
               ;; Do-loop ends when socket disconnected, reconnect after
               ;; timeout
               (log-fmt "Reconnecting")
               (sleep *reconnect-timeout*))
           (restart-loop () nil)))))

(defvar *logger-threads*)

(defun start-logging (servers)
  "Start logger for each server in config plist."
  (setf *logger-threads*
	(loop for server in servers
	   collect
	     (bt:make-thread #'(lambda ()
				 (log-server (getf server :server)
					     (getf server :port)
					     (getf server :nick)
					     (getf server :channels)
					     (getf server :extra)))
			     :name (format nil "~a-logger"
					   (getf server :server))))))

;; Config
(defvar *config*)
(defstruct conf servers web-port)

(defun read-config (path)
  "Read config struct from file."
  (with-open-file (in path)
    (setf *config* (read in))))

;; Start app
(defun start (&optional conf-file)
  (let ((default-conf (asdf:system-relative-pathname "zoglog" "config.lisp")))
    (unless conf-file
      (setf conf-file default-conf))
    (if (probe-file conf-file)
        (progn
          (read-config conf-file)
          (start-web (conf-web-port *config*))
          (start-logging (conf-servers *config*)))
        (log-fmt "Config not found, doing nothing."))))
