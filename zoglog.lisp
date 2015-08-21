;;;; zoglog.lisp

(in-package #:zoglog)

(defparameter *reconnect-timeout* 10)

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
      (vom:error "Stream error: ~a" c))
  (invoke-restart 'continue))

(defun restart-banned (c)
  "Do nothing when logger was banned."
  (vom:info "Logger was banned: ~a" (text c))
  (invoke-restart 'continue))

(defun restart-unknown-error (c)
  "Invoke RESTART-LOOP on other errors."
  (vom:error "Unknown error: ~a" c)
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
               (vom:info "Reconnecting")
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
                             :initial-bindings (list (cons
                                                      '*standard-output*
                                                      *standard-output*))
			     :name (format nil "~a-logger"
					   (getf server :server))))))

;; Config
(defvar *config*)
(defstruct conf servers web-port log-path web-log)

(defun read-config (path)
  "Read config struct from file."
  (with-open-file (in path)
    (setf *config* (read in))))

(defvar *log-stream*)
(defun create-log-file (path)
  (if path
      (setf *log-stream* (open path
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :append))))

(defvar *hunch-log*)
(defun setup-web-log (path)
  (when path
    (let ((stream (open path
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :append)))
      (setf *hunch-log* stream)
      (setf (hunchentoot:acceptor-access-log-destination
             *acceptor*) *hunch-log*)
      (setf (hunchentoot:acceptor-message-log-destination
             *acceptor*) *hunch-log*))))

;; Setup logging
(setf vom:*log-hook*
  (lambda (level package package-level)
    (declare (ignore level package-level package))
    (values t *standard-output* *log-stream*)))

;; Start app
(defun start (&optional conf-file)
  (let ((default-conf (asdf:system-relative-pathname "zoglog" "config.lisp")))
    (unless conf-file
      (setf conf-file default-conf))
    (if (probe-file conf-file)
        (progn
          (read-config conf-file)
          (start-web (conf-web-port *config*))
          (start-logging (conf-servers *config*))
          (create-log-file (conf-log-path *config*))
          (vom:config t :info)
          ;; Setup hunchentoot logging
          (setup-web-log (conf-web-log *config*)))
        (vom:info "Config not found, doing nothing."))))

(defun stop ()
  "Stop all threads."
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  (if *logger-threads*
      (dolist (th *logger-threads*)
        (bt:destroy-thread th)))
  (when *hunch-log*
    (close *hunch-log*)
    (setf *hunch-log* nil))
  (when *log-stream*
    (finish-output *log-stream*)
    (close *log-stream*)
    (setf *log-stream* nil)))
