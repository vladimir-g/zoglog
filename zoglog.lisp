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

(defun make-stream (socket &optional encoding)
  (let* ((encoding (or encoding :utf-8))
         (stream (usocket:socket-stream socket))
         (fmt (flexi-streams:make-external-format encoding :eol-style :crlf)))
    (flexi-streams:make-flexi-stream stream :external-format fmt)))

(defun log-server (server port nick channels
                   &optional extra-commands encoding)
  "Run logging loop for specified server."
  (update-db-channels server channels)
  (handler-bind ((nickname-already-in-use #'restart-change-nick)
                 (logger-was-kicked #'restart-kicked)
                 (message-parse-error #'restart-message-parse-error)
                 (stream-error #'restart-stream-error)
                 (logger-was-banned #'restart-banned)
                 (error #'restart-unknown-error))
    (loop do
         (restart-case
             (let* ((socket (usocket:socket-connect server
                                                    port
                                                    :element-type
                                                    'flexi-streams:octet))
                    (stream (make-stream socket encoding))
                    (*users-list* (make-hash-table :test #'equal)))
               (unwind-protect
                    (progn
                      (set-nick stream nick)
                      (send-cmd stream "JOIN ~{#~a~^,~}" channels)
                      (when extra-commands
                        (loop for cmd in extra-commands
                             do (send-cmd stream cmd)))
                      (vom:info "Connected to: ~{#~a~^,~}" channels)
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
                                  (set-nick stream nick)
                                  (send-cmd stream "JOIN ~{#~a~^,~}"
                                            channels)))))))
                 (close stream)
                 (usocket:socket-close socket))
               ;; Do-loop ends when socket disconnected, reconnect after
               ;; timeout
               (vom:info "Reconnecting")
               (sleep *reconnect-timeout*))
           (restart-loop () nil)))))

(defvar *logger-threads* nil)

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
                                             (getf server :extra)
                                             (getf server :encoding)))
                             :initial-bindings (list (cons
                                                      '*standard-output*
                                                      *standard-output*))
                             :name (format nil "~a-logger"
                                           (getf server :server))))))

;; Config
(defvar *config* nil)
(defstruct conf
  servers
  web-port
  log-path
  web-log
  database-user
  database-password
  database-name
  database-host
  log-level)

(defun read-config (path)
  "Read config struct from file."
  (with-open-file (in path)
    (setf *config* (read in))))

(defun open-log-file (path)
  "Get log file stream in utf-8."
  (flexi-streams:make-flexi-stream (open path
                                         #+ccl :sharing #+ccl :lock
                                         :direction :output
                                         :if-does-not-exist :create
                                         :element-type 'flexi-streams:octet
                                         :if-exists :append)
                                   :external-format :utf-8))

(defvar *log-stream* nil)
(defun create-log-file (path)
  "Create log file. CCL version requires lock for multithreading."
  (when path
    (setf *log-stream* (open-log-file path))))

(defvar *hunch-log* nil)
(defun setup-web-log (path)
  (when path
    (let ((stream (open-log-file path)))
      (setf *hunch-log* stream)
      (setf (hunchentoot:acceptor-access-log-destination
             *acceptor*) *hunch-log*)
      (setf (hunchentoot:acceptor-message-log-destination
             *acceptor*) *hunch-log*))))


(defmacro add-post (fun-name &body body)
  "Advise function, thanks to http://stackoverflow.com/a/5409823"
  (let ((orig (gensym)))
    `(let ((,orig (fdefinition ,fun-name))) 
       (setf (fdefinition ,fun-name) (lambda (&rest args)
                                       (apply ,orig args)
                                       ,@body)))))

;; Flush file log after write. Maybe this isn't very good for
;; performance.
(add-post 'vom::do-log
  (if *log-stream*
      (finish-output *log-stream*)))

;; Setup logging
(setf vom:*log-hook*
  (lambda (level package package-level)
    (declare (ignore level package-level package))
    (if *log-stream*
        (values t *standard-output* *log-stream*)
        (values t *standard-output*))))

;; Setup database if config provided
(defun setup-database (config)
  (setf *database-name* (conf-database-name config))
  (setf *database-user* (conf-database-user config))
  (setf *database-host* (conf-database-host config))
  (setf *database-password* (conf-database-password config)))

;; Start app
(defun start (&optional conf-file)
  (let ((default-conf (asdf:system-relative-pathname "zoglog" "config.lisp")))
    (unless conf-file
      (setf conf-file default-conf))
    (hunchentoot:reset-session-secret)
    (if (probe-file conf-file)
        (progn
          (read-config conf-file)
          (vom:config t (or (conf-log-level *config*) :info))
          (create-log-file (conf-log-path *config*))
          (setup-database *config*)
          (init-db)
          (start-logging (conf-servers *config*))
          ;; Setup hunchentoot logging
          (start-web (conf-web-port *config*))
          (setup-web-log (conf-web-log *config*)))
        (progn
          (vom:info "Config not found.")
          (init-db)
          (start-web)))))

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
