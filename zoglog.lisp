;;;; zoglog.lisp

(in-package #:zoglog)

;; Config
(defvar *config* nil)

(defun read-config (path)
  "Read config struct from file."
  (with-open-file (in path)
    (setf *config* (read in))))

;; Logging threads utility

(defvar *logger-instances* (make-hash-table))

(defun create-logging-thread (server-name server-conf)
  "Start logging thread for server SERVER-NAME with SERVER-CONF."
  (bt:make-thread #'(lambda ()
                      (apply #'log-server server-conf))
                  :initial-bindings (list (cons
                                           '*standard-output*
                                           *standard-output*))
                  :name (format nil "~a-logger" server-name)))

(defun start-logging-thread (server-name &optional server-conf)
  "Start logging thread for SERVER-NAME, use global config when config
  isn't supplied. Raises error if config doesn't exists or logging
  already started."
  (unless server-conf
    (setf server-conf
          (cdr (assoc server-name (getf *config* :servers)))))
  (unless server-conf
    (error "Server conf required for server ~a" server-name))
  (when (gethash server-name *logger-instances*)
    (error "Logger thread already launched for server ~a" server-name))
  (let ((thread (create-logging-thread server-name
                                       server-conf)))
    (setf (gethash server-name *logger-instances*)
          thread)))

(defun stop-logging-thread (server-name)
  "Stop logging threads for SERVER-NAME if it is running."
  (let ((thread (gethash server-name *logger-instances*)))
    (when (and (bt:threadp thread) (bt:thread-alive-p thread))
      (bt:destroy-thread thread))
    (remhash server-name *logger-instances*)))

(defun start-logging (&optional (servers (getf *config* :servers)))
  "Start logger for each server in config plist."
  (loop for server in servers
       do (start-logging-thread (car server) (cdr server))))

(defun stop-logging ()
  "Stop all running logging threads"
  (loop for server-name being the hash-keys in *logger-instances*
     do (stop-logging-thread server-name)))

;; Logging

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
  "Setup hunchentoot logging."
  (when (and path *acceptor*)
    (let ((stream (open-log-file path)))
      (setf *hunch-log* stream)
      (setf (hunchentoot:acceptor-access-log-destination
             *acceptor*) *hunch-log*)
      (setf (hunchentoot:acceptor-message-log-destination
             *acceptor*) *hunch-log*))))

;; Flush file log after write. Maybe this isn't very good for
;; performance.
(add-post 'vom::do-log
  (if *log-stream*
      (finish-output *log-stream*)))

;; Setup logging
(setf vom:*log-hook*
  (lambda (level package package-level)
    (declare (ignore level package-level))
    (if (and *log-stream* (eq package :zoglog))
        (values t *log-stream*)
        (values t))))

;; Setup database if config provided
(defun setup-database (&optional (config *config*))
  (setf *database-name* (getf config :database-name))
  (setf *database-user* (getf config :database-user))
  (setf *database-host* (getf config :database-host))
  (setf *database-password* (getf config :database-password)))

;; Start app
(defun start (&optional conf-file)
  (let ((default-conf (asdf:system-relative-pathname "zoglog" "config.lisp")))
    (unless conf-file
      (setf conf-file default-conf))
    (if (probe-file conf-file)
        (progn
          (read-config conf-file)
          (vom:config t (or (getf *config* :log-level) :info))
          (create-log-file (getf *config* :log-path))
          (when (getf *config* :read-timeout)
            (setf *read-timeout* (getf *config* :read-timeout)))
          (setup-database *config*)
          (init-db)
          (start-logging (getf *config* :servers))
          ;; Setup hunchentoot logging
          (start-web (getf *config* :web-port)
                     (getf *config* :web-listen-addr))
          (setup-web-log (getf *config* :web-log)))
        (progn
          (vom:info "Config not found.")
          (init-db)
          (start-web)))))

(defun stop ()
  "Stop all loggers and web interface, close logs."
  (stop-web)
  (stop-logging)
  (clear-stats-cache)
  (when *hunch-log*
    (close *hunch-log*)
    (setf *hunch-log* nil))
  (when *log-stream*
    (finish-output *log-stream*)
    (close *log-stream*)
    (setf *log-stream* nil)))
