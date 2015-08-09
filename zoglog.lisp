;;;; zoglog.lisp

(in-package #:zoglog)

(defparameter *reconnect-timeout* 10)

;; Restart handlers
(defun restart-change-nick (c)
  "Invoke CHANGE-NICK restart."
  (declare (ignore c))
  (format t "Changing nick~%")
  (invoke-restart 'change-nick))

(defun restart-message-parse-error (c)
  "Invoke CONTINUE restart on message parsing error."
  (format t "Parse error: ~a, line: ~a~%" c (raw c))
  (invoke-restart 'continue))

(defun restart-unknown-error (c)
  "Invoke RESTART-LOOP on other errors."
  (format t "Unknown error: ~a" c)
  (invoke-restart 'restart-loop))

(defun log-server (server port nick channels)
  "Run logging loop for specified server."
  (handler-bind ((nickname-already-in-use #'restart-change-nick)
                 (message-parse-error #'restart-message-parse-error)
                 (error #'restart-unknown-error))
    (loop do
         (restart-case
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
                                (let ((message (parse-message line
							      channels
							      server)))
                                  (process message)
                                  (when (save-p message channels)
                                      (save message)))
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
               (sleep *reconnect-timeout*))
           (restart-loop () nil)))))
