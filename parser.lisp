;;; Parsing and message creating functions

(in-package #:zoglog)

(defun make-message (prefix command args raw &optional channels server)
  "Create generic irc message object or it's subclass."
  (flet ((init-instance (type)
           (make-instance type
                          :server server
			  :channels channels
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
      ;; PART
      ((string= command "QUIT") (init-instance 'quit-message))
      ;; Other
      (t (init-instance 'irc-message)))))

(defun parse-message (line &optional channels server)
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
        (make-message prefix command args line channels server))
    (error (c) (error 'message-parse-error :text c :raw line))))
