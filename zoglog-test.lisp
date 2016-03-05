;;;; package.lisp
(in-package #:cl-user)

(defpackage #:zoglog-test
  (:use #:cl #:fiveam #:zoglog)
  (:export #:run-tests))

(in-package #:zoglog-test)

;; Run all test suites
(defun run-tests ()
  (run! 'irc-messages-tests)
  (run! 'stat-tests)
  (run! 'pagination-tests))

;;; Message parsing tests
(def-suite irc-messages-tests
    :description "Test suite for IRC message parsing")

(in-suite irc-messages-tests)

(defparameter +channels+ '("#channel1" "#channel2"))
(defparameter +server+ "test-server")
(defparameter +logger-nick+ "test-logger")

(defun parse-message (line)
  "Parse message with some predefined args."
  (zoglog::parse-message line +channels+ +server+ +logger-nick+))

(defmacro msg-test (name prefix command args nick channel &rest check-forms)
  "Create series of tests for attributes of the message."
  (let ((line (format nil ":~a ~a ~a" prefix command args))
        (msg (gensym)))
    `(test ,name
       (let ((zoglog::*users-list* (make-hash-table :test #'equal)))
         (zoglog::add-to-users-list ,channel '(,nick))
         (let ((,msg (parse-message ,line)))
           ;; Common checks
           (is (equal ,prefix (zoglog::prefix ,msg)))
           (is (equal ,command (zoglog::command ,msg)))
           ;; Additional checks
           ,@(loop for (func-name check) in check-forms
                collect
                  (let ((func (find-symbol (symbol-name func-name) 'zoglog)))
                    `(is (equal ,check (,func ,msg))))))))))

(msg-test
 privmsg
 "user!~user@domain.tld" "PRIVMSG" "#channel1 :Hello, #channel1!"
 "user" "#channel1"
 (nick "user")
 (host "~user@domain.tld")
 (args '("Hello, #channel1!"))
 (channel "#channel1")
 (message "Hello, #channel1!"))

(msg-test
 notice
 "user!~user@domain.tld" "NOTICE" "#channel1 :Notice this"
 "user" "#channel1"
 (nick "user")
 (host "~user@domain.tld")
 (args '("Notice this"))
 (channel "#channel1")
 (message "Notice this"))

(msg-test
 join
 "user!~user@domain.tld" "JOIN" ":#channel1"
 "user" "#channel1"
 (nick "user")
 (host "~user@domain.tld")
 (channel "#channel1"))

(msg-test
 part
 "user!~user@domain.tld" "PART" "#channel1 :Bye, all!"
 "user" "#channel1"
 (nick "user")
 (host "~user@domain.tld")
 (args '("Bye, all!"))
 (channel "#channel1")
 (message "Bye, all!"))

(msg-test
 kick
 "user!~user@domain.tld" "KICK" "#channel1 user2 :was kicked for this post"
 "user" "#channel1"
 (nick "user")
 (host "~user@domain.tld")
 (channel "#channel1")
 (args '("user2" "was kicked for this post"))
 (user "user2")
 (message "was kicked for this post"))

(msg-test
 nick
 "user!~user@domain.tld" "NICK" ":newnick"
 "user" "#channel1"
 (nick "user")
 (host "~user@domain.tld")
 (args '("newnick"))
 (message "newnick"))

;; Numeric messages
(msg-test
 numeric-simple
 "server.tld" "001" "test-logger :We're an anarchosyndicalist commune."
 "user" "#channel1"
 (nick nil)
 (code 1)
 (host "server.tld")
 (args '("test-logger" "We're an anarchosyndicalist commune.")))

(test numeric-already-in-use
  (let* ((line (format nil
                       ":server.tld 433 * ~a :Nickname already in use"
                       +logger-nick+))
         (msg (parse-message line)))
    (is (eq 'zoglog::numeric-message (type-of msg)))
    (is (eq 433 (zoglog::code msg)))
    (signals zoglog::nickname-already-in-use (zoglog::process msg))))

(test numeric-logger-was-banned
  (let* ((line (format nil
                       ":server.tld 474 ~a #channel :Cannot join channel (+b)"
                       +logger-nick+))
         (msg (parse-message line)))
    (is (eq 'zoglog::numeric-message (type-of msg)))
    (is (eq 474 (zoglog::code msg)))
    (signals zoglog::logger-was-banned (zoglog::process msg))))

;; ACTION message is really a PRIVMSG with custom args
(test action
  (let* ((act (format nil "~CACTION suffers~:*~C" #\u001))
         (line (format nil
                       ":user!~~user@domain.tld PRIVMSG #channel1 :~a"
                       act))
         (msg (parse-message line)))
    (setf (zoglog::channels msg) '("#channel1"))
    (is (equal "user!~user@domain.tld" (zoglog::prefix msg)))
    (is (equal "#channel1" (zoglog::channel msg)))
    (is (equal "PRIVMSG" (zoglog::command msg)))
    (is (eq 'zoglog::action-message (type-of msg)))
    (is (eq t (zoglog::save-p msg)))
    (is (equal (list act) (zoglog::args msg)))
    (is (equal "suffers" (zoglog::action msg)))
    (is (equal "user" (zoglog::nick msg)))))

;; QUIT instance also contains list of channels which user has leaved
(test quit
  (let ((zoglog::*users-list* (make-hash-table :test #'equal)))
    (zoglog::add-to-users-list "#channel2" '("user"))
    (let ((msg (parse-message ":user!~user@domain.tld QUIT :Pong timeout")))
      (is (equal "user!~user@domain.tld" (zoglog::prefix msg)))
      (is (equal "QUIT" (zoglog::command msg)))
      (is (equal '("Pong timeout") (zoglog::args msg)))
      (is (equal "Pong timeout" (zoglog::message msg)))
      (is (equal "user" (zoglog::nick msg)))
      (is (equal '("#channel2") (zoglog::quit-channels msg)))
      (is (eq t (zoglog::save-p msg)))
      ;; User must be removed from users list
      (zoglog::process msg)
      (is (null (gethash "#channel2" zoglog::*users-list*))))))

;;; Statistics calculation tests
(def-suite stat-tests
    :description "Test suite for channel statistics")

(in-suite stat-tests)

(defun make-stats-table (users-data)
  "Create hash with users messages count."
  (let ((stats (make-hash-table :test #'equal)))
    (loop for (nick count) in users-data
       do (setf (gethash nick stats) count))
    stats))

(test statistics
  (let* ((stats (make-stats-table
                 '(("u-1" 5432) ("u-2" 1230) ("u-3" 4) ("u-4" 0))))
         (result (zoglog::prepare-message-stats stats)))
    (is (= (length (getf result :users)) 4))
    (is (= (getf result :count) (+ 5432 1230 4)))
    (is (= (getf result :active-count) 3))
    (is (= (getf result :all-count) 4))
    (loop for user in (getf result :users)
       for nick in '("u-1" "u-2" "u-3" "u-4")
       for count in '(5432 1230 4 0)
       with overall = (+ 5432 1230 4 0)
       do (progn
            (is (equal (getf user :nick) nick))
            (is (= (getf user :messages) count))
            (is (= (getf user :share) (* 100 (/ count overall))))))))

(test statistics-empty
  (let* ((stats (make-stats-table
                 '(("u-1" 0) ("u-2" 0) ("u-3" 0) ("u-4" 0))))
         (result (zoglog::prepare-message-stats stats)))
    (is (= (length (getf result :users)) 4))
    (is (= (getf result :count) 0))
    (is (= (getf result :active-count) 0))
    (is (= (getf result :all-count) 4))
    (loop for user in (getf result :users)
       for nick in '("u-1" "u-2" "u-3" "u-4")
       do (progn
            (is (equal (getf user :nick) nick))
            (is (= (getf user :messages) 0))
            (is (= (getf user :share) 0))))))

;;; Pagination tests
(def-suite pagination-tests
    :description "Test suite for message pagination")

(in-suite pagination-tests)

(test pager-links
  (let* ((hunchentoot::*acceptor* (make-instance 'hunchentoot:acceptor))
         (hunchentoot::*reply* (make-instance 'hunchentoot:reply))
         (request
          (make-instance
           'hunchentoot:request
           :headers-in '()
           :uri "/path/?from-id=12345&nick=tester&limit=20"))
         (older-id 12344)
         (newer-id 12365))
    (multiple-value-bind (newer-link older-link newest-link oldest-link)
        (zoglog::get-pager-links request older-id newer-id)
      (is (equal "/path/?from-id=12365&nick=tester&limit=20" newer-link))
      (is (equal "/path/?to-id=12344&nick=tester&limit=20" older-link))
      (is (equal "/path/?from-id=0&nick=tester&limit=20" oldest-link))
      (is (equal "/path/?nick=tester&limit=20" newest-link)))))
