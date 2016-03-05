;;; Message statistics processing with in-memory cache
(in-package #:zoglog)

(defvar *message-stats* (make-hash-table :test #'equal
                                         #+sbcl :synchronized #+sbcl t))

(defvar *message-stats* (make-hash-table :test #'equal))

(defvar *message-stats-lock* (bt:make-lock "stats"))

(defmacro with-stats-lock (&body body)
  "Execute BODY with lock on implementations where lock is
required. CCL and SBCL (with :synchronized t) have lock-free
hash-tables."
  #+(or ccl sbcl) `(progn ,@body)
  #-(or ccl sbcl) `(bt:with-lock-held (*message-stats-lock*) ,@body))

(defun compare-user (first second)
  "Compare two users by message count and nick in reverse order."
  (cond
    ((> (getf first :messages) (getf second :messages)) t)
    ((= (getf first :messages) (getf second :messages))
     (string-lessp (getf first :nick) (getf second :nick)))))

(defun load-statistics (server channel)
  "Load stats for channel from database and fill memory cache."
  (let ((db-stats (get-db-message-stats :server server :channel channel))
        (stats (make-hash-table :test #'equal)))
    (loop for row in db-stats
       do (setf (gethash (car row) stats) (cadr row)))
    (with-stats-lock
      (setf (gethash (cons server channel) *message-stats*) stats))
    stats))

(defun get-cached-statistics (&key server channel)
  "Get copy of cached stats for channel from memory."
  (with-stats-lock
    (let ((stats (gethash (cons server channel) *message-stats*)))
      (when stats
        (copy-hash-table stats)))))

(defun prepare-message-stats (stats)
  "Process message stats data for channel."
  (let ((count (loop for i being the hash-values of stats sum i))
        (active-count 0)
        (all-count 0))
    (list
     :users (progn
             (stable-sort
              (loop for nick being the hash-keys in stats
                 using (hash-value messages)
                 collect (list
                          :nick nick
                          :messages messages
                          :share (if (plusp count)
                                     (* 100 (/ messages count))
                                     0))
                 when (> messages 0)
                 do (incf active-count)
                 do (incf all-count))
              #'compare-user))
     :count count
     :active-count active-count
     :all-count all-count)))

(defun incf-message-count (&key server channel nick)
  "Increment message count for user on channel."
  (with-stats-lock
    (incf (gethash nick
                   (gethash (cons server channel) *message-stats*)
                   0))))

(defun add-new-user-to-stats (&key server channel nick)
  "Add new user to cached message stats hash."
  (with-stats-lock
    (let ((count (gethash nick (gethash (cons server channel)
                                        *message-stats*))))
      (when (eq count nil)
        (setf (gethash nick (gethash (cons server channel)
                                     *message-stats*))
              0)))))

(defun get-message-stats (&key server channel)
  "Get processed message statistics from cache. Stats hash must be
already filled when LOG-SERVER started, so loading from db in this
function needed only as fallback and allowed when web interface
started without logger threads."
  (let ((stats (get-cached-statistics :server server :channel channel)))
    (unless stats
      (setf stats (load-statistics server channel)))
    (prepare-message-stats stats)))

(defun clear-stats-cache ()
  "Remove all entries from message stats cache."
  (with-stats-lock
    (loop for k being the hash-keys of *message-stats*
       do (remhash k *message-stats*))))
