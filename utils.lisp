;;; Some helper functions

(in-package #:zoglog)

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

(defun string-prefix-p (string line)
  "Check if LINE starts with STRING."
  (let ((pos (search string line)))
    (and pos (= pos 0))))

(defun send-pong (stream line)
  "Send 'PONG' in answer to 'PING' line throuck STREAM."
  (let ((data (string-trim '(#\space #\return #\newline)
                           (cadr (split-sequence #\: line :count 2)))))
    (send-cmd stream "PONG :~a" data)))

(defun split-once (line seq)
  "Split string LINE by sequence SEQ once."
  (let ((pos (search seq line)))
    (if pos
        (list (subseq line 0 pos) (subseq line (+ pos (length seq))))
        (list line))))

(defun numeric-p (string)
  "Check if string contains only digits."
  (not (position-if-not #'digit-char-p string)))

(defun slice-list (lst start &optional end)
  "Save SUBSEQ wrapper for with length checking."
  (let ((len (length lst)))
    (when (or (not end) (> end len))
      (setf end len))
    (subseq lst start end)))

(defun last-but-one (lst)
  "Get last but one element from list."
  (first (rest (reverse lst))))

;; Timezones
(defun get-offset-from-zone (zone)
  "Calculate offset in seconds for ZONE string like '+12:20' or '-02:00'."
  (let* ((sign (if (equal (char zone 0) #\-) #'- #'+))
         (splitted (split-sequence #\: zone))
         (hours (parse-integer (string-trim "+-" (car splitted))))
         (minutes (parse-integer (cadr splitted))))
    (funcall sign (+ (* hours 3600) (* minutes 60)))))

(defun create-tz-name (zone)
  (concatenate 'string "UTC " zone))

(defun make-timezones (zones)
  "Create hash with timezones with strings like 'UTC +12:00' as keys
and second offsets as values."
  (let ((timezones (make-hash-table :test #'equal)))
    (loop for zone in zones
       do
         (setf (gethash (create-tz-name zone) timezones)
               (get-offset-from-zone zone)))
    timezones))

;; List of available timezones. Offsets taken from
;; https://en.wikipedia.org/wiki/List_of_UTC_time_offsets

(defparameter +timezone-offsets+
  '("-12:00" "-11:00" "-10:00" "-09:30" "-09:00" "-08:00" "-07:00" "-06:00"
    "-05:00" "-04:30" "-04:00" "-03:30" "-03:00" "-02:00" "-01:00" "+00:00"
    "+01:00" "+02:00" "+03:00" "+03:30" "+04:00" "+04:30" "+05:00" "+05:30"
    "+05:45" "+06:00" "+06:30" "+07:00" "+08:00" "+08:30" "+08:45" "+09:00"
    "+09:30" "+10:00" "+10:30" "+11:00" "+11:30" "+12:00" "+12:45" "+13:00"
    "+14:00"))

(defparameter +timezone-names+
  (mapcar #'create-tz-name +timezone-offsets+))

(defparameter +timezones+ (make-timezones +timezone-offsets+))

(defvar *default-tz* "UTC +00:00")

(defun get-offset (key)
  "Get offset or zero from timezone hash."
  (multiple-value-bind (value present) (gethash key +timezones+)
    (if present value (gethash *default-tz* +timezones+))))

(defparameter +display-date-format+
  `(:year "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
(defparameter +search-date-format+
  `(:year "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2)))
