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
