;;; Logging for logger

(in-package #:zoglog)

(defparameter *log-to-stdout* t)

(let ((stdout *standard-output*))
  (defun log-fmt (&rest rest)
    "Log message like format function"
    (let* ((control-string (pop rest))
	   (cs (concatenate 'string control-string "~%")))
      (when *log-to-stdout*
	(apply #'format (append (list stdout) (list cs) rest))))))

