(in-package #:zoglog)

(defun return-404 ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))

(hunchentoot:define-easy-handler (main :uri "/") ()
  "Main page, display list of channels with servers."
  (setf (hunchentoot:content-type*) "text/plain")
  (with-output-to-string (out)
    (let ((server))
      (dolist (channel (get-all-channels))
	(when (string/= server (server-id channel))
	  (format out "~a:~%" (server-id channel))
	  (setf server (server-id channel)))
	(format out "~c~a~%" #\tab (name channel))))))

(defun match-channel (request)
  "Check if url matches scheme /channel/:server-name/:channel-name/
and return these names."
  (multiple-value-bind (str match)
      (cl-ppcre:scan-to-strings "^/channel/([^/]+)/([^/]+)/$"
				(hunchentoot:request-uri* request))
    (declare (ignore str))
    match))

(hunchentoot:define-easy-handler (channel :uri #'match-channel) ()
  "Display channel log."
  (let ((matches (match-channel hunchentoot:*request*)))
    (unless (channel-exists-p (elt matches 0) (elt matches 1))
      (return-404)
      (return-from channel nil))
    (format nil "Found!")))
