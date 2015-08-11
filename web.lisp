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

(hunchentoot:define-easy-handler (channel-log :uri #'match-channel) ()
  "Display channel log."
  (destructuring-bind (server channel)
      (map 'list #'(lambda (x) x) (match-channel hunchentoot:*request*))
    (progn
      ;; Show 404 if channel not found
      (unless (channel-exists-p server channel)
        (return-404)
        (return-from channel-log nil))
      (setf (hunchentoot:content-type*) "text/plain")
      (setf channel (format nil "#~a" channel))
      ;; Get log records
      (with-output-to-string (out)
        (dolist (msg (get-log-records :server server :channel channel :limit 10))
          (format out "~a: ~a~%" (date msg) (message msg)))))))
