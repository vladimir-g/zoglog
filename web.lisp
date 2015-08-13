(in-package #:zoglog)

;;; Templates

(djula:add-template-directory (asdf:system-relative-pathname "zoglog" "tpl/"))
(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +main.html+ (djula:compile-template* "main.html"))
(defparameter +channel.html+ (djula:compile-template* "channel.html"))

;;; Handlers

(defun return-404 ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))

(hunchentoot:define-easy-handler (main :uri "/") ()
  "Main page, display list of channels with servers."
  (djula:render-template* +main.html+ nil :channels (get-all-channels)))

(defun match-channel (request)
  "Check if url matches scheme /channel/:server-name/:channel-name/
and return these names."
  (multiple-value-bind (str match)
      (cl-ppcre:scan-to-strings "^/channel/([^/]+)/([^/]+)/$"
				(hunchentoot:script-name* request))
    (declare (ignore str))
    match))


(defun convert-date (string)
  "Convert STRING to local-time timestamp or nil."
  (local-time:parse-timestring string :fail-on-error nil))

(defun get-pager-links (&key server
                          channel
                          date-from
                          date-to nick
			  host
                          message-type
                          limit
                          messages)
  (let ((next-link (= (length messages) (+ limit 1)))
        (prev-link nil))
    (values next-link prev-link)))

;; Maximum log entries on one page
(defvar *log-display-limit* 1000)

(hunchentoot:define-easy-handler (channel-log :uri #'match-channel
					      :default-request-type :get)
    (date-from date-to nick host message
	       (limit :parameter-type 'integer))
  "Display filtered channel log."
  (destructuring-bind (server channel)
      (map 'list #'(lambda (x) x) (match-channel hunchentoot:*request*))
    (progn
      ;; Show 404 if channel not found
      (unless (channel-exists-p server channel)
        (return-404)
        (return-from channel-log nil))
      (setf channel (format nil "#~a" channel))

      ;; Validate filter parameters
      (if limit
          (when (> limit *log-display-limit*)
            (setf limit *log-display-limit*))
          (setf limit *default-log-limit*))
      (when date-from
	(setf date-from (convert-date date-from)))
      (when date-to
	(setf date-to (convert-date date-to)))

      (let ((messages (get-log-records
                       :server server
                       :channel channel
                       :host host
                       :nick nick
                       :message message
                       :date-from date-from
                       :date-to date-to
                       :limit (+ limit 1))))
        (multiple-value-bind (next-link prev-link)
            (get-pager-links :server server :channel channel
                             :host host :nick nick
                             :message message :date-from date-from
                             :date-to date-to :limit limit
                             :messages messages))
          (djula:render-template* +channel.html+
                                  nil
                                  :messages (slice-list
                                             (nreverse messages) 0 limit)
                                  :server server
                                  :channel channel
                                  :next-link next-link
                                  :prev-link prev-link))))))

(defun start-web ()
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor
		  :port 4242
		  :document-root
		  (asdf:system-relative-pathname "zoglog" "www/"))))
