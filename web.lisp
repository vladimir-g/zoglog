(in-package #:zoglog)

;;; Templates

(djula:add-template-directory (asdf:system-relative-pathname "zoglog" "tpl/"))
(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +main.html+ (djula:compile-template* "main.html"))
(defparameter +channel.html+ (djula:compile-template* "channel.html"))

(setf hunchentoot:*rewrite-for-session-urls* nil)
(setf hunchentoot:*session-max-time* 86400)

(defun get-selected-tz (request)
  "Get selected timezone name from cookie or default."
  (let ((selected-tz (hunchentoot:cookie-in "zoglog-tz" request)))
    (if selected-tz
        (hunchentoot:url-decode selected-tz)
        *default-tz*)))

(defun convert-date (string tz-offset)
  "Convert STRING to local-time timestamp or nil."
  (local-time:parse-timestring string
                               :offset tz-offset
                               :fail-on-error nil))

(defun format-search-date (date timezone)
  "Format date for search input."
  (if date
      (local-time:format-timestring nil
				    date
				    :format +search-date-format+
				    :timezone timezone)
      ""))

;; Template filters

;; Very simple URL regex
(defparameter +url-regex+ (cl-ppcre:create-scanner
                           "\\b(((ftp|http)s?|file)://[^\\s]+)"))

(defun replace-with-link (target-string
                          start
                          end
                          match-start
                          match-end
                          reg-starts
                          reg-ends)
  "Replace link with html. TODO: url-encode href properly."
  (declare (ignore start end reg-starts reg-ends))
  (format nil "<a rel=\"nofollow\" href=\"~a\">~:*~a</a>"
          (subseq target-string
                  match-start
                  match-end)))

(djula::def-filter :linkify (it)
   (cl-ppcre:regex-replace-all +url-regex+
                               (hunchentoot:escape-for-html it)
                               #'replace-with-link))

;;; Handlers
(defun return-404 ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))

(hunchentoot:define-easy-handler (main :uri "/") ()
  "Main page, display list of channels with servers."
    (djula:render-template* +main.html+ nil
                            :channels (get-all-channels)
                            :timezones +timezone-names+
                            :current-url (hunchentoot:request-uri*)
                            :selected-tz (get-selected-tz
                                          hunchentoot:*request*)))

(hunchentoot:define-easy-handler (set-timezone :uri "/set-timezone/")
    (timezone return-path)
  "Save selected timezone to cookie and redirect user back."
  (when (find timezone +timezone-names+ :test #'equal)
    (hunchentoot:set-cookie "zoglog-tz"
			    :value (hunchentoot:url-encode timezone)
			    :path "/"
			    :max-age 31536000))
  (hunchentoot:redirect return-path))

(defun match-channel (request)
  "Check if url matches scheme /channel/:server-name/:channel-name/
and return these names."
  (multiple-value-bind (str match)
      (cl-ppcre:scan-to-strings "^/channel/([^/]+)/([^/]+)/$"
                                (hunchentoot:script-name* request))
    (declare (ignore str))
    match))

(defun redirect-to-date (&key request server channel date-to nick
                           host message skip-to)
  "Redirect user to page with to-id is nearest to DATE."
  (let* ((params (hunchentoot:get-parameters* request))
         (url (hunchentoot:script-name* request))
         (first (car (get-log-records :server server
                                      :channel channel
                                      :host host
                                      :nick nick
                                      :message message
                                      :date-from skip-to
                                      :date-to date-to
                                      :limit 1
                                      :sort 'asc)))
         (from-id (if first
                    (write-to-string (- (id first) 1)) ; lte, not lt
                    ""))
         (query (remove-if #'(lambda (i) (member (car i)
                                                '("to-id" "from-id")
                                                :test #'equal))
                           params))
         (redirect-to (create-url url
                                  (acons "from-id" from-id query))))
    (hunchentoot:redirect redirect-to)))

(defun show-in-context (&key request server channel id limit)
  "Redirect user to page with to-id is nearest to DATE."
  (let* ((from-id (get-context-start :server server
				     :channel channel
				     :event-id id
				     :size (floor limit 2)))
	 (query (if (/= limit *default-log-limit*)
		    (list (cons "limit" (write-to-string limit)))
		    '()))
         (url (hunchentoot:script-name* request)))
    (hunchentoot:redirect
     (format nil "~a#msg-~a"
	     (create-url url
			 (acons "from-id"
				(write-to-string from-id)
				query))
	     id))))

(defun get-pager-links (&key request to-id from-id messages limit)
  "Get links to next and previous pages"
  (when (/= (length messages) 0)
    (let* ((params (hunchentoot:get-parameters* request))
           (url (hunchentoot:script-name* request))
           (query (remove-if #'(lambda (i) (member (car i)
                                                   '("to-id" "from-id")
                                                   :test #'equal))
                             params))     ; Remove ids from query
           (has-next (= (length messages) (+ limit 1)))
           (first-id (write-to-string (id (first messages))))
           (last-id (if has-next
                        (write-to-string (id (elt messages (- limit 1))))))
           (newer-link)
           (older-link))
      (if from-id
          ;; Moving from older to newer
          (progn
            (when has-next
              (setf newer-link (create-url
                                url (acons "from-id" last-id query))))
            (setf older-link (create-url
                              url (acons "to-id" first-id query))))
          ;; Moving backwards, from newer to older
          (progn
            (when has-next
              (setf older-link (create-url
                                url (acons "to-id" last-id query))))
            (when to-id
              (setf newer-link (create-url
                                url (acons "from-id" first-id query))))))
      (values newer-link older-link))))

;; Maximum log entries on one page
(defvar *log-display-limit* 1000)

(defun nullable-str (string)
  (if (= (length string) 0)
      nil
      string))

(hunchentoot:define-easy-handler (channel-log :uri #'match-channel)
    ((date-from :parameter-type #'nullable-str)
     (date-to :parameter-type #'nullable-str)
     (nick :parameter-type #'nullable-str)
     (host :parameter-type #'nullable-str)
     (message :parameter-type #'nullable-str)
     (skip-to-date :parameter-type #'nullable-str)
     (from-id :parameter-type 'integer)
     (to-id :parameter-type 'integer)
     (show-in-context :parameter-type 'integer)
     (limit :parameter-type 'integer))
  "Display filtered channel log."
  (destructuring-bind (server channel)
      (map 'list #'(lambda (x) x) (match-channel hunchentoot:*request*))
    (progn
      ;; Show 404 if channel not found
      (unless (channel-exists-p server channel)
        (return-404)
        (return-from channel-log nil))
      (let* ((channel (format nil "#~a" channel))
             (tz (get-selected-tz hunchentoot:*request*))
             (tz-offset (get-offset tz))
             (lt-tz (local-time::%make-simple-timezone tz tz tz-offset)))
        ;; Validate filter parameters
        (if limit
            (when (> limit *log-display-limit*)
              (setf limit *log-display-limit*))
            (setf limit *default-log-limit*))
        (when date-from
          (setf date-from (convert-date date-from tz-offset)))
        (when date-to
          (setf date-to (convert-date date-to tz-offset)))
        (when nick
          (setf nick (string-trim " " nick)))

        ;; Process skip to date
        (when skip-to-date
          (setf skip-to-date (convert-date skip-to-date tz-offset))
          (if skip-to-date
              (return-from channel-log
                (redirect-to-date :request hunchentoot:*request*
                                  :server server
                                  :channel channel
                                  :host host
                                  :nick nick
                                  :message message
                                  :date-to date-to
                                  :skip-to skip-to-date))))

	;; Show comment in context without filters
        (when show-in-context
	  (return-from channel-log
	    (show-in-context :request hunchentoot:*request*
			     :server server
			     :channel channel
			     :id show-in-context
			     :limit limit)))

        (let* ((sort (if from-id 'asc 'desc))
               (messages (get-log-records
                          :server server
                          :channel channel
                          :host host
                          :nick nick
                          :message message
                          :date-from date-from
                          :date-to date-to
                          :to-id to-id
                          :from-id from-id
                          :limit (+ limit 1)
                          :sort sort))
               ;; Slice list to limit and format dates
               (messages-list (mapcar #'(lambda (e) (format-date e lt-tz) e)
                                      (slice-list messages 0 limit))))
          (multiple-value-bind (newer-link older-link)
              (get-pager-links :request hunchentoot:*request*
                               :from-id from-id
                               :to-id to-id
                               :messages messages
                               :limit limit)
            (if (eq sort 'desc)
                (setf messages-list (nreverse messages-list)))
            (djula:render-template* +channel.html+
                                    nil
                                    :messages messages-list
                                    :server server
                                    :channel channel
                                    :host host
                                    :nick nick
                                    :message message
                                    :date-from (format-search-date
						date-from
						lt-tz)
                                    :date-to (format-search-date
					      date-to
					      lt-tz)
                                    :limit limit
                                    :max-limit *log-display-limit*
                                    :default-limit *default-log-limit*
                                    :current-url (hunchentoot:request-uri*)
				    :newest-url (hunchentoot:script-name*)
                                    :timezones +timezone-names+
                                    :selected-tz tz
                                    :nicks (get-nicks :server server
                                                      :channel channel)
                                    :newer-link newer-link
                                    :older-link older-link)))))))

(defvar *acceptor* nil)

(defun start-web (&optional (port 4242))
  "Start logger web interface."
  (setf *acceptor* (make-instance
		    'hunchentoot:easy-acceptor
		    :port port
		    :document-root
		    (asdf:system-relative-pathname "zoglog" "www/")))
  (hunchentoot:start *acceptor*))
