(in-package #:zoglog)

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
      nil))

;;; Handlers
(defun return-404 ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))

(hunchentoot:define-easy-handler (main :uri "/") ()
  "Main page, display list of channels with servers."
  (render-template #'main-tpl (list :channels (get-all-channels))))

(hunchentoot:define-easy-handler (set-timezone :uri "/set-timezone/")
    (timezone return-path)
  "Save selected timezone to cookie and redirect user back."
  (when (find timezone +timezone-names+ :test #'equal)
    (hunchentoot:set-cookie "zoglog-tz"
                            :value (hunchentoot:url-encode timezone)
                            :path "/"
                            :max-age 31536000))
  (hunchentoot:redirect return-path))

(hunchentoot:define-easy-handler (channel-nicks :uri "/nicknames/")
    (server channel)
  "Get list of all nicknames for channel."
  (unless (channel-exists-p server channel)
    (return-404)
    (return-from channel-nicks nil))
  (format nil "~{~a~^~%~}"
          (get-nicks :server server
                     :channel (format nil "#~a" channel))))

(defun redirect-to-date (&key request server channel date-to date-from
                           nick host message skip-to lt-tz limit)
  "Redirect to message list starting with first message after DATE."
  (let* ((query (list (cons "host" host)
                      (cons "date-to" (format-search-date
                                       date-to
                                       lt-tz))
                      (cons "date-from" (format-search-date
                                         date-from
                                         lt-tz))
                      (cons "nick" nick)
                      (cons "limit" (if limit (write-to-string limit)))
                      (cons "message" message)))
         (url (hunchentoot:script-name* request))
         (first-id (get-id-before-date :server server
                                       :channel channel
                                       :host host
                                       :nick nick
                                       :message message
                                       :date-from date-from
                                       :date-to skip-to))
         (from-id (if first-id
                      (write-to-string first-id)
                      "0"))
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

(defun nullable-str (string)
  (if (= (length string) 0)
      nil
      string))

;;; Web interface uses range-based pagination, page contains LIMIT
;;; count of comments starting from FROM-ID or lesser to TO-ID. So,
;;; next page is defined as FROM-ID=ID-OF-LAST-VISIBLE-COMMENT and
;;; previous page is TO-ID=ID-OF-FIRST-VISIBLE-COMMENT.
;;;
;;; This method helps avoid usage of OFFSET in SQL query and helps to
;;; maintain same query performance on every page. There are two problems:
;;; - You can't skip to some specific page.
;;; - It's not easy to find if next and previous pages are available.
;;;
;;; First problem can't be really avoided, so app contains some
;;; alternative navigation methods (skip to date). Solution for second
;;; problem is to get more than LIMIT and check if first and last
;;; messages exist, then show only messages in the "middle" of
;;; results, basically stripping first and last messages (except when
;;; there are less results than LIMIT+2). When querying with TO-ID
;;; message list is reversed because SQL query has different sort order.
;;;
;;; There are two cases (exaple is asc order):
;;;
;;; FROM-ID/TO-ID is equal to first message id, so previous page exist
;;; first-id == 0.id && length > 1
;;; [0 1 2 3 4 5 6 7], 8 messages, limit 6
;;; displaying:
;;;    1 2 3 4 5 6
;;; next left-id = 6.id if length >= limit + 2
;;;      where 6.id == messages[length - 2].id
;;; prev right-id=1.id
;;;
;;; FROM-ID/TO-ID is not equal to first message id, so previous
;;; page doesn't exist
;;; first-id != 0.id || length == 1
;;; [0 1 2 3 4 5 6 7], 8 messages, limit 6
;;; displaying
;;;  0 1 2 3 4 5
;;; next left-id = 5.id if length >= limit + 1
;;;      where 5.id == messages[limit - 1].id
;;; prev is NIL
;;;
;;; When order is ASC, from-id = right-id (next page) and to-id =
;;; left-id (prev page). If order is DESC (TO-ID is not NIL or not
;;; TO-ID and not FROM-ID) message list is reversed and ids are
;;; swapped.

(defun prepare-message-list (&key messages sort from-id to-id limit)
  "Get older and newer ids for pagination and slice messages to limit."
  (let ((first-id (if (eq sort 'desc) to-id from-id))
        (length (length messages))
        (left-id nil)
        (right-id nil)
        (older-id nil)
        (newer-id nil)
        (messages-list))
    (when (> length 1)
      (unless first-id
        (setf first-id (id (first messages))))
      (cond ((and (or from-id to-id)
                  (= first-id (id (first messages)))
                  (> length 1))
             ;; Have previous messages and more than one message
             (setf messages-list (slice-list messages 1 (1+ limit)))
             (setf left-id (id (elt messages 1)))
             ;; Have next messages
             (when (>= length (+ 2 limit))
               (setf right-id (id (elt messages (- length 2))))))
            ;; Have no previous messages, only one message or list of
            ;; newest messages
            (t
             (setf messages-list (slice-list messages 0 limit))
             (when (>= length (+ 1 limit))
               (setf right-id (id (car (last messages-list)))))))
      (if (eq sort 'asc)
          ;; Have from-id and ascending order
          (progn
            (setf older-id left-id)
            (setf newer-id right-id))
          ;; Have to-id and descending order
          (progn
            (setf older-id right-id)
            (setf newer-id left-id)
            (setf messages-list (nreverse messages-list)))))
    (values messages-list newer-id older-id)))

(defun paginate-messages (&key request from-id to-id messages limit sort)
  "Slice messages list to limit and create pagination links."
  (multiple-value-bind (messages-list newer-id older-id)
      (prepare-message-list :messages messages
                            :sort sort
                            :limit limit
                            :from-id from-id
                            :to-id to-id)
    (multiple-value-bind (newer-link older-link newest-link oldest-link)
        (get-pager-links request older-id newer-id)
      (values messages-list newer-link older-link newest-link oldest-link))))

(defun get-pager-links (request older-id newer-id)
  "Get links to next and previous pages"
  (let* ((params (hunchentoot:get-parameters* request))
         (url (hunchentoot:script-name* request))
         (query (remove-if #'(lambda (i) (member (car i)
                                                 '("to-id" "from-id")
                                                 :test #'equal))
                           params))     ; Remove ids from query
         (newest-link (create-url url query))
         (oldest-link (create-url url (acons "from-id" "0" query)))
         (newer-link)
         (older-link))
    (when older-id
      (setf older-link
            (create-url url (acons "to-id"
                                   (write-to-string older-id)
                                   query))))
    (when newer-id
      (setf newer-link
            (create-url url (acons "from-id"
                                   (write-to-string newer-id)
                                   query))))
    (values newer-link older-link newest-link oldest-link)))

(defmacro match-channel-regex (scanner request)
  "Match url with two values with precompiled regex scanner"
  (let ((str (gensym))
        (match (gensym)))
    `(multiple-value-bind (,str ,match)
         (cl-ppcre:scan-to-strings ,scanner
                                   (hunchentoot:script-name* ,request))
       (declare (ignore ,str))
       ,match)))

;;; Channel page

;;; URL match regex
(defparameter +chan-regex+ (cl-ppcre:create-scanner
                            "^/channel/([^/]+)/([^/]+)/$"))

(defun match-channel-page (request)
  (match-channel-regex +chan-regex+ request))

(hunchentoot:define-easy-handler (channel-log :uri #'match-channel-page)
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
      (map 'list #'(lambda (x) x) (match-channel-page hunchentoot:*request*))
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
                                  :date-from date-from
                                  :lt-tz lt-tz
                                  :limit (if (/= limit *default-log-limit*)
                                             limit
                                             nil)
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
                          :limit (+ limit 2)
                          :sort sort)))
          (multiple-value-bind (messages-list
                                newer-link
                                older-link
                                newest-link
                                oldest-link)
              (paginate-messages :request hunchentoot:*request*
                                 :from-id from-id
                                 :to-id to-id
                                 :messages messages
                                 :sort sort
                                 :limit limit)
              (render-template #'channel-tpl
                               (list :messages messages-list
                                     :server server
                                     :channel channel
                                     :host host
                                     :nick nick
                                     :message message
                                     :date-from date-from
                                     :date-to date-to
                                     :timezone lt-tz
                                     :limit limit
                                     :newest-url (hunchentoot:script-name*)
                                     :newest-link newest-link
                                     :oldest-link oldest-link
                                     :to-id to-id
                                     :from-id from-id
                                     :newer-link newer-link
                                     :older-link older-link))))))))


;; Statistics
(hunchentoot:define-easy-handler (statistics :uri "/statistics/") ()
  "Show main statistics page with channel list."
  (render-template #'statistics-tpl
                   (list :channels (get-all-channels)
                         :active-menu-item "statistics")))

;;; Channel statistics page
(defparameter +stat-regex+ (cl-ppcre:create-scanner
                            "^/statistics/([^/]+)/([^/]+)/$"))

(defun match-stat-page (request)
  (match-channel-regex +stat-regex+ request))

(defun prepare-message-stats (stats)
  "Process message stats data."
  (let ((count (loop for i in stats sum (cadr i)))
        (active-count 0)
        (all-count 0))
    (list
     :users (loop for item in stats
               collect (list
                        :nick (car item)
                        :messages (cadr item)
                        :share (if (plusp count)
                                   (float (* 100 (/ (cadr item) count)))
                                   0))
               when (> (cadr item) 0)
                 do (incf active-count)
               do (incf all-count))
     :count count
     :active-count active-count
     :all-count all-count)))

(hunchentoot:define-easy-handler (channel-stat :uri #'match-stat-page) ()
  "Display channel statistics page."
  (destructuring-bind (server channel)
      (map 'list #'(lambda (x) x) (match-stat-page hunchentoot:*request*))
    (progn
      (unless (channel-exists-p server channel)
        (return-404)
        (return-from channel-stat nil))
      (let* ((channel (format nil "#~a" channel))
             (message-stats (prepare-message-stats
                             (get-message-stats :server server
                                                :channel channel))))
        (render-template #'stat-channel-tpl
                         (list :server server
                               :channel channel
                               :message-stats message-stats
                               :active-menu-item "statistics"))))))

;;; Startup code

(defvar *acceptor* nil)

(defun start-web (&optional (port 4242))
  "Start logger web interface."
  (handler-case
      (let ((acceptor (make-instance
                       'hunchentoot:easy-acceptor
                       :port port
                       :document-root
                       (asdf:system-relative-pathname "zoglog" "www/"))))
        (hunchentoot:start acceptor)
        (setf *acceptor* acceptor))
    (error (c) (progn
                 (vom:error "Can't start web interface: ~a" c)
                 (setf *acceptor* nil)))))

(defun stop-web ()
  "Stop web interface."
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))
