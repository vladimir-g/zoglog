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

(defun generate-pager-link (url query &optional additional)
  "Generate link to page."
  (let ((q (if additional (cons additional query) query)))
    (reduce #'(lambda (url p)
                (url-rewrite:add-get-param-to-url url (car p) (cdr p)))
            q :initial-value url)))

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
              (setf newer-link (generate-pager-link
                                url query (cons "from-id" last-id))))
            (setf older-link (generate-pager-link
                              url query (cons "to-id" first-id))))
          ;; Moving backwards, from newer to older
          (progn
            (when has-next
              (setf older-link (generate-pager-link
                                url query (cons "to-id" last-id))))
            (when to-id
              (setf newer-link (generate-pager-link
                                url query (cons "from-id" first-id))))))
      (values newer-link older-link))))

;; Maximum log entries on one page
(defvar *log-display-limit* 1000)

(defun nullable-str (string)
  (if (= (length string) 0)
      nil
      string))

(hunchentoot:define-easy-handler (channel-log :uri #'match-channel
                                              :default-request-type :get)
    ((date-from :parameter-type #'nullable-str)
     (date-to :parameter-type #'nullable-str)
     (nick :parameter-type #'nullable-str)
     (host :parameter-type #'nullable-str)
     (message :parameter-type #'nullable-str)
     (from-id :parameter-type 'integer)
     (to-id :parameter-type 'integer)
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
             (messages-list (slice-list messages 0 limit)))
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
                                  :date-from date-from
                                  :date-to date-to
                                  :limit limit
                                  :newer-link newer-link
                                  :older-link older-link))))))

(defvar *acceptor*)

(defun start-web (&optional (port 4242))
  "Start logger web interface."
  (setf *acceptor* (make-instance
		    'hunchentoot:easy-acceptor
		    :port port
		    :document-root
		    (asdf:system-relative-pathname "zoglog" "www/")))
  (hunchentoot:start *acceptor*))

(let ((testparams '(("a" . "1") ("b" . "3") ("c" . "sndk7&"))))
  (reduce #'(lambda (url params)
              (url-rewrite:add-get-param-to-url url (car params) (car params)))
          testparams
          :initial-value "http://test.ru"))
