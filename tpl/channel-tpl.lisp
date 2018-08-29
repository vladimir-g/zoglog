(in-package #:zoglog)

(defun pagination-tpl (args)
  "Template part for paginator block."
  (with-args (oldest-link
              older-link
              newer-link
              newest-link
              from-id
              to-id
              nick
              host
              message
              limit
              date-from
              date-to)
      args
    (let ((date-from (format-search-date date-from (getf args :timezone)))
          (date-to (format-search-date date-to (getf args :timezone))))
      (cl-who:with-html-output-to-string (s nil :indent t)
        ((:nav :class "text-center paginator")
         ;; Previous
         (when (and oldest-link older-link (or (null from-id) (/= from-id 0)))
           (cl-who:htm
            (:a :class "btn btn-sm btn-default"
                :title "Oldest"
                :href oldest-link
                (:span :class "glyphicon glyphicon-fast-backward"))))
         (when older-link
           (cl-who:htm
            (:a :class "btn btn-sm btn-default"
                :title "Older"
                :href older-link
                (:span :class "glyphicon glyphicon-arrow-left"))))
         ;; Skip to date form
         ((:form :class "form-inline skip-to-date-form"
                 :method "post"
                 :action "?")
          (:div :class "form-group"
                (:label :class "sr-only" "Skip to")
                (:div :class "input-group input-group-sm"
                      (:input :type "text"
                              :class "form-control datepicker skip-to-date"
                              :data-min date-from
                              :data-max date-to
                              :autocomplete "off"
                              :name "skip-to-date"
                              :placeholder "Skip to date"
                              :value "")
                      (:span
                       :class "input-group-btn"
                       (:button
                        :class "btn btn-default"
                        :title "Skip to date"
                        :type"submit"
                        (:span :class "glyphicon glyphicon-new-window")))))
          (when nick
            (cl-who:htm
             (:input :type "hidden" :name "nick"
                     :value (cl-who:escape-string nick))))
          (when host
            (cl-who:htm
             (:input :type "hidden" :name "host"
                     :value (cl-who:escape-string host))))
          (when message
            (cl-who:htm
             (:input :type "hidden" :name "message"
                     :value (cl-who:escape-string message))))
          (when date-from
            (cl-who:htm
             (:input :type "hidden" :name "date-from"
                     :value (cl-who:escape-string date-from))))
          (when date-to
            (cl-who:htm
             (:input :type "hidden" :name "date-to"
                     :value (cl-who:escape-string date-to))))
          (when (and limit (/= limit *default-log-limit*))
            (cl-who:htm
             (:input :type "hidden" :name "limit"
                     :value (cl-who:escape-string (write-to-string limit))))))

         ;; Next
         (when newer-link
           (cl-who:htm
            (:a :class "btn btn-sm btn-default"
                :title "Newer"
                :href newer-link
                (:span :class "glyphicon glyphicon-arrow-right"))))
         (when (and (or from-id to-id) newest-link)
           (cl-who:htm
            (:a :class "btn btn-sm btn-default"
                :title "Newest"
                :href newest-link
                (:span :class "glyphicon glyphicon-fast-forward")))))))))

(defun filter-tpl (args)
  "Template part for filter block."
  (with-args (channel
              server
              nick
              host
              message
              date-from
              date-to
              limit
              newest-url)
      args
    (let ((date-from (format-search-date date-from (getf args :timezone)))
          (date-to (format-search-date date-to (getf args :timezone))))
      (cl-who:with-html-output-to-string (s nil :indent t)
        ((:div :class "well well-sm")

         ((:form :id "filter-form"
                 :class "form-inline"
                 :method "get"
                 :action "?")

          (:div :class "form-group"
                (:label :class "sr-only" :for "nick" "Nick:")
                ((:div :class "input-group")
                 (:div
                  :class "input-group-addon"
                  (:span :id "nick-icon" :class "glyphicon glyphicon-user"))
                 (:input :type "text"
                         :class "form-control"
                         :id "nick"
                         :name "nick"
                         :data-channel channel
                         :data-server server
                         :placeholder "Nick"
                         :value (when nick
                                  (cl-who:escape-string nick)))))

          (:div :class "form-group"
                (:label :class "sr-only" :for "host" "Host:")
                (:input :type "text"
                        :class "form-control"
                        :id "host"
                        :name "host"
                        :placeholder "user@hostname"
                        :value (when host
                                 (cl-who:escape-string host))))

          (:div :class "form-group"
                (:label :class "sr-only" :for "message" "Message:")
                (:input :type "text"
                        :class "form-control"
                        :id "message"
                        :name "message"
                        :placeholder "Message"
                        :value (when message
                                 (cl-who:escape-string message))))

          (:div :class "form-group"
                (:label :class "sr-only" :for "date-from" "From date:")
                ((:div :class "input-group")
                 (:div
                  :class "input-group-addon"
                  (:span :class "glyphicon glyphicon-calendar"))
                 (:input :type "text"
                         :class "form-control datepicker"
                         :id "date-from"
                         :name "date-from"
                         :placeholder "From: 1991-12-26T01:01:01"
                         :value (when date-from
                                  (cl-who:escape-string date-from)))))

          (:div :class "form-group"
                (:label :class "sr-only" :for "date-to" "To date:")
                ((:div :class "input-group")
                 (:input :type "text"
                         :class "form-control datepicker"
                         :id "date-to"
                         :name "date-to"
                         :placeholder "To: 2037-06-06T06:06:06"
                         :value (when date-to
                                  (cl-who:escape-string date-to)))
                 (:div
                  :class "input-group-addon"
                  (:span :class "glyphicon glyphicon-calendar"))))

          (:div :class "form-group"
                (:label :class "sr-only" :for "limit" "Limit:")
                (:input :type "number"
                        :class "form-control"
                        :id "limit"
                        :name "limit"
                        :min "1"
                        :max *log-display-limit*
                        :placeholder "Limit"
                        :value (when (and limit
                                          (/= limit *default-log-limit*))
                                 limit)))

          (:div :class "btn-group btn-group-sm"
                :role "group"
                :aria-label "filter"
                (:button :type "submit" :class "btn btn-primary"
                         (:span :class "glyphicon glyphicon-search"))
                (:a :class "btn btn-danger" :href newest-url
                    (:span :class "glyphicon glyphicon-remove")))))))))

;; Channel page template
(deftemplate channel-tpl
  (title
   (cl-who:fmt "ZOGLOG - ~a/~a" (getf args :channel) (getf args :server)))
  (center
   ((:div :class "page-header")
    (:h1 (cl-who:str (getf args :channel))
         (:small (cl-who:str (getf args :server))
                 (:a
                  :href (format
                         nil
                         "/statistics/~a/~a/"
                         (hunchentoot:url-encode (getf args :server))
                         (hunchentoot:url-encode (subseq (getf args :channel)
                                                         1)))
                  :title "Channel statistics"
                  (:span :class "glyphicon glyphicon-stats"))))
    (cl-who:str (filter-tpl args))
    (cl-who:str (pagination-tpl args))
    ((:table
      :id "messages-table"
      :class (format nil
                     "table table-striped table-hover table-condensed ~a"
                     (get-selected-font-family hunchentoot:*request*)))
     (:thead
      (:tr
       (:th :class "col-lg-1" "Date")
       (:th :class "col-lg-1 col-md-1 col-sm-1 text-right nick-col" "Nick")
       (:th "Message")))
     (:tbody
      (loop for msg in (getf args :messages)
         do (cl-who:htm
             ((:tr :id (format nil "msg-~a" (id msg))
                   :class
                   (cond ((equal (message-type msg) "NOTICE") "info")
                         ((equal (message-type msg) "ACTION") "warning")
                         ((equal (message-type msg) "KICK") "danger")))
              ;; Message date
              (:td :class "date-cell"
                   (:a :rel "nofollow"
                       :title "Show in unfiltered context"
                       :href (format nil
                                     "?show-in-context=~a&amp;limit=~a"
                                     (id msg)
                                     (getf args :limit))
                       (:small (cl-who:str
                                (format-date (date msg)
                                             (getf args :timezone)))))) ;FIXME
              ;; Nick
              (:td :class "text-right nick-cell"
                   (when (member (message-type msg)
                                 '("PRIVMSG" "NOTICE")
                                 :test #'equal)
                     (cl-who:htm
                      (:strong :title (host msg)
                               :style (format
                                       nil "color: ~a;"
                                       (get-nick-color-hsl (nick msg)))
                               (cl-who:esc (nick msg))))))
              ;; Message contents
              (:td :class "log-message"
                   (if (member (message-type msg)
                               '("PRIVMSG" "NOTICE" "ACTION")
                               :test #'equal)

                       (cl-who:htm
                        (:strong
                         :class (when (not (equal (message-type msg) "ACTION"))
                                  "message-nick")
                         :title (when (not (equal (message-type msg) "ACTION"))
                                  (host msg))
                         :style (format
                                 nil "color: ~a;"
                                 (get-nick-color-hsl (nick msg)))
                         (cl-who:esc (nick msg))
                         (when (not (equal (message-type msg) "ACTION"))
                           (cl-who:htm :br))
                         " ")
                        (cl-who:str (linkify (message msg))))

                       (cl-who:htm
                        (:span
                         :class "text-muted"
                         (cl-who:esc (nick msg))
                         (cl-who:esc (format nil " (~a)" (host msg)))
                         (cond ((equal (message-type msg) "JOIN")
                                (cl-who:str " has joined"))
                               ((equal (message-type msg) "QUIT")
                                (cl-who:htm
                                 " has quit"
                                 (when (message msg)
                                   (cl-who:esc
                                    (format nil ": ~a" (message msg))))))
                               ((equal (message-type msg) "PART")
                                (cl-who:htm
                                 " has left channel"
                                 (when (message msg)
                                   (cl-who:esc
                                    (format nil ": ~a" (message msg))))))
                               ((equal (message-type msg) "KICK")
                                (cl-who:htm
                                 " kicked " (cl-who:esc (message msg))))
                               ((equal (message-type msg) "NICK")
                                (cl-who:htm
                                 " is now known as"
                                 (:strong
                                  (cl-who:esc (message msg)))))))))))))))
    (cl-who:str (pagination-tpl args)))))
