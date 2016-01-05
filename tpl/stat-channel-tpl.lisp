(in-package #:zoglog)

;; Template for channel statistics page
(deftemplate stat-channel-tpl
  (title
   (cl-who:fmt "ZOGLOG - statistics for ~a/~a"
               (getf args :channel)
               (getf args :server)))
  (center
   ((:div :class "page-header")
    (:h1 "Statistics: " (cl-who:str (getf args :channel))
         (:small (cl-who:str (getf args :server))
                 (:a
                  :href (format
                         nil
                         "/channel/~a/~a/"
                         (hunchentoot:url-encode (getf args :server))
                         (hunchentoot:url-encode (subseq (getf args :channel)
                                                         1)))
                  :title "Channel log"
                  (:span :class "glyphicon glyphicon-list-alt")))))

   ((:div :class "row")
    ((:div :class "col-md-6")

     (let ((stats (getf args :message-stats)))
       (cl-who:htm
        ((:div :class "panel panel-default")
         (:div :class "panel-heading"
               (:strong "Messages"))
         (:div :class "panel-body"
               (:dl :class "dl-horizontal"
                    (cl-who:htm
                     (:dt "Messages count:")
                     (:dd (cl-who:str (getf stats :count)))
                     (:dt "Unique nicknames:")
                     (:dd (cl-who:str (getf stats :all-count)))
                     (:dt "Active users:")
                     (:dd (cl-who:str (getf stats :active-count)))))))

        ((:table :class "table table-striped table-hover table-condensed")
         (:thead
          (:tr
           (:th "#")
           (:th "Nick")
           (:th "Count")
           (:th "%")))
         (:tbody
          (loop for user in (getf stats :users)
             count user into counter
             do (cl-who:htm
                 (:tr
                  (:td (cl-who:str counter))
                  (:td :class "stat-nick"
                       (:strong
                        :style (format nil "color: ~a;"
                                       (get-nick-color-hsl
                                        (getf user :nick)))
                        (cl-who:esc (getf user :nick))))
                  (:td (cl-who:str (getf user :messages)))
                  (:td (cl-who:fmt "~$" (getf user :share))))))))))))))
