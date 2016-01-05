(in-package #:zoglog)

;; Main page template
(deftemplate main-tpl
  (center
   (:div :class "page-header"
         (:h1 "Channels"))
   ((:div :class "list-group")
    (loop for channel in (getf args :channels)
       with current-chan
       do (progn
            (when (not (equal current-chan (server-id  channel)))
              (setf current-chan (server-id channel))
              (cl-who:htm (:a :class "list-group-item list-group-item-info"
                              (cl-who:str (server-id channel)))))
            (cl-who:htm (:a
                         :href (format nil "/channel/~a/~a/"
                                       (hunchentoot:url-encode
                                        (server-id channel))
                                       (hunchentoot:url-encode
                                        (name channel)))
                         :class "list-group-item"
                         (:strong :class "text-primary"
                                  (cl-who:fmt "#~a" (name channel))))))))))
