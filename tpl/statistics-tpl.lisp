(in-package #:zoglog)

;; Template for main statistics page
(deftemplate statistics-tpl
  (center
   (:div :class "page-header"
         (:h1 "Statistics"))
   ((:div :class "list-group")
    (loop for channel in (getf args :channels)
       with current-chan
       do (progn
            (when (not (equal current-chan (server-id  channel)))
              (setf current-chan (server-id channel))
              (cl-who:htm (:a :class "list-group-item list-group-item-success"
                              (cl-who:str (server-id channel)))))
            (cl-who:htm (:a
                         :href (format nil "/statistics/~a/~a/"
                                       (hunchentoot:url-encode
                                        (server-id channel))
                                       (hunchentoot:url-encode
                                        (subseq (name channel) 1)))
                         :class "list-group-item"
                         (:strong :class "text-primary"
                                  (cl-who:str (name channel))))))))))
