(in-package #:zoglog)

(defun base-tpl (blocks &optional args)
  "Base site template."
  (cl-who:with-html-output-to-string (s nil :indent t :prologue t)
    ((:html :lang "en")
     (:head
      (:meta :charset "utf-8")
      (:meta :http-equiv "X-UA-Compatible" :content "IE=edge")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:title (cl-who:str (getf blocks :title "ZOGLOG")))
      (:link :href "/css/bootstrap.min.css" :rel "stylesheet")
      (:link :href "/css/rome.min.css" :rel "stylesheet")
      (:link :href "/css/main.css" :rel "stylesheet"))
     (:body

      ((:nav :class "navbar navbar-default navbar-static-top")
       ((:div :class "container-fluid")
        (:div :class "navbar-header"
              (:a :href "/" :class "navbar-brand" "ZOGLOG"))
        (:ul :class "nav navbar-nav"
             (:li :class (when (equal (getf args :active-menu-item)
                                      "statistics")
                           "active")
                  (:a :href "/statistics/" "Statistics")))

        (:form :class "navbar-form navbar-right"
               :action "/set-timezone/"
               :autocomplete "off"
               (:input :type "hidden"
                       :name "return-path"
                       :value (hunchentoot:request-uri*))
               ((:div :class "input-group")
                ((:span :class "input-group-addon" :title "timezone")
                 (:span :class "glyphicon glyphicon-time"))
                (:select
                 :class "form-control"
                 :name "timezone"
                 (loop for zone in +timezone-names+
                    do (cl-who:htm (:option
                                    :value zone
                                    :selected (equal zone
                                                     (get-selected-tz
                                                      hunchentoot:*request*))
                                    (cl-who:str zone)))))
                (:span :class "input-group-btn"
                       (:button :class "btn btn-default"
                                :type "submit"
                                :title "Set timezone"
                                (:span :class "glyphicon glyphicon-ok")))))))

      (:div :class "container-fluid"
            (cl-who:str (getf blocks :center)))

      (:script :src "/js/rome.min.js")
      (:script :src "/js/main.js")))))
