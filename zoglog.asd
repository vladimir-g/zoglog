;;; zoglog.asd

(asdf:defsystem #:zoglog
    :description "Describe zoglog here"
    :author "Your Name <your.name@example.com>"
    :license "Specify license here"
    :serial t
    :depends-on (#:usocket
                 #:split-sequence
                 #:postmodern
                 #:local-time
                 #:hunchentoot
                 #:bordeaux-threads
                 #:djula
                 #:vom
                 #:cl-ppcre
                 #:flexi-streams)
    :components ((:file "package")
                 (:file "utils")
                 (:file "database")
                 (:file "data")
                 (:file "irc")
                 (:file "web")
                 (:file "zoglog")))
