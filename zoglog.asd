;;; zoglog.asd

(asdf:defsystem #:zoglog
    :description "Simple IRC logger with web interface"
    :author "Vladimir Gorbunov <vsg@suburban.me>"
    :license "MIT"
    :serial t
    :depends-on (#:usocket
                 #:split-sequence
                 #:postmodern
                 #:local-time
                 #:hunchentoot
                 #:bordeaux-threads
                 #:cl-who
                 #:cl+ssl
                 #:vom
                 #:cl-ppcre
                 #:flexi-streams
                 #:cl-base64
                 #:babel)
    :components ((:file "package")
                 (:file "utils")
                 (:file "database")
                 (:file "statistics")
                 (:file "irc-messages")
                 (:file "irc")
                 (:module "tpl"
                          :serial t
                          :components ((:file "templates")
                                       (:file "base-tpl")
                                       (:file "main-tpl")
                                       (:file "statistics-tpl")
                                       (:file "stat-channel-tpl")
                                       (:file "channel-tpl")))
                 (:file "web")
                 (:file "zoglog"))
    :in-order-to ((asdf:test-op (asdf:test-op :zoglog-test))))
