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
                 #:djula
                 #:vom
                 #:cl-ppcre
                 #:flexi-streams)
    :components ((:file "package")
                 (:file "utils")
                 (:file "database")
                 (:file "irc-messages")
                 (:file "irc")
                 (:file "web")
                 (:file "zoglog"))
    :in-order-to ((asdf:test-op (asdf:test-op :zoglog-test))))
