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
	       #:hunchentoot)
  :components ((:file "package")
               (:file "utils")
               (:file "logging")
               (:file "data")
               (:file "database")
               (:file "parser")
	       (:file "web")
	       (:file "zoglog")))

