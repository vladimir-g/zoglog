;;;; zoglog.asd

(asdf:defsystem #:zoglog
  :description "Describe zoglog here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:usocket #:split-sequence)
  :components ((:file "package")
               (:file "zoglog")))

