;;; zoglog-test.asd

(asdf:defsystem #:zoglog-test
    :description "Test for zoglog IRC logger"
    :author "Vladimir Gorbunov <vsg@suburban.me>"
    :license "MIT"
    :depends-on (#:zoglog
                 #:fiveam)
    :components ((:file "zoglog-test"))
    :perform (asdf:test-op (o s)
                           (uiop:symbol-call :zoglog-test :run-tests)))
