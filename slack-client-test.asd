#|
  This file is a part of slack-client project.
  Copyright (c) 2016 Kazuo Koga (obiwanko@me.com)
|#

(in-package :cl-user)
(defpackage slack-client-test-asd
  (:use :cl :asdf))
(in-package :slack-client-test-asd)

(defsystem slack-client-test
  :author "Kazuo Koga"
  :license "Apache-2.0"
  :depends-on (:slack-client
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "slack-client"))))
  :description "Test system for slack-client"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
