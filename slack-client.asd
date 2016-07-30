#|
  This file is a part of slack-client project.
  Copyright (c) 2016 Kazuo Koga (obiwanko@me.com)
|#

#|
  Author: Kazuo Koga (obiwanko@me.com)
|#

(in-package :cl-user)
(defpackage slack-client-asd
  (:use :cl :asdf))
(in-package :slack-client-asd)

(defsystem slack-client
  :version "0.1"
  :author "Kazuo Koga"
  :license "Apache-2.0"
  :depends-on ("blackbird"
               "babel"
               "cl-async"
               "drakma-async"
               "jonathan"
               "safe-queue"
               "websocket-driver")
  :components ((:module "src"
                :components
                ((:file "slack-client"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op slack-client-test))))
