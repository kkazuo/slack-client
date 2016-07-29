(in-package :cl-user)
(defpackage slack-client-test
  (:use :cl
        :slack-client
        :prove))
(in-package :slack-client-test)

;; NOTE: To run this test file, execute `(asdf:test-system :slack-client)' in your Lisp.

(plan nil)

(defparameter *c* (make-instance 'slack-client))
(run-client *c*)

(finalize)
