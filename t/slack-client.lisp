(in-package :cl-user)
(defpackage slack-client-test
  (:use :cl
        :prove))
(in-package :slack-client-test)

;; NOTE: To run this test file, execute `(asdf:test-system :slack-client)' in your Lisp.

(plan nil)

(finalize)
