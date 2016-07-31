(in-package :cl-user)
(defpackage slack-client-test
  (:use :cl
        :slack-client
        :prove))
(in-package :slack-client-test)

;; NOTE: To run this test file, execute `(asdf:test-system :slack-client)' in your Lisp.

(plan nil)

(defparameter *c* (make-instance 'slack-client))
(ev:bind :*
  (lambda (ev)
    (format t "~A~%" ev))
  :on *c*)
(ev:bind "message"
  (lambda (ev)
    (let* ((data (ev:data ev))
           (channel (afind data "channel"))
           (text (afind data "text")))
      (format t "~A: ~A~%" channel text)
      (when (equal text "hello")
        (send-text *c* "world"
                   :channel-id channel))))
  :on *c*)
(run-client *c*)

(finalize)
