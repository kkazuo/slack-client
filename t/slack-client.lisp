(in-package :cl-user)
(defpackage slack-client-test
  (:use :cl
        :prove))
(in-package :slack-client-test)

;; NOTE: To run this test file, execute `(asdf:test-system :slack-client)' in your Lisp.

(plan nil)

(defparameter *c* (make-instance 'sc:slack-client))
(sc:bind :* *c*
  (lambda (ev)
    (format t "~A: ~A~%" (ev:ev ev) ev)))
(sc:bind "message" *c*
  (lambda (ev)
    (sc:with-data-let ev (channel user text)
      (format t "~A:~A: ~A~%" channel user text)
      (when (equal text "hello")
        (sc:send-text *c* channel "world"))
      (when (equal text "now")
        (sc:send-text *c* channel (get-universal-time))))))

(as:with-event-loop ()
  (as:signal-handler
   as:+sigint+ (lambda (sig)
                 (declare (ignore sig))
                 (as:exit-event-loop)))
  (sc:run-client *c*))

(finalize)
