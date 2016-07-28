(in-package :cl-user)
(defpackage slack-client
  (:nicknames #:sc #:slack)
  (:use :cl)
  (:export #:run-client))
(in-package :slack-client)

(defun slack-api-token ()
  (or (ccl:getenv "SLACK_API_TOKEN")
      (error "env SLACK_API_TOKEN does not set.")))

(defun rtm-start (token)
  (bb:catcher
   (bb:multiple-promise-bind
       (body status headers)
       (das:http-request "https://slack.com/api/rtm.start"
                         :method :post
                         :parameters `(("token" . ,token)))
     (declare (ignorable status headers))
     (jonathan:parse
      (babel:octets-to-string body :encoding :iso-8859-1)
      :as :hash-table))
   (das:http-eof ()
                 (format t "Server hung up unexpectedly =[~%"))
   (error (e)
          (format t "Error: ~a~%" e))))

(defun run-client ()
  (as:with-event-loop ()
    (as:signal-handler 2 (lambda (sig)
                           (declare (ignore sig))
                           (as:exit-event-loop)))
    (bb:chain (slack-api-token)
      (:then (token)
             (rtm-start token))
      (:then (info)
             (cond ((gethash "ok" info)
                    (let* ((url (gethash "url" info))
                             (ws (wsd:make-client url)))
                        (event-emitter:on :message ws
                                          (lambda (message)
                                            (format t "~S~%"
                                                    (jonathan:parse message))))
                        (event-emitter:on :close ws
                                          (lambda (code reason)
                                            (format t "Closed because '~A' (Code=~A)~%" reason code)))
                        (wsd:start-connection ws)))
                   (t (format t "failed.~%"))))
      )))
