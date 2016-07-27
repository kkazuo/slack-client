(in-package :cl-user)
(defpackage slack-client
  (:use :cl
   :blackbird))
(in-package :slack-client)

(defun slack-api-token ()
  (or (ccl:getenv "SLACK_API_TOKEN")
      (error "env SLACK_API_TOKEN does not set.")))

(defun rtm-start (token)
  (catcher
   (multiple-promise-bind
       (body status headers)
       (das:http-request "https://slack.com/api/rtm.start"
                         :method :post
                         :parameters `(("token" . ,token)))
     (declare (ignore status headers))
     (jonathan:parse
      (babel:octets-to-string body :encoding :iso-8859-1)
      :as :hash-table))
   (das:http-eof ()
                 (format t "Server hung up unexpectedly =[~%"))
   (error (e)
          (format t "Error: ~a~%" e))))

(defun run ()
  (as:start-event-loop
   (lambda ()
     (chain (slack-api-token)
       (:then (token)
              (rtm-start token))
       (:then (info)
              (if (gethash "ok" info)
                  (format t "~A~%" (gethash "url" info))
                  (format t "failed.~%")))
       ))))
