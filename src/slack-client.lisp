(in-package :cl-user)
(defpackage slack-client
  (:use :cl
        :blackbird))
(in-package :slack-client)

(defun slack-api-token ()
  (or (ccl:getenv "SLACK_API_TOKEN")
      (error "env SLACK_API_TOKEN does not set.")))

(defun my-http-request ()
  (catcher
   (multiple-promise-bind
       (body status headers)
       (das:http-request "https://slack.com/api/rtm.start"
                         :method :post
                         :parameters `(("token" . ,(slack-api-token))))
     (format t "Status: ~a~%" status)
     (format t "Headers: ~s~%" headers)
     (format t "Body: ~a~%" (if (stringp body) body (babel:octets-to-string body))))
   (das:http-eof ()
                 (format t "Server hung up unexpectedly =[~%"))
   (error (e)
          (format t "Error: ~a~%" e))))

(defun run ()
  (as:start-event-loop #'my-http-request))
