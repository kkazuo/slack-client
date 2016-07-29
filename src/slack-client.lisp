(in-package :cl-user)
(defpackage slack-client
  (:nicknames #:sc #:slack)
  (:use :cl)
  (:export #:run-client
           #:slack-client))
(in-package :slack-client)

(defclass slack-client ()
  ((state :initform nil :accessor state-of)
   (send-id :initform 0 :accessor send-id-of)
   (last-ping :initform nil :accessor last-ping-of)
   (fail-ping :initform 0 :accessor fail-ping-of)))

(defun prop-of (client key)
  (cdr (assoc key (state-of client) :test #'equal)))

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
      :as :alist))
   (das:http-eof ()
                 (format t "Server hung up unexpectedly =[~%"))
   (error (e)
          (format t "Error: ~a~%" e))))

(defun asv (key alist)
  (cdr (assoc key alist :test #'equal)))

(defun ping (client ws)
  (cond ((last-ping-of client)
         (cond ((< 3 (incf (fail-ping-of client)))
                (as:exit-event-loop)))))
  (wsd:send-text
   ws
   (jonathan:to-json
    (list :|type| "ping"
          :|id| (setf (last-ping-of client)
                      (incf (send-id-of client)))
          :|time| (get-internal-run-time)))))

(defun pong (client ws reply-to time)
  (let* ((now (get-internal-run-time))
         (dif (- now time)))
    (cond ((eql reply-to (last-ping-of client))
           (setf (last-ping-of client) nil
                 (fail-ping-of client) 0)))))

(defun tick (client ws)
  (ping client ws))

(defun on-message (client ws message)
  (format t "~S~%" message)
  (let ((type (asv "type" message)))
    (cond ((equal type "pong")
           (pong client ws
                 (asv "reply_to" message)
                 (asv "time" message)))
          (t
           nil))))

(defun ws-connect (client)
  (cond ((prop-of client "ok")
         (let* ((url (prop-of client "url"))
                (ws (wsd:make-client url)))
           (event-emitter:on
            :message ws
            (lambda (message)
              (on-message client ws (jonathan:parse message :as :alist))))
           (event-emitter:on
            :open ws
            (lambda () (as:with-interval (1) (tick client ws))))
           (event-emitter:on
            :close ws
            (lambda (code reason)
              (format t "Closed because '~A' (Code=~A)~%" reason code)))
           (wsd:start-connection ws))
         (format t "started.~%"))
        (t
         (format t "failed.~%"))))

(defun run-client (client)
  (as:with-event-loop ()
    (as:signal-handler
     as:+sigint+ (lambda (sig)
                   (declare (ignore sig))
                   (as:exit-event-loop)))
    (bb:chain (slack-api-token)
      (:then (token)
             (rtm-start token))
      (:then (info)
             (setf (state-of client) info)
             (format t "~S~%" info)
             (ws-connect client)))))
