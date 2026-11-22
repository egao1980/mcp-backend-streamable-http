(in-package #:mcp-backend-streamable-http/tests)

;;; 0.4.0 (egao1980/mcp-backend-streamable-http#9): caller headers, the
;;; server's SSE notifications and requests reaching the client, legacy
;;; HTTP+SSE reported. The server is a Clack app (MAKE-MCP-APP, or a
;;; hand-written one) behind LOOPBACK-HTTP-BACKEND, an http-protocol backend
;;; whose SEND turns the request into a Clack env and calls the app, recording
;;; every request. No socket.

(defclass loopback-http-backend (http-protocol:http-backend)
  ((app :initarg :app :accessor loopback-app)
   (requests :initform '() :accessor loopback-requests
             :documentation "Plists (:method :path :headers :body :timeout), newest first."))
  (:default-initargs :name "loopback"))

(defun %header-alist (headers)
  (cond ((hash-table-p headers)
         (loop for k being the hash-keys of headers using (hash-value v)
               collect (cons (string-downcase (string k)) v)))
        (t (loop for h in headers
                 collect (cons (string-downcase (string (car h))) (cdr h))))))

(defun %url-path (url)
  (let* ((s (princ-to-string url))
         (start (or (search "://" s) -3))
         (slash (position #\/ s :start (+ start 3))))
    (if slash (subseq s slash (or (position #\? s :start slash) (length s))) "/")))

(defmethod http-protocol:send ((backend loopback-http-backend) client request &key)
  (declare (ignore client))
  (let* ((headers (%header-alist (http-protocol:http-request-headers request)))
         (table (let ((h (make-hash-table :test 'equal)))
                  (loop for (k . v) in headers do (setf (gethash k h) v))
                  h))
         (content (http-protocol:http-request-content request))
         (body (if (stringp content)
                   content
                   (babel:octets-to-string (coerce content '(vector (unsigned-byte 8)))
                                           :encoding :utf-8)))
         (path (%url-path (http-protocol:http-request-url request))))
    (push (list :method (http-protocol:http-request-method request) :path path
                :headers headers :body body
                :timeout (http-protocol:http-request-timeout request))
          (loopback-requests backend))
    (destructuring-bind (status plist parts)
        (funcall (loopback-app backend)
                 (list :request-method (http-protocol:http-request-method request)
                       :path-info path :server-name "mcp.test" :server-port 80
                       :headers table :raw-body body))
      (let ((h (make-hash-table :test 'equal)))
        (loop for (k v) on plist by #'cddr do (setf (gethash (string-downcase (string k)) h) v))
        (make-instance 'http-protocol:http-response
                       :status status :headers h
                       :body (format nil "~{~a~}" parts))))))

(defmacro with-loopback ((backend app) &body body)
  `(let* ((,backend (make-instance 'loopback-http-backend :app ,app))
          (http-protocol:*http-backend* ,backend))
     ,@body))

(defun %connect (&rest keys)
  (apply #'mcp-protocol:backend-mcp-connect
         (mcp-backend-streamable-http:make-streamable-http-mcp-backend)
         :url "http://mcp.test/mcp" keys))

(defun %request-header (request name)
  (cdr (assoc (string-downcase name) (getf request :headers) :test #'string=)))

(defun %obj (&rest kv) (apply #'mcp-protocol:json-object kv))

(defun %tool-text (result)
  (let ((content (and (hash-table-p result) (gethash "content" result))))
    (and content (plusp (length content)) (gethash "text" (elt content 0)))))

;;; A server whose "busy" tool sends a log line, progress and list_changed
;;; while the call runs (SSE events before the response).

(defun %busy-server ()
  (let ((server (make-instance 'mcp-protocol:mcp-server :name "busy" :version "1.0.0")))
    (mcp-protocol:register-tool
     server
     (mcp-protocol:make-mcp-tool
      "busy" :input-schema (%obj "type" "object")
      :handler (lambda (args)
                 (declare (ignore args))
                 (mcp-protocol:mcp-log server "info" "step one" :logger "busy")
                 ;; no progressToken in the call, so the notification is sent as is
                 (mcp-protocol:mcp-notify server "notifications/progress"
                                          (%obj "progressToken" "p1" "progress" 1 "total" 2 "message" "half"))
                 (mcp-protocol:mcp-notify server "notifications/tools/list_changed" (%obj))
                 "done")))
    server))

;;; headers

(deftest headers-reach-the-server-on-every-request
  (with-loopback (backend (mcp-backend-streamable-http:make-mcp-app (%busy-server) :path "/mcp"))
    (let ((client (%connect :headers '(("Authorization" . "Bearer s3cret")
                                       ("X-Trace" . "abc")
                                       ("Mcp-Method" . "evil")
                                       ("accept" . "text/plain")))))
      (mcp-protocol:mcp-initialize client)
      (mcp-protocol:call-tool client "busy" nil)
      (let ((requests (loopback-requests backend)))
        (ok (>= (length requests) 2) "initialize and tools/call at least")
        (ok (every (lambda (r) (equal "Bearer s3cret" (%request-header r "authorization"))) requests)
            "Authorization on every POST")
        (ok (every (lambda (r) (equal "abc" (%request-header r "x-trace"))) requests))
        (ok (equal "tools/call" (%request-header (first requests) "mcp-method"))
            "an MCP header cannot be overridden")
        (ok (= 1 (count "mcp-method" (getf (first requests) :headers) :key #'car :test #'string=))
            "and is not sent twice")
        (ok (search "text/event-stream" (%request-header (first requests) "accept")))))))

(deftest headers-from-a-hash-table
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "Authorization" h) "Bearer t")
    (with-loopback (backend (mcp-backend-streamable-http:make-mcp-app (%busy-server) :path "/mcp"))
      (let ((client (%connect :headers h)))
        (mcp-protocol:mcp-initialize client)
        (ok (equal "Bearer t" (%request-header (first (loopback-requests backend)) "authorization")))
        (ok (equal '(("Authorization" . "Bearer t"))
                   (mcp-backend-streamable-http:transport-headers
                    (mcp-protocol:mcp-client-transport client))))))))

(deftest headers-must-be-strings
  (ok (signals (mcp-backend-streamable-http:make-streamable-http-rpc-transport
                :url "http://mcp.test/" :headers '(("X-N" . 5)))
               'type-error))
  (ok (signals (mcp-backend-streamable-http:make-streamable-http-rpc-transport
                :url "http://mcp.test/" :headers "Authorization: x")
               'type-error)))

;;; server notifications

(deftest sse-notifications-reach-the-client-in-order
  (with-loopback (backend (mcp-backend-streamable-http:make-mcp-app (%busy-server) :path "/mcp"))
    (declare (ignorable backend))
    (let* ((seen '()) (logs '()) (progress '()) (changed 0)
           (client (%connect
                    :log-handler (lambda (m) (push (list (mcp-protocol:mcp-log-message-level m)
                                                         (mcp-protocol:mcp-log-message-data m))
                                                   logs))
                    :progress-handler (lambda (p) (push (list (mcp-protocol:mcp-progress-value p)
                                                              (mcp-protocol:mcp-progress-total p)
                                                              (mcp-protocol:mcp-progress-message p))
                                                        progress))
                    :on-tools-changed (lambda (c) (declare (ignore c)) (incf changed))
                    :notification-hook (lambda (c method params)
                                         (declare (ignore c params))
                                         (push method seen)))))
      (mcp-protocol:mcp-initialize client)
      (ok (equal "done" (%tool-text (mcp-protocol:call-tool client "busy" nil))))
      (ok (equal '("notifications/message" "notifications/progress"
                   "notifications/tools/list_changed")
                 (reverse seen))
          "every notification, in order, before call-tool returned")
      (ok (equal '(("info" "step one")) logs))
      (ok (equal '((1 2 "half")) progress))
      (ok (= 1 changed))
      (ok (= 1 (mcp-protocol:mcp-client-tools-generation client))))))

;;; server requests in an SSE response

(defun %scripted-app (events-for)
  "A Clack app: a POST with a method gets an SSE body of (funcall EVENTS-FOR
method id) messages (hash tables / strings); a POST without one (a client's
answer) gets 202."
  (lambda (env)
    (let* ((msg (rpc-protocol:decode-message
                 (mcp-backend-streamable-http::slurp-env-body env)))
           (method (gethash "method" msg)))
      (if (null method)
          '(202 () (""))
          (list 200 '(:content-type "text/event-stream")
                (mapcar (lambda (data)
                          (sse-protocol:encode-sse-event
                           (sse-protocol:make-sse-event :event "message" :data data)))
                        (funcall events-for method (gethash "id" msg))))))))

(deftest sse-server-requests-are-answered-by-post
  (with-loopback (backend
                  (%scripted-app
                   (lambda (method id)
                     (declare (ignore method))
                     (list (rpc-protocol:encode-request "ping" (%obj) :id "s1")
                           (rpc-protocol:encode-request "roots/list" (%obj) :id "s2")
                           (rpc-protocol:encode-request "sampling/createMessage" (%obj) :id "s3")
                           (rpc-protocol:encode-notification "notifications/tools/list_changed" (%obj))
                           (rpc-protocol:encode-response (%obj "tools" #()) :id id)))))
    (let* ((client (%connect :era :legacy
                             :roots (list (mcp-protocol:make-mcp-root "file:///w"))))
           (tools (mcp-protocol:list-tools client)))
      (ok (null tools) "the call's own response is the result")
      (ok (= 1 (mcp-protocol:mcp-client-tools-generation client)) "the notification was routed")
      (let ((answers (mapcar (lambda (r) (rpc-protocol:decode-message (getf r :body)))
                             (remove-if (lambda (r) (search "\"method\"" (getf r :body)))
                                        (reverse (loopback-requests backend))))))
        (ok (equal '("s1" "s2" "s3") (mapcar (lambda (m) (gethash "id" m)) answers))
            "each server request got a POSTed answer, in order")
        (ok (hash-table-p (gethash "result" (first answers))) "ping → {}")
        (ok (equal "file:///w"
                   (gethash "uri" (elt (gethash "roots" (gethash "result" (second answers))) 0)))
            "roots/list → the client's roots")
        (ok (eql rpc-protocol:+method-not-found+
                 (gethash "code" (gethash "error" (third answers))))
            "sampling without a handler → -32601")))))

(deftest the-app-accepts-a-client-answer
  (let* ((app (mcp-backend-streamable-http:make-mcp-app (%busy-server)))
         (res (funcall app (list :request-method :post :path-info "/"
                                 :raw-body (rpc-protocol:encode-response (%obj) :id "s1")
                                 :headers (make-hash-table :test 'equal)))))
    (ok (eql 202 (first res)))))

;;; legacy HTTP+SSE

(defun %legacy-app ()
  "A 2024-11-05 server's SSE endpoint: GET only."
  (lambda (env)
    (if (eq :get (getf env :request-method))
        '(200 (:content-type "text/event-stream") ("event: endpoint
data: /messages?session=1

"))
        '(405 (:content-type "text/plain" :allow "GET") ("Method Not Allowed")))))

(deftest legacy-405-is-transport-unsupported
  (with-loopback (backend (%legacy-app))
    (declare (ignorable backend))
    (let ((client (%connect)))
      (handler-case (progn (mcp-protocol:mcp-initialize client)
                           (ok nil "initialize must fail"))
        (mcp-backend-streamable-http:mcp-transport-unsupported (c)
          (ok t "a distinct condition")
          (ok (typep c 'mcp-protocol:mcp-error) "an MCP-ERROR")
          (ok (eql 405 (mcp-backend-streamable-http:mcp-transport-unsupported-status c)))
          (ok (eq :sse (mcp-backend-streamable-http:mcp-transport-unsupported-transport c)))
          (ok (equal "http://mcp.test/mcp" (mcp-backend-streamable-http:mcp-transport-unsupported-url c)))
          (ok (search "legacy HTTP+SSE" (princ-to-string c))))))))

(deftest legacy-endpoint-event-is-transport-unsupported
  (with-loopback (backend (lambda (env)
                            (declare (ignore env))
                            '(200 (:content-type "text/event-stream")
                              ("event: endpoint
data: /messages?session=1

"))))
    (declare (ignorable backend))
    (ok (signals (mcp-protocol:mcp-initialize (%connect :era :legacy))
                 'mcp-backend-streamable-http:mcp-transport-unsupported))))

(deftest other-http-errors-are-not-unsupported
  (with-loopback (backend (lambda (env)
                            (declare (ignore env))
                            '(404 (:content-type "text/plain") ("no such endpoint"))))
    (declare (ignorable backend))
    (handler-case (progn (mcp-protocol:mcp-initialize (%connect :era :legacy))
                         (ok nil "initialize must fail"))
      (mcp-backend-streamable-http:mcp-transport-unsupported ()
        (ok nil "a 404 is not the legacy transport"))
      (mcp-protocol:mcp-error (c)
        (ok (search "404" (princ-to-string c)))))))

(deftest a-405-after-the-session-started-is-an-rpc-error
  (let ((established nil))
    (with-loopback (backend (let ((app (mcp-backend-streamable-http:make-mcp-app (%busy-server) :path "/mcp")))
                              (lambda (env)
                                (if established
                                    '(405 (:content-type "text/plain") ("no"))
                                    (funcall app env)))))
      (declare (ignorable backend))
      (let ((client (%connect)))
        (mcp-protocol:mcp-initialize client)
        (setf established t)
        (handler-case (progn (mcp-protocol:call-tool client "busy" nil) (ok nil "must fail"))
          (mcp-backend-streamable-http:mcp-transport-unsupported () (ok nil "not after a success"))
          (mcp-protocol:mcp-error () (ok t)))))))

;;; timeouts

(deftest call-tool-timeout-reaches-http-post
  (with-loopback (backend (mcp-backend-streamable-http:make-mcp-app (%busy-server) :path "/mcp"))
    (let ((client (%connect)))
      (mcp-protocol:mcp-initialize client)
      (mcp-protocol:call-tool client "busy" nil :timeout 7)
      (let ((timeout (getf (first (loopback-requests backend)) :timeout)))
        (ok (or (eql 7 timeout)
                (and timeout (search "7" (princ-to-string timeout))))
            (format nil "the POST had :timeout 7, got ~s" timeout))))))
