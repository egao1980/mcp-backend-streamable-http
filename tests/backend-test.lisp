(in-package #:mcp-backend-streamable-http/tests)

(defun %headers (&optional (accept "application/json"))
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "accept" h) accept
          (gethash "mcp-protocol-version" h) "2026-07-28"
          (gethash "mcp-method" h) "server/discover"
          (gethash "mcp-name" h) "test")
    h))

(defun %discover-body ()
  (rpc-protocol:encode-request
   "server/discover"
   (mcp-protocol:json-object
    "_meta" (mcp-protocol:json-object
             "io.modelcontextprotocol/protocolVersion" "2026-07-28"
             "io.modelcontextprotocol/clientInfo"
             (mcp-protocol:json-object "name" "t" "version" "0")
             "io.modelcontextprotocol/clientCapabilities"
             (mcp-protocol:json-object)))
   :id 1))

(deftest backend-class
  (ok (typep (mcp-backend-streamable-http:make-streamable-http-mcp-backend)
             'mcp-backend-streamable-http:streamable-http-mcp-backend)))

(deftest clack-discover-json
  (let* ((server (make-instance 'mcp-protocol:mcp-server
                                :name "http-fix" :version "0.1.0"))
         (app (mcp-backend-streamable-http:make-mcp-app server))
         (res (funcall app (list :request-method :post
                                 :path-info "/"
                                 :raw-body (%discover-body)
                                 :headers (%headers)))))
    (ok (eql 200 (first res)))
    (ok (search "application/json" (getf (second res) :content-type)))
    (let* ((msg (rpc-protocol:decode-message (first (third res))))
           (result (gethash "result" msg)))
      (ok (equal "complete" (gethash "resultType" result)))
      (ok (find "2025-11-25" (coerce (gethash "supportedVersions" result) 'list)
                :test #'string=)))))

(deftest clack-discover-sse
  (let* ((server (make-instance 'mcp-protocol:mcp-server
                                :name "http-fix" :version "0.1.0"))
         (app (mcp-backend-streamable-http:make-mcp-app server))
         (res (funcall app (list :request-method :post
                                 :path-info "/"
                                 :raw-body (%discover-body)
                                 :headers (%headers "text/event-stream")))))
    (ok (eql 200 (first res)))
    (ok (search "text/event-stream" (getf (second res) :content-type)))
    (let* ((ev (sse-protocol:decode-sse-block (first (third res))))
           (msg (rpc-protocol:decode-message (sse-protocol:sse-event-data ev)))
           (result (gethash "result" msg)))
      (ok (equal "message" (sse-protocol:sse-event-type ev)))
      (ok (equal "complete" (gethash "resultType" result))))))

(deftest clack-get-405
  (let* ((server (make-instance 'mcp-protocol:mcp-server))
         (app (mcp-backend-streamable-http:make-mcp-app server))
         (res (funcall app (list :request-method :get :path-info "/"))))
    (ok (eql 405 (first res)))))

(deftest connect-shape
  (let ((client (mcp-protocol:backend-mcp-connect
                 (mcp-backend-streamable-http:make-streamable-http-mcp-backend)
                 :url "http://127.0.0.1:9/mcp")))
    (ok (typep client 'mcp-protocol:mcp-client))
    (ok (typep (mcp-protocol:mcp-client-transport client)
               'mcp-backend-streamable-http:streamable-http-rpc-transport))))

(deftest mcp-name-from-params
  (let ((tr (mcp-backend-streamable-http:make-streamable-http-rpc-transport
             :url "http://127.0.0.1:9/mcp" :mcp-name "client")))
    (let ((h (mcp-backend-streamable-http::%mcp-headers
              tr "tools/call" (mcp-protocol:json-object "name" "echo"))))
      (ok (equal "echo" (cdr (assoc "Mcp-Name" h :test #'string=)))))
    (let ((h (mcp-backend-streamable-http::%mcp-headers
              tr "resources/read" (mcp-protocol:json-object "uri" "memo://hi"))))
      (ok (equal "memo://hi" (cdr (assoc "Mcp-Name" h :test #'string=)))))
    (let ((h (mcp-backend-streamable-http::%mcp-headers tr "tools/list")))
      (ok (equal "client" (cdr (assoc "Mcp-Name" h :test #'string=)))))))

(deftest session-header-roundtrip
  (let ((tr (mcp-backend-streamable-http:make-streamable-http-rpc-transport
             :url "http://127.0.0.1:9/mcp")))
    (setf (mcp-backend-streamable-http:transport-session-id tr) "sess-1")
    (let ((h (mcp-backend-streamable-http::%mcp-headers tr "tools/list")))
      (ok (equal "sess-1" (cdr (assoc "Mcp-Session-Id" h :test #'string=)))))))

(deftest origin-forbidden
  (let* ((server (make-instance 'mcp-protocol:mcp-server))
         (app (mcp-backend-streamable-http:make-mcp-app
               server :allowed-origins '("http://ok.example")))
         (headers (%headers)))
    (setf (gethash "origin" headers) "http://evil.example")
    (let ((res (funcall app (list :request-method :post
                                  :path-info "/"
                                  :raw-body (%discover-body)
                                  :headers headers))))
      (ok (eql 403 (first res))))))

(deftest header-mismatch-400
  (let* ((server (make-instance 'mcp-protocol:mcp-server))
         (app (mcp-backend-streamable-http:make-mcp-app server))
         (headers (%headers)))
    (setf (gethash "mcp-method" headers) "tools/list")
    (let* ((res (funcall app (list :request-method :post
                                   :path-info "/"
                                   :raw-body (%discover-body)
                                   :headers headers)))
           (msg (rpc-protocol:decode-message (first (third res)))))
      (ok (eql 400 (first res)))
      (ok (eql mcp-protocol:+mcp-error-header-mismatch+
               (gethash "code" (gethash "error" msg)))))))

(deftest notification-is-202
  (let* ((server (make-instance 'mcp-protocol:mcp-server))
         (app (mcp-backend-streamable-http:make-mcp-app server))
         (headers (%headers))
         (body (rpc-protocol:encode-notification
                "notifications/cancelled"
                (mcp-protocol:json-object))))
    (setf (gethash "mcp-method" headers) "notifications/cancelled")
    (let ((res (funcall app (list :request-method :post
                                  :path-info "/"
                                  :raw-body body
                                  :headers headers))))
      (ok (eql 202 (first res)))
      (ok (equal "" (first (third res)))))))

(defun %echo-server ()
  (let ((server (make-instance 'mcp-protocol:mcp-server
                               :name "http-fix" :version "0.2.0")))
    (mcp-protocol:register-tool
     server
     (mcp-protocol:make-mcp-tool
      "echo" :description "echo msg"
      :input-schema (mcp-protocol:json-object
                     "type" "object"
                     "properties" (mcp-protocol:json-object
                                   "msg" (mcp-protocol:json-object "type" "string"))
                     "required" #("msg"))
      :handler (lambda (args)
                 (mcp-protocol:tool-result
                  (list (mcp-protocol:make-text-content
                         (or (mcp-protocol:param args "msg") "")))))))
    server))

(deftest tools-call-invalid-schema-is-32602
  "mcp-error from inputSchema must not collapse to -32603 via make-rpc-app."
  (let* ((app (mcp-backend-streamable-http:make-mcp-app (%echo-server)))
         (headers (%headers))
         (body (rpc-protocol:encode-request
                "tools/call"
                (mcp-protocol:json-object
                 "name" "echo"
                 "arguments" (mcp-protocol:json-object))
                :id 1)))
    (setf (gethash "mcp-method" headers) "tools/call"
          (gethash "mcp-name" headers) "echo")
    (let* ((res (funcall app (list :request-method :post
                                   :path-info "/"
                                   :raw-body body
                                   :headers headers)))
           (msg (rpc-protocol:decode-message (first (third res)))))
      (ok (eql 200 (first res)))
      (ok (eql rpc-protocol:+invalid-params+
               (gethash "code" (gethash "error" msg)))))))
