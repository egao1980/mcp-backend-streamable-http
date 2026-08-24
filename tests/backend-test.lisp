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
