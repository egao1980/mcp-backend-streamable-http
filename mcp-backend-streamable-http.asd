(defsystem "mcp-backend-streamable-http"
  :version "0.4.0"
  :description "Streamable HTTP transport backend for mcp-protocol (POST JSON / optional SSE): caller headers, the server's SSE notifications and requests routed to the client, legacy HTTP+SSE servers reported as MCP-TRANSPORT-UNSUPPORTED"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "mcp-protocol" "0.4.1")
               "rpc-backend-http" "rpc-protocol-json"
               "sse-protocol" "http-protocol" "http-server-protocol" "babel")
  :properties (:cl-repo (:ci (:with ("dissect"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "mcp-backend-streamable-http/tests"))))

(defsystem "mcp-backend-streamable-http/tests"
  :depends-on ("mcp-backend-streamable-http" "rpc-protocol-json" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test")
               ;; 0.4.0
               (:file "client-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
