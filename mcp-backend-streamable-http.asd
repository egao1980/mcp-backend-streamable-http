(defsystem "mcp-backend-streamable-http"
  :version "0.2.0"
  :description "Streamable HTTP transport backend for mcp-protocol (POST JSON / optional SSE)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("mcp-protocol" "rpc-backend-http" "sse-protocol" "http-protocol"
               "http-server-protocol" "babel")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "mcp-backend-streamable-http/tests"))))

(defsystem "mcp-backend-streamable-http/tests"
  :depends-on ("mcp-backend-streamable-http" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
