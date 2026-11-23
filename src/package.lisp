(defpackage #:mcp-backend-streamable-http
  (:use #:cl)
  (:export #:streamable-http-mcp-backend
           #:make-streamable-http-mcp-backend
           #:use-streamable-http-mcp-backend
           #:make-mcp-app
           #:streamable-http-rpc-transport
           #:make-streamable-http-rpc-transport
           #:transport-session-id
           #:transport-protocol-version
           ;; 0.4.0
           #:transport-headers
           #:transport-on-notification
           #:transport-on-request
           #:mcp-transport-unsupported
           #:mcp-transport-unsupported-url
           #:mcp-transport-unsupported-transport
           #:mcp-transport-unsupported-status))

(in-package #:mcp-backend-streamable-http)
