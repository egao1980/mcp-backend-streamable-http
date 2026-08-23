(in-package #:mcp-backend-streamable-http/tests)

(deftest backend-class
  (ok (typep (mcp-backend-streamable-http:make-streamable-http-mcp-backend) 'mcp-backend-streamable-http:streamable-http-mcp-backend)))
