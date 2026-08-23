(in-package #:mcp-backend-streamable-http)

(defclass streamable-http-mcp-backend (mcp-protocol:mcp-backend) ())

(defun make-streamable-http-mcp-backend ()
  (make-instance 'streamable-http-mcp-backend))

(defun use-streamable-http-mcp-backend ()
  (setf mcp-protocol:*mcp-backend* (make-streamable-http-mcp-backend)))
