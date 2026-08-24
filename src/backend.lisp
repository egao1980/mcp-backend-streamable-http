(in-package #:mcp-backend-streamable-http)

(defclass streamable-http-mcp-backend (mcp-protocol:mcp-backend) ())

(defun make-streamable-http-mcp-backend ()
  (make-instance 'streamable-http-mcp-backend))

(defun use-streamable-http-mcp-backend ()
  (setf mcp-protocol:*mcp-backend* (make-streamable-http-mcp-backend)))

(defun %header (env name)
  (let ((headers (getf env :headers)))
    (cond
      ((hash-table-p headers)
       (or (gethash name headers)
           (gethash (string-downcase name) headers)))
      ((listp headers)
       (cdr (assoc name headers :test #'string-equal)))
      (t nil))))

(defun %wants-sse (env)
  (let ((accept (or (getf env :accept) (%header env "accept") "")))
    (and (stringp accept)
         (search "text/event-stream" accept :test #'char-equal))))

(defun make-mcp-app (server &key path)
  "Clack app: POST JSON-RPC. Optional Accept: text/event-stream → SSE message event.
   GET is 405 (wave-1). Required MCP headers are accepted but not required locally."
  (let ((inner (rpc-backend-http:make-rpc-app
                (lambda (method params)
                  (mcp-protocol:dispatch-mcp-method server method params))
                :path path)))
    (lambda (env)
      (let ((res (funcall inner env)))
        (if (and (eql (first res) 200) (%wants-sse env))
            (list 200
                  '(:content-type "text/event-stream; charset=utf-8"
                    :cache-control "no-cache")
                  (list (sse-protocol:encode-sse-event
                         (sse-protocol:make-sse-event
                          :event "message"
                          :data (first (third res))))))
            res)))))

(defun %ensure-http-server ()
  (or http-server-protocol:*http-server-backend*
      (progn
        (asdf:load-system "http-server-backend-hunchentoot")
        (funcall (find-symbol "USE-HUNCHENTOOT-BACKEND"
                              :http-server-backend-hunchentoot)))))

(defclass streamable-http-rpc-transport (rpc-protocol:rpc-transport)
  ((url :initarg :url :initform nil :accessor transport-url)
   (protocol-version :initarg :protocol-version
                     :accessor transport-protocol-version
                     :initform mcp-protocol:+mcp-protocol-version+)
   (mcp-name :initarg :mcp-name :accessor transport-mcp-name
             :initform "cl-stack-mcp")
   (next-id :initform 0 :accessor transport-next-id)))

(defun make-streamable-http-rpc-transport
    (&key url (protocol-version mcp-protocol:+mcp-protocol-version+)
       (mcp-name "cl-stack-mcp"))
  (make-instance 'streamable-http-rpc-transport
                 :url url
                 :protocol-version protocol-version
                 :mcp-name mcp-name))

(defun %octets-to-string (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(defun %body-string (response)
  (let ((b (http-protocol:response-body response)))
    (cond
      ((stringp b) b)
      ((and (vectorp b) (not (stringp b)))
       (%octets-to-string b))
      (t ""))))

(defun %raise-rpc (msg)
  (let ((err (gethash "error" msg)))
    (if err
        (error 'rpc-protocol:rpc-error
               :code (or (gethash "code" err) rpc-protocol:+internal-error+)
               :message (gethash "message" err)
               :data (gethash "data" err))
        (gethash "result" msg))))

(defun %decode-body (body content-type)
  (if (and (stringp content-type)
           (search "text/event-stream" content-type :test #'char-equal))
      (let ((ev (sse-protocol:decode-sse-block body)))
        (rpc-protocol:decode-message (sse-protocol:sse-event-data ev)))
      (rpc-protocol:decode-message body)))

(defun %mcp-headers (transport method)
  `(("content-type" . "application/json")
    ("accept" . "application/json, text/event-stream")
    ("MCP-Protocol-Version" . ,(transport-protocol-version transport))
    ("Mcp-Method" . ,method)
    ("Mcp-Name" . ,(transport-mcp-name transport))))

(defmethod rpc-protocol:backend-rpc-call
    ((transport streamable-http-rpc-transport) method params &key timeout id)
  (unless http-protocol:*http-backend*
    (error 'rpc-protocol:rpc-error
           :message "*http-backend* is nil — bind an http-protocol backend"
           :code rpc-protocol:+internal-error+))
  (let* ((id (or id (incf (transport-next-id transport))))
         (url (or (transport-url transport)
                  (error 'rpc-protocol:rpc-error
                         :message "streamable HTTP transport has no :url"
                         :code rpc-protocol:+internal-error+)))
         (res (apply #'http:post url
                     :content (rpc-protocol:encode-request method params :id id)
                     :headers (%mcp-headers transport method)
                     (when timeout (list :timeout timeout)))))
    (unless (<= 200 (http-protocol:response-status res) 299)
      (error 'rpc-protocol:rpc-error
             :message (format nil "HTTP ~a" (http-protocol:response-status res))
             :code rpc-protocol:+internal-error+))
    (%raise-rpc
     (%decode-body (%body-string res)
                   (http-protocol:response-header res "content-type")))))

(defmethod rpc-protocol:backend-rpc-notify
    ((transport streamable-http-rpc-transport) method params)
  (unless http-protocol:*http-backend*
    (error 'rpc-protocol:rpc-error
           :message "*http-backend* is nil — bind an http-protocol backend"
           :code rpc-protocol:+internal-error+))
  (http:post (or (transport-url transport)
                          (error 'rpc-protocol:rpc-error
                                 :message "streamable HTTP transport has no :url"
                                 :code rpc-protocol:+internal-error+))
                      :content (rpc-protocol:encode-notification method params)
                      :headers (%mcp-headers transport method))
  t)

(defmethod rpc-protocol:backend-rpc-serve
    ((transport streamable-http-rpc-transport) handler &key)
  (declare (ignore handler))
  (error 'rpc-protocol:rpc-error
         :message "use backend-mcp-serve / make-mcp-app, not rpc-serve on this transport"
         :code rpc-protocol:+internal-error+))

(defmethod mcp-protocol:backend-mcp-connect
    ((backend streamable-http-mcp-backend) &key url
                                             (era :unknown) (probe nil)
                                             name version
                                             protocol-version)
  (let ((client (make-instance 'mcp-protocol:mcp-client
                               :transport (make-streamable-http-rpc-transport
                                           :url url
                                           :protocol-version
                                           (or protocol-version
                                               mcp-protocol:+mcp-protocol-version+)
                                           :mcp-name (or name "cl-stack-mcp"))
                               :era era
                               :name (or name "cl-stack-mcp")
                               :version (or version "0.1.0")
                               :protocol-version
                               (or protocol-version
                                   mcp-protocol:+mcp-protocol-version+))))
    (when probe
      (mcp-protocol:mcp-initialize client))
    client))

(defmethod mcp-protocol:backend-mcp-serve
    ((backend streamable-http-mcp-backend) server
     &key (host "127.0.0.1") (port 8080) (path "/"))
  (%ensure-http-server)
  (http-server-protocol:serve (make-mcp-app server :path path)
                              :host host :port port))

(use-streamable-http-mcp-backend)
