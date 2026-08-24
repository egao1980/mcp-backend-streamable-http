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
   (session-id :initarg :session-id :initform nil :accessor transport-session-id)
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

(defun %param-string (params key)
  (when (hash-table-p params)
    (let ((v (gethash key params)))
      (and (stringp v) v))))

(defun %mcp-name-for (method params default)
  "Modern HTTP: Mcp-Name is the tool/prompt name or resource URI, not the client name."
  (or (and (member method '("tools/call" "prompts/get") :test #'string=)
           (%param-string params "name"))
      (and (string= method "resources/read")
           (%param-string params "uri"))
      default))

(defun %mcp-headers (transport method &optional params)
  (let ((headers `(("content-type" . "application/json")
                   ("accept" . "application/json, text/event-stream")
                   ("MCP-Protocol-Version" . ,(transport-protocol-version transport))
                   ("Mcp-Method" . ,method)
                   ("Mcp-Name" . ,(%mcp-name-for method params
                                                 (transport-mcp-name transport))))))
    (when (transport-session-id transport)
      (push (cons "Mcp-Session-Id" (transport-session-id transport)) headers))
    headers))

(defun %maybe-store-session (transport response)
  (let ((sid (http-protocol:response-header response "mcp-session-id")))
    (when (and sid (stringp sid) (plusp (length sid)))
      (setf (transport-session-id transport) sid)))
  transport)

(defun %maybe-store-protocol-version (transport method result)
  (when (and (hash-table-p result)
             (member method '("initialize" "server/discover") :test #'string=))
    (let ((ver (gethash "protocolVersion" result)))
      (when (stringp ver)
        (setf (transport-protocol-version transport) ver))))
  result)

(defun %ensure-url (transport)
  (or (transport-url transport)
      (error 'rpc-protocol:rpc-error
             :message "streamable HTTP transport has no :url"
             :code rpc-protocol:+internal-error+)))

(defun %post (transport method params &key timeout id notify)
  (unless http-protocol:*http-backend*
    (error 'rpc-protocol:rpc-error
           :message "*http-backend* is nil — bind an http-protocol backend"
           :code rpc-protocol:+internal-error+))
  (let* ((url (%ensure-url transport))
         (body (if notify
                   (rpc-protocol:encode-notification method params)
                   (rpc-protocol:encode-request method params :id id)))
         (res (apply #'http:post url
                     :content body
                     :headers (%mcp-headers transport method params)
                     (when timeout (list :timeout timeout))))
         (status (http-protocol:response-status res))
         (ctype (http-protocol:response-header res "content-type"))
         (text (%body-string res)))
    (%maybe-store-session transport res)
    (cond
      ((<= 200 status 299)
       (if notify
           t
           (let ((result (%raise-rpc (%decode-body text ctype))))
             (%maybe-store-protocol-version transport method result)
             result)))
      (t
       (let ((msg (ignore-errors (%decode-body text ctype))))
         (if (and msg (hash-table-p msg) (gethash "error" msg))
             (%raise-rpc msg)
             (error 'rpc-protocol:rpc-error
                    :message (format nil "HTTP ~a~@[ ~a~]" status
                                     (and (plusp (length text)) text))
                    :code rpc-protocol:+internal-error+)))))))

(defmethod rpc-protocol:backend-rpc-call
    ((transport streamable-http-rpc-transport) method params &key timeout id)
  (%post transport method params
         :timeout timeout
         :id (or id (incf (transport-next-id transport)))))

(defmethod rpc-protocol:backend-rpc-notify
    ((transport streamable-http-rpc-transport) method params)
  (%post transport method params :notify t)
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
      (mcp-protocol:mcp-initialize client)
      (let ((ver (mcp-protocol:mcp-client-protocol-version client))
            (transport (mcp-protocol:mcp-client-transport client)))
        (when (stringp ver)
          (setf (transport-protocol-version transport) ver))))
    client))

(defmethod mcp-protocol:backend-mcp-serve
    ((backend streamable-http-mcp-backend) server
     &key (host "127.0.0.1") (port 8080) (path "/"))
  (%ensure-http-server)
  (http-server-protocol:serve (make-mcp-app server :path path)
                              :host host :port port))

(use-streamable-http-mcp-backend)
