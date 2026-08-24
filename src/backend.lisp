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

(defun %octets-to-string (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(defun %slurp-stream (stream)
  (if (and (open-stream-p stream)
           (ignore-errors
             (let ((et (stream-element-type stream)))
               (and et (subtypep et 'character)))))
      (with-output-to-string (out)
        (loop for c = (read-char stream nil :eof)
              until (eq c :eof)
              do (write-char c out)))
      (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
        (loop for b = (read-byte stream nil :eof)
              until (eq b :eof)
              do (vector-push-extend b bytes))
        (%octets-to-string bytes))))

(defun slurp-env-body (env)
  (let ((raw (getf env :raw-body)))
    (cond
      ((null raw) "")
      ((stringp raw) raw)
      ((and (vectorp raw) (not (stringp raw)))
       (%octets-to-string raw))
      ((streamp raw) (%slurp-stream raw))
      (t (princ-to-string raw)))))

(defun %origin-host (origin)
  (when (and origin (stringp origin) (plusp (length origin)))
    (let* ((after (or (search "://" origin) -3))
           (rest (subseq origin (+ after 3)))
           (slash (or (position #\/ rest) (length rest)))
           (hostport (subseq rest 0 slash)))
      (string-downcase hostport))))

(defun %request-host (env)
  (let ((host (or (%header env "host") (getf env :server-name))))
    (when (and host (stringp host) (plusp (length host)))
      (string-downcase host))))

(defun %origin-allowed-p (env allowed-origins)
  "Missing Origin is OK (non-browser). Present Origin MUST match allowlist or Host."
  (let ((origin (%header env "origin")))
    (cond
      ((or (null origin) (and (stringp origin) (zerop (length origin)))) t)
      (allowed-origins
       (member origin allowed-origins :test #'string-equal))
      (t
       (let ((oh (%origin-host origin))
             (hh (%request-host env)))
         (or (null hh)
             (and oh (or (string-equal oh hh)
                         (string-equal oh (format nil "localhost:~a" (getf env :server-port)))
                         (eql (search hh oh) 0)))))))))

(defun %header-mismatch (id message)
  (list 400
        '(:content-type "application/json; charset=utf-8")
        (list (rpc-protocol:encode-error-response
               mcp-protocol:+mcp-error-header-mismatch+
               message
               :id id))))

(defun %check-mcp-headers (env method params id)
  "HeaderMismatch (-32020) / HTTP 400 when MCP-* disagrees with the JSON-RPC body."
  (let ((h-ver (%header env "mcp-protocol-version"))
        (h-method (%header env "mcp-method"))
        (h-name (%header env "mcp-name"))
        (b-ver (mcp-protocol:param (mcp-protocol:param params "_meta")
                                   "io.modelcontextprotocol/protocolVersion"))
        (b-name (or (mcp-protocol:param params "name")
                    (mcp-protocol:param params "uri"))))
    (cond
      ((and h-ver b-ver (not (string-equal h-ver b-ver)))
       (%header-mismatch id "MCP-Protocol-Version does not match _meta.protocolVersion"))
      ((and h-method method (not (string-equal h-method method)))
       (%header-mismatch id "Mcp-Method does not match JSON-RPC method"))
      ((and h-name b-name (member method '("tools/call" "prompts/get" "resources/read")
                                  :test #'string=)
            (not (string-equal h-name b-name)))
       (%header-mismatch id "Mcp-Name does not match tool/prompt name or resource URI"))
      (t nil))))

(defun make-mcp-app (server &key path allowed-origins)
  "Clack app: POST JSON-RPC. Accept: text/event-stream → SSE message event.
   GET is 405. Origin is validated (403). Header/body mismatch → -32020 / 400.
   JSON-RPC notifications (no id) → 202 empty body."
  (lambda (env)
    (block app
      (when (and path (not (string= (or (getf env :path-info) "/") path)))
        (return-from app
          '(404 (:content-type "text/plain") ("not found"))))
      (unless (eq (getf env :request-method) :post)
        (return-from app
          '(405 (:content-type "text/plain" :allow "POST") ("POST only"))))
      (unless (%origin-allowed-p env allowed-origins)
        (return-from app
          '(403 (:content-type "text/plain") ("forbidden origin"))))
      (let* ((body (slurp-env-body env))
             (msg (rpc-protocol:decode-message body))
             (method (gethash "method" msg))
             (params (or (gethash "params" msg) (mcp-protocol:json-object)))
             (id-present (nth-value 1 (gethash "id" msg)))
             (id (gethash "id" msg)))
        (unless method
          (return-from app
            (list 400 '(:content-type "application/json; charset=utf-8")
                  (list (rpc-protocol:encode-error-response
                         rpc-protocol:+invalid-request+ "missing method" :id id)))))
        (let ((mismatch (%check-mcp-headers env method params id)))
          (when mismatch
            (return-from app mismatch)))
        (handler-case
            (let ((result (mcp-protocol:dispatch-mcp-method server method params)))
              (cond
                ((not id-present)
                 '(202 () ("")))
                ((%wants-sse env)
                 (list 200
                       '(:content-type "text/event-stream; charset=utf-8"
                         :cache-control "no-cache")
                       (list (sse-protocol:encode-sse-event
                              (sse-protocol:make-sse-event
                               :event "message"
                               :data (rpc-protocol:encode-response result :id id))))))
                (t
                 (list 200
                       '(:content-type "application/json; charset=utf-8")
                       (list (rpc-protocol:encode-response result :id id))))))
          (rpc-protocol:rpc-error (c)
            (list (if (eql (rpc-protocol:rpc-error-code c)
                           mcp-protocol:+mcp-error-header-mismatch+)
                      400
                      200)
                  '(:content-type "application/json; charset=utf-8")
                  (list (rpc-protocol:encode-error-response
                         (rpc-protocol:rpc-error-code c)
                         (or (rpc-protocol:rpc-error-message c) "rpc error")
                         :id id :data (rpc-protocol:rpc-error-data c)))))
          (mcp-protocol:mcp-error (c)
            (list 200
                  '(:content-type "application/json; charset=utf-8")
                  (list (rpc-protocol:encode-error-response
                         (or (mcp-protocol:mcp-error-code c)
                             rpc-protocol:+internal-error+)
                         (or (mcp-protocol:mcp-error-message c) "mcp error")
                         :id id :data (mcp-protocol:mcp-error-data c))))))))))

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
