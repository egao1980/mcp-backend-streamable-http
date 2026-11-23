(in-package #:mcp-backend-streamable-http)

;;; 0.4.0 (egao1980/mcp-backend-streamable-http#9; cl-stack-code H3e-2a), the
;;; client side a host needs:
;;;
;;; Headers. BACKEND-MCP-CONNECT and MAKE-STREAMABLE-HTTP-RPC-TRANSPORT take
;;;   :HEADERS, an alist (name . value) or a hash table name → value, strings
;;;   (e.g. "Authorization" "Bearer …" from a host's config, any ${VAR} already
;;;   expanded by the host). They go on every POST after the MCP headers;
;;;   one that names an MCP header (Content-Type, Accept, MCP-Protocol-Version,
;;;   Mcp-Method, Mcp-Name, Mcp-Session-Id; any case) is dropped, so an MCP
;;;   header cannot be overridden. A name or value that is not a string →
;;;   TYPE-ERROR at construction.
;;;
;;; Messages the server sends. An SSE response (Content-Type text/event-stream)
;;;   is read event by event, in order. A notification (method, no id) goes to
;;;   the transport's ON-NOTIFICATION; a request (method and id) to ON-REQUEST,
;;;   whose result, or RPC-ERROR (code, message, data; any other ERROR → -32603;
;;;   no ON-REQUEST → -32601), is POSTed back as the JSON-RPC response (the
;;;   streamable HTTP way to answer a server; the reply's HTTP answer is not
;;;   read further). The response whose id is the call's is the result; the
;;;   notifications and requests before it are handled first. BACKEND-MCP-CONNECT
;;;   sets both to MCP-PROTOCOL:MCP-CLIENT-TRANSPORT-HANDLERS (list_changed,
;;;   log, progress, ping, roots, sampling, elicitation reach the client's
;;;   handlers, which it takes as for mcp-backend-stdio: :ROOTS
;;;   :SAMPLING-HANDLER :ELICITATION-HANDLER :LOG-HANDLER :PROGRESS-HANDLER
;;;   :ON-TOOLS-CHANGED :NOTIFICATION-HOOK). Errors in a notification handler
;;;   are dropped. The body is read whole before it is handled, so a server
;;;   that waits for the answer to its request before it ends the stream is
;;;   not served; there is no GET stream (notifications outside a request do
;;;   not arrive).
;;;
;;; Legacy HTTP+SSE (2024-11-05: a GET event stream that names a POST
;;;   endpoint). Before the first successful response, an answer that shows
;;;   that transport signals MCP-TRANSPORT-UNSUPPORTED (an MCP-PROTOCOL:MCP-ERROR,
;;;   so MCP-INITIALIZE's discover fallback and %RPC-CALL pass it through
;;;   unchanged): HTTP 405 (the endpoint exists but takes no POST: the legacy
;;;   GET stream), or a 2xx SSE body whose first event is "endpoint". Readers:
;;;   MCP-TRANSPORT-UNSUPPORTED-URL, -TRANSPORT (:SSE), -STATUS. A host reports
;;;   it per server ("transport sse is not supported yet"). Other HTTP errors
;;;   stay RPC-ERRORs as before.

(define-condition mcp-transport-unsupported (mcp-protocol:mcp-error)
  ((url :initarg :url :initform nil :reader mcp-transport-unsupported-url)
   (transport :initarg :transport :initform :sse :reader mcp-transport-unsupported-transport)
   (status :initarg :status :initform nil :reader mcp-transport-unsupported-status))
  (:default-initargs :code nil)
  (:report (lambda (c s)
             (format s "~@[~a: ~]the server speaks the legacy HTTP+SSE transport (2024-11-05), which is not supported~@[ (HTTP ~a)~]"
                     (mcp-transport-unsupported-url c)
                     (mcp-transport-unsupported-status c))))
  (:documentation "The server at URL answered as a legacy HTTP+SSE (2024-11-05)
server, not a streamable HTTP one."))

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
   JSON-RPC notifications (no id) → 202 empty body. Server notifications sent while a
   request is handled (progress, log) precede the response as SSE message events; with
   a plain JSON response they are dropped."
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
        (when (and (not method)
                   (or (nth-value 1 (gethash "result" msg))
                       (nth-value 1 (gethash "error" msg))))
          ;; 0.4.0: a client's answer to a request the server sent. This app
          ;; sends none, so it is accepted and dropped.
          (return-from app '(202 () (""))))
        (unless method
          (return-from app
            (list 400 '(:content-type "application/json; charset=utf-8")
                  (list (rpc-protocol:encode-error-response
                         rpc-protocol:+invalid-request+ "missing method" :id id)))))
        (let ((mismatch (%check-mcp-headers env method params id)))
          (when mismatch
            (return-from app mismatch)))
        (handler-case
            (let* ((sse (%wants-sse env))
                   (notes '())
                   (result (let ((mcp-protocol:*mcp-notifier*
                                   (lambda (note-method note-params)
                                     (when sse
                                       (push (rpc-protocol:encode-notification note-method note-params)
                                             notes)))))
                             (mcp-protocol:dispatch-mcp-method server method params))))
              (cond
                ((not id-present)
                 '(202 () ("")))
                (sse
                 (list 200
                       '(:content-type "text/event-stream; charset=utf-8"
                         :cache-control "no-cache")
                       (mapcar (lambda (data)
                                 (sse-protocol:encode-sse-event
                                  (sse-protocol:make-sse-event :event "message" :data data)))
                               (append (reverse notes)
                                       (list (rpc-protocol:encode-response result :id id))))))
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
   (next-id :initform 0 :accessor transport-next-id)
   ;; 0.4.0
   (headers :initarg :headers :initform '() :reader transport-headers
            :documentation "Extra request headers, an alist of strings (MCP headers removed).")
   (on-notification :initarg :on-notification :initform nil
                    :accessor transport-on-notification
                    :documentation "(lambda (method params)) for notifications in an SSE response.")
   (on-request :initarg :on-request :initform nil :accessor transport-on-request
               :documentation "(lambda (method params)) → result, for requests in an SSE response.")
   (established :initform nil :accessor transport-established
                :documentation "True after the first 2xx response.")))

(defparameter +mcp-header-names+
  '("content-type" "accept" "mcp-protocol-version" "mcp-method" "mcp-name" "mcp-session-id")
  "Headers the transport sets itself; :HEADERS cannot override them.")

(defun %normalize-headers (headers)
  "HEADERS (alist or hash table of strings) → an alist, MCP headers removed, in order."
  (let ((pairs (cond ((null headers) '())
                     ((hash-table-p headers)
                      (loop for k being the hash-keys of headers using (hash-value v)
                            collect (cons k v)))
                     ((listp headers) headers)
                     (t (error 'type-error :datum headers :expected-type '(or list hash-table))))))
    (loop for pair in pairs
          do (check-type pair cons)
             (check-type (car pair) string)
             (check-type (cdr pair) string)
          unless (member (car pair) +mcp-header-names+ :test #'string-equal)
            collect (cons (car pair) (cdr pair)))))

(defun make-streamable-http-rpc-transport
    (&key url (protocol-version mcp-protocol:+mcp-protocol-version+)
       (mcp-name "cl-stack-mcp") headers on-notification on-request)
  "A client transport POSTing to URL. HEADERS (0.4.0): extra request headers.
ON-NOTIFICATION / ON-REQUEST (0.4.0): (lambda (method params)) for the messages
the server sends in an SSE response."
  (make-instance 'streamable-http-rpc-transport
                 :url url
                 :protocol-version protocol-version
                 :mcp-name mcp-name
                 :headers (%normalize-headers headers)
                 :on-notification on-notification
                 :on-request on-request))

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

(defun %sse-content-type-p (content-type)
  (and (stringp content-type)
       (search "text/event-stream" content-type :test #'char-equal)))

(defun %sse-events (body)
  "The events of the SSE text BODY, in order."
  (with-input-from-string (in body)
    (sse-protocol:collect-sse-events in)))

(defun %decode-body (body content-type)
  "The (last) JSON-RPC message of BODY. For error answers only; a 2xx SSE body
goes through %HANDLE-SSE-BODY."
  (if (%sse-content-type-p content-type)
      (let ((ev (car (last (%sse-events body)))))
        (rpc-protocol:decode-message (sse-protocol:sse-event-data ev)))
      (rpc-protocol:decode-message body)))

(defun %legacy-endpoint-event-p (events)
  (let ((first (first events)))
    (and first (equal "endpoint" (sse-protocol:sse-event-type first)))))

(defun %unsupported (transport status)
  (error 'mcp-transport-unsupported
         :url (princ-to-string (transport-url transport))
         :status status
         :message (format nil "~a: the server speaks the legacy HTTP+SSE transport (2024-11-05), which is not supported (HTTP ~a)"
                          (transport-url transport) status)))

(defun %answer-server-request (transport method params id)
  "Run ON-REQUEST for the server's request and POST the response back."
  (let* ((handler (transport-on-request transport))
         (wire (handler-case
                   (if handler
                       (rpc-protocol:encode-response (funcall handler method params) :id id)
                       (rpc-protocol:encode-error-response
                        rpc-protocol:+method-not-found+
                        (format nil "method not found: ~a" method) :id id))
                 (rpc-protocol:rpc-error (c)
                   (rpc-protocol:encode-error-response
                    (or (rpc-protocol:rpc-error-code c) rpc-protocol:+internal-error+)
                    (or (rpc-protocol:rpc-error-message c) "rpc error")
                    :id id :data (rpc-protocol:rpc-error-data c)))
                 (error (c)
                   (rpc-protocol:encode-error-response
                    rpc-protocol:+internal-error+ (princ-to-string c) :id id)))))
    ;; The reply's own HTTP answer (202) carries nothing for us; a failure to
    ;; deliver it must not cost the call it arrived in.
    (ignore-errors (%post-wire transport method wire))))

(defun %handle-sse-body (transport body id)
  "Route every message of the SSE BODY in order; → the response message for ID
(or the last response when none carries ID), or NIL."
  (let ((response nil) (last-response nil))
    (dolist (ev (%sse-events body))
      (let* ((data (sse-protocol:sse-event-data ev))
             (msg (and data (plusp (length data)) (ignore-errors (rpc-protocol:decode-message data)))))
        (when (hash-table-p msg)
          (multiple-value-bind (method method-p) (gethash "method" msg)
            (multiple-value-bind (msg-id id-p) (gethash "id" msg)
              (cond
                ((and method-p (stringp method) (not id-p))
                 (let ((fn (transport-on-notification transport)))
                   (when fn
                     (ignore-errors (funcall fn method (gethash "params" msg))))))
                ((and method-p (stringp method))
                 (%answer-server-request transport method (gethash "params" msg) msg-id))
                (t
                 (setf last-response msg)
                 (when (and id-p (equal msg-id id) (null response))
                   (setf response msg)))))))))
    (or response last-response)))

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
    ;; 0.4.0: the caller's headers after the MCP ones (none of them an MCP header).
    (append headers (transport-headers transport))))

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

(defun %ensure-http-backend ()
  (unless http-protocol:*http-backend*
    (error 'rpc-protocol:rpc-error
           :message "*http-backend* is nil — bind an http-protocol backend"
           :code rpc-protocol:+internal-error+)))

(defun %post-wire (transport method wire &key params timeout)
  "POST the JSON text WIRE with the MCP and caller headers. → the response."
  (%ensure-http-backend)
  (apply #'http:post (%ensure-url transport)
         :content wire
         :headers (%mcp-headers transport method params)
         (when timeout (list :timeout timeout))))

(defun %post (transport method params &key timeout id notify)
  (let* ((body (if notify
                   (rpc-protocol:encode-notification method params)
                   (rpc-protocol:encode-request method params :id id)))
         (res (%post-wire transport method body :params params :timeout timeout))
         (status (http-protocol:response-status res))
         (ctype (http-protocol:response-header res "content-type"))
         (text (%body-string res)))
    (%maybe-store-session transport res)
    (unless (transport-established transport)
      (when (or (eql status 405)
                (and (<= 200 status 299) (%sse-content-type-p ctype)
                     (%legacy-endpoint-event-p (ignore-errors (%sse-events text)))))
        (%unsupported transport status)))
    (cond
      ((<= 200 status 299)
       (setf (transport-established transport) t)
       (cond
         (notify t)
         (t
          (let* ((msg (if (%sse-content-type-p ctype)
                          (%handle-sse-body transport text id)
                          (rpc-protocol:decode-message text)))
                 (result (if (hash-table-p msg)
                             (%raise-rpc msg)
                             (error 'rpc-protocol:rpc-error
                                    :message (format nil "no JSON-RPC response to ~a" method)
                                    :code rpc-protocol:+internal-error+))))
            (%maybe-store-protocol-version transport method result)
            result))))
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
    ((backend streamable-http-mcp-backend) &key url headers
                                             (era :unknown) (probe nil)
                                             name version
                                             protocol-version
                                             roots sampling-handler elicitation-handler
                                             log-handler progress-handler
                                             on-tools-changed notification-hook)
  (let ((client (make-instance 'mcp-protocol:mcp-client
                               :era era
                               :name (or name "cl-stack-mcp")
                               :version (or version "0.1.0")
                               :protocol-version
                               (or protocol-version
                                   mcp-protocol:+mcp-protocol-version+)
                               :roots roots
                               :sampling-handler sampling-handler
                               :elicitation-handler elicitation-handler
                               :log-handler log-handler
                               :progress-handler progress-handler
                               :on-tools-changed on-tools-changed
                               :notification-hook notification-hook)))
    (multiple-value-bind (on-notification on-request)
        (mcp-protocol:mcp-client-transport-handlers client)
      (setf (mcp-protocol:mcp-client-transport client)
            (make-streamable-http-rpc-transport
             :url url
             :protocol-version (or protocol-version mcp-protocol:+mcp-protocol-version+)
             :mcp-name (or name "cl-stack-mcp")
             :headers headers
             :on-notification on-notification
             :on-request on-request)))
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
