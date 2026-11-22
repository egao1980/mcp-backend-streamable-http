# mcp-backend-streamable-http

Streamable HTTP backend for [`mcp-protocol`](https://github.com/egao1980/mcp-protocol). POST JSON-RPC; `Accept: text/event-stream` wraps the JSON body as an SSE `message` event. GET is 405.

Client POSTs send `MCP-Protocol-Version`, `Mcp-Method`, and `Mcp-Name`.
`Mcp-Name` is the tool/prompt name or resource URI for `tools/call` /
`prompts/get` / `resources/read`. Legacy servers that return `Mcp-Session-Id`
are echoed on later requests. Dual-era: modern `2026-07-28` and legacy
`2025-11-25`.

Server MUST-gaps:

- `Origin` present and not allowed → **403**. `:allowed-origins` on `make-mcp-app`; default matches `Host`.
- Header/body mismatch → JSON-RPC **`-32020` HeaderMismatch**, HTTP **400**.
- Notification POST (no JSON-RPC `id`) → **202** empty body.
- Server notifications sent while a request runs (`send-progress`, `mcp-log`) come before the response as SSE `message` events when the client accepts `text/event-stream`. With a JSON response they are dropped. There is no GET stream, so notifications outside a request (resource updates) are not delivered over HTTP.

```lisp
(asdf:load-system "mcp-backend-streamable-http")
(mcp-backend-streamable-http:use-streamable-http-mcp-backend)

;; Clack app (no live server required for tests)
(mcp-backend-streamable-http:make-mcp-app
 (make-instance 'mcp-protocol:mcp-server))

;; serve
(mcp-protocol:mcp-serve server :host "127.0.0.1" :port 8080)

;; client
(mcp-protocol:mcp-connect :url "http://127.0.0.1:8080/" :probe t)
```

## Client (0.4.0)

- `:headers` (alist or hash table of strings, e.g. `(("Authorization" . "Bearer …"))`) go on every POST after the MCP headers. A header that names an MCP header (`Content-Type`, `Accept`, `MCP-Protocol-Version`, `Mcp-Method`, `Mcp-Name`, `Mcp-Session-Id`) is dropped.
- In an SSE response, the server's notifications go to the client's handlers (mcp-protocol 0.4.0 `mcp-client-handle-notification`: list_changed, log, progress, the notification hook), in order, before the call returns. The server's requests (ping, `roots/list`, sampling, elicitation) go to `mcp-client-handle-request`, and the answer is POSTed back. `backend-mcp-connect` takes the client's `:roots :sampling-handler :elicitation-handler :log-handler :progress-handler :on-tools-changed :notification-hook`. The body is read whole, so a server that waits for its answer before it ends the stream is not served yet. There is no GET stream either.
- A legacy HTTP+SSE server (2024-11-05) signals `mcp-transport-unsupported` (an `mcp-error`; readers `-url`, `-transport` → `:sse`, `-status`) before the first successful response. That is HTTP 405 to the POST, or an SSE `endpoint` event.

```lisp
(handler-case
    (mcp-protocol:mcp-connect :url "https://mcp.example/mcp"
                              :headers '(("Authorization" . "Bearer tok"))
                              :on-tools-changed (lambda (client) (declare (ignore client)) …)
                              :probe t)
  (mcp-backend-streamable-http:mcp-transport-unsupported (c)
    (format t "~a~%" c)))
```

## License

MIT
