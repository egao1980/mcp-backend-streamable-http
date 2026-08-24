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

## License

MIT
