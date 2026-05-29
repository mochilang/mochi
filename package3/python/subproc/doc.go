// Package subproc implements MEP-71 Phase 14: the subprocess runtime
// mode that lets the Mochi host talk to a CPython worker over JSON-RPC
// 2.0 instead of linking libpython into the host binary (the
// "embedded" mode covered by Phase 8.2).
//
// The trade-off vs embedded:
//
//   - Pro: no libpython link, no host-process GIL contention, full
//     isolation (a SIGSEGV in the Python C extension only kills the
//     worker), and a Mochi host that runs on platforms without a
//     CPython development library at build time.
//
//   - Con: every Mochi -> Python call pays the JSON marshal + IPC
//     round-trip cost (~50us local, ~5us with Linux pipes; >100us if
//     the host process is under load). For tight call loops the
//     embedded mode is faster. For long-running workers (e.g. an ML
//     inference loop where each call drives ~10ms of GPU work) the
//     overhead is in the noise.
//
// The mode is selected by `[python].runtime-mode = "subprocess"` in
// `mochi.toml`. The subproc package ships:
//
//   - Wire-level JSON-RPC 2.0: Request / Response / RPCError + the
//     standard -32700..-32603 error codes.
//   - Newline-delimited framing on stdin/stdout (one JSON message per
//     line; consistent with MCP and LSP-over-stdio practice).
//   - Codec for the framing layer; Client (caller side) + Server
//     (dispatcher side) for the message-loop layer.
//   - The Python worker source renderer that the wrapper synthesiser
//     deposits next to the imported module so `python -m mochi_worker
//     <import-spec>` runs as the subprocess entrypoint.
//
// All IO is io.Reader / io.Writer-driven so tests can wire a Client
// against an in-process Server via io.Pipe and exercise the full
// protocol without ever forking a real subprocess. The real os/exec
// spawn is sub-phase 14.1 (kept offline so the umbrella gate stays
// deterministic).
//
// Layout:
//
//   - protocol.go: Request / Response / RPCError + standard codes.
//   - frame.go: ReadFrame / WriteFrame (newline-delimited).
//   - codec.go: Codec (framing + marshal).
//   - client.go: Client.Call (synchronous request -> response).
//   - server.go: ServeCodec (dispatch loop) + Handler signature.
//   - worker.go: RenderWorker emits the Python worker source.
//
// Cancellation and request multiplexing (one outstanding request at a
// time vs pipelined) are sub-phase 14.2; the live `os/exec` spawn +
// stderr forwarding is sub-phase 14.1; the `mochi pkg run --runtime=
// subprocess` CLI wiring is sub-phase 14.3.
package subproc
