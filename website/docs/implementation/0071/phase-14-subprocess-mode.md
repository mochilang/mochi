---
title: MEP-71 Phase 14 (Subprocess runtime mode)
sidebar_position: 15
sidebar_label: "Phase 14. Subprocess mode"
description: "MEP-71 Phase 14: JSON-RPC 2.0 stdio protocol + Python worker source renderer for the [python].runtime-mode = \"subprocess\" runtime."
---

# MEP-71 Phase 14. Subprocess runtime mode

Status: **LANDED (pending merge)** as of 2026-05-30 00:58 (GMT+7). Implements the protocol + renderer layer of the subprocess runtime: a CPython worker process the Mochi host talks to over JSON-RPC 2.0 on stdio, instead of linking libpython into the host binary. Selected by `[python].runtime-mode = "subprocess"` in `mochi.toml`.

The trade-off vs the embedded mode (Phase 8.2): the subprocess pays ~50us of IPC + JSON round-trip on every call but avoids host-process GIL contention, gets full isolation against C extension crashes, and runs on platforms where a CPython development library is not available at host build time.

## Gate

The umbrella sentinel `TestPhase14SubprocessRuntime` in `package3/python/subproc/phase14_test.go` is green. The sentinel:

- Wires a `Client` to an in-process `Server` over a pair of `io.Pipe`s (identical transport shape to the real subprocess; no actual fork).
- A typed handler simulates an imported Python surface: `add(int, int) -> int` returns the sum; `fail()` returns an `*RPCError` with `CodeInvalidParams` + a structured Data payload.
- `Client.Call("add", [3, 4])` returns the literal `7`.
- `Client.Call("fail", nil)` surfaces the `*RPCError` to the caller with the same code, message, and data the handler attached.
- `Client.Call("unknown", nil)` returns `CodeMethodNotFound` (the handler's fall-through).
- Closing the client + waiting for the server goroutine to drain terminates cleanly without leaking goroutines.
- `RenderWorker` produces a Python source file that embeds the same error code constants the host expects (-32700 / -32601 / -32603); the wire-level codes are the contract between the Go host and the Python worker.

Plus 35 unit tests (`go test ./package3/python/subproc/... -count=1`) covering:

- Protocol: `NewRequest` / `NewResponse` / `NewErrorResponse` shape + Params/Result/Data marshalling; `RPCError.Error` string handling (nil-safe); JSON round-trip; standard JSON-RPC 2.0 error codes (-32700 / -32600 / -32601 / -32602 / -32603) are wired exactly.
- Framing: newline-append on write; embedded-newline rejection; CRLF stripping on read (Windows pipe survival); EOF vs truncated-frame distinction (`io.EOF` vs `io.ErrUnexpectedEOF`); 128 KiB payload round-trip; `MaxFrameSize` cap on write.
- Codec: write-then-read round-trip for Request + Response; rejection of `jsonrpc != "2.0"` on write + read; missing-method rejection on `ReadRequest`; malformed-JSON rejection.
- Client: happy-path Call; `*RPCError` propagation via `errors.As`; generic error -> `CodeInternalError` mapping by the server; sequential ID assignment across multiple Calls; `Call` after `Close` rejects.
- Server: nil-handler rejection; parse-error response (-32700) on malformed input; `*RPCError` from handler surfaces verbatim; success round-trip carries the right `id` + `result`.
- Worker source: `WorkerOptions.Validate` (empty import / empty methods / non-identifier method names rejected); sync variant ships `_main()` + `_dispatch(req)`; async variant ships `async def _main()` + `await _METHODS[method](*params)` + `asyncio.run(_main())`; module-import trailing-newline normalisation.

## Files

- `package3/python/subproc/doc.go` — package overview (embedded vs subprocess trade-off, sub-phase decomposition).
- `package3/python/subproc/protocol.go` — `Request`, `Response`, `RPCError`, `JSONRPCVersion`, standard error codes, `NewRequest` / `NewResponse` / `NewErrorResponse` builders.
- `package3/python/subproc/frame.go` — `WriteFrame`, `ReadFrame`, `MaxFrameSize`, CRLF-tolerant line reader.
- `package3/python/subproc/codec.go` — `Codec` (framing + marshalling on top of `io.Reader` / `io.Writer`).
- `package3/python/subproc/client.go` — `Client.Call`, serialised request/response, `Client.Close`.
- `package3/python/subproc/server.go` — `Handler` signature + `ServeCodec` dispatch loop.
- `package3/python/subproc/worker.go` — `WorkerOptions` + `RenderWorker` (sync + async variants).
- `package3/python/subproc/protocol_test.go` — 9 cases.
- `package3/python/subproc/frame_test.go` — 8 cases.
- `package3/python/subproc/codec_test.go` — 6 cases.
- `package3/python/subproc/client_test.go` — 5 cases (over `io.Pipe`-backed `Server`).
- `package3/python/subproc/server_test.go` — 4 cases.
- `package3/python/subproc/worker_test.go` — 4 cases.
- `package3/python/subproc/phase14_test.go` — Phase 14 umbrella sentinel.

## Sub-phase decomposition

Phase 14 ships the offline protocol + renderer. Live `os/exec` spawn, request pipelining, and the user-facing CLI verb are deferred so the umbrella gate stays deterministic.

| Sub-phase | Title | Status | Notes |
|-----------|-------|--------|-------|
| 14 | JSON-RPC 2.0 protocol + Codec + Client / Server + worker source renderer | LANDED (pending merge) | This PR. |
| 14.1 | Live `os/exec` spawn + stderr forwarding + worker lifetime management | NOT STARTED | Wraps `Client` around an `*exec.Cmd`; surfaces worker stderr to the Mochi error reporter. |
| 14.2 | Request pipelining (multiple in-flight requests demultiplexed by ID) | NOT STARTED | Splits the read loop into a background goroutine + per-request response channels. |
| 14.3 | `mochi pkg run --runtime=subprocess` CLI verb + `[python].runtime-mode` config dispatch | NOT STARTED | Wires the worker behind the unified `mochi pkg` CLI. |
| 14.4 | Mixed sync + async worker surfaces | NOT STARTED | One module, both sync and `async def` callables; today the renderer picks per-worker. |

## Fixtures

Phase 14 is protocol-only; the fixture corpus is not exercised. Sub-phase 14.1 will round-trip every async fn from `httpx`, `aiohttp`, `fastapi` through a real `python3 -m mochi_worker` invocation and assert byte-equal results vs the embedded-mode baseline.

## Skip count

N/A. Phase 14 has no `SkipReport` surface; protocol violations surface as JSON-RPC errors (`-32700` parse / `-32600` invalid request / `-32601` method not found / `-32602` invalid params / `-32603` internal), not skip reports.

## Cross-references

- [MEP-71 spec §9 "Subprocess runtime mode"](/docs/mep/mep-0071) for the normative protocol + lifetime rules.
- [Phase 8](./phase-08-build) for the embedded runtime mode (sub-phase 8.2 cgo libpython link).
- [Phase 12](./phase-12-async-bridge) for the in-process async shim renderer the subprocess worker mirrors at the wire layer.
- [JSON-RPC 2.0 spec](https://www.jsonrpc.org/specification) for the protocol envelope + reserved error code ranges.
- [LSP base protocol](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#baseProtocol) for the stdio framing precedent (we use the simpler newline-delimited variant since payloads are small).
