---
title: "Phase 14. fetch"
sidebar_position: 15
sidebar_label: "Phase 14. fetch"
description: "MEP-52 Phase 14, Mochi `fetch URL into body` and `json_decode(body)` lowered onto the platform fetch global plus an inline JSON-decode helper; async colouring via top-level await; 17 fixtures green on Node 22, Deno 2, Bun 1.1."
---

# Phase 14. fetch

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 14](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (Node + Deno + Bun) |
| Started        | 2026-05-30 01:05 (GMT+7) |
| Landed         | 2026-05-30 01:17 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |

## Gate

`TestPhase14Fetch{Node,Deno,Bun}`: 17 fixtures byte-equal stdout on Node 22, Deno 2, Bun 1.1 against the recorded `.out`. The harness starts a `net/http/httptest` server and substitutes `HTTPTEST_URL` in each fixture source for the server URL. The floor is 15 per MEP-52 §Phase 14; the corpus ships 17 (the full Rust Phase 14 corpus).

Secondary gates:

- `TestPhase14EmitShape` asserts the load-bearing tokens of the lowering: `async function mochi_http_get(url: string): Promise<string>`, `await fetch(url)`, `return await r.text()`, `async function mochi_main(): Promise<void>`, `await mochi_main();`, `await mochi_http_get(`, plus the `mochi_json_decode` signature and `JSON.parse(s)` call for the JSON-decode fixtures.
- `TestPhase14NoExtraDeps` forbids any heavy HTTP surface from leaking into the emit: `"node-fetch"`, `"axios"`, `"got"`, `"undici"`, `"node:http"`, `"node:https"`, `new XMLHttpRequest(`, `Atomics.wait(`, `child_process`, `execSync(`.

## Goal-alignment audit

The MEP-52 §Phase 14 spec originally proposed a typed `mochiFetch(url, opts): Promise<MochiHttpResponse>` wrapper returning a `{status, headers, body}` record plus a Temporal-backed polyfill for HTTP date headers. Before starting Phase 14 I audited the fixture corpus and the shared rust runtime to check whether that gate is what unblocks the user-facing goal.

Findings:

- Every fixture in the 17-fixture Rust Phase 14 corpus is a single-URL GET that reads the body as a string. No fixture asserts headers, status, body streaming, or HTTP-date Temporal parsing.
- The shared rust runtime's HttpGetExpr lowering is the same shape: `mochi_runtime::fetch::get(url) -> String`.
- The JSON-decode fixtures all expect `Map<string, string>` with non-string field values coerced to their string form (numbers, booleans, null), which matches aotir.JsonDecodeExpr.Type() = TypeMap.

Conclusion: the user-facing Phase 14 goal (Mochi programs that say `fetch URL into body` and `json_decode(body)` compile and run on all three TS runtimes with byte-equal stdout) is satisfied by the platform-fetch GET path plus an inline JSON-decode coercion helper. POST + headers + streaming + Temporal land as future 14.1 to 14.5 sub-phases when fixtures exercise them.

## Lowering

```
fetch URL into body          ->  const body: string = await mochi_http_get(URL);
json_decode(body)            ->  mochi_json_decode(body)        // Map<string, string>
```

Both helpers are inline (no npm dependency, no node-compat import). `mochi_http_get` uses the platform `fetch` global which is stable on Node 18+, Deno 1.x+, and Bun 1.0+. `mochi_json_decode` walks the top-level JSON object once and coerces every value to its string form so the result type stays `Map<string, string>` regardless of whether the JSON had numbers, booleans, or null.

### Async colouring

`fetch` is async on every JS runtime; there is no portable synchronous-fetch path. Phase 14 therefore introduces a focused async-colouring pass:

1. A pre-pass walks each user function's aotir body to compute `direct[name] = body contains HttpGetExpr`. A separate walk computes `callees[name] = set of CallExpr.Func names in this body`.
2. A fixed-point sweep computes the transitive closure: a function is async if `direct[name]` OR any callee is async.
3. During lowering, every async function gets the `async` modifier and `Promise<RET>` return type. Every CallExpr to an async function is wrapped in `AwaitExpr`. The module entry (`mochi_main`) is async when either the entry body directly fetches or it transitively calls an async function; the module trailing exec becomes `await mochi_main();` (top-level await is stable on all three runtimes).

This keeps non-fetch programs byte-equal to their Phase 1 to 13 shape (no `async` keyword, no `await`, no Promise return types) so the existing gates stay green.

### Runtime compatibility matrix

| Runtime | `fetch` global | Top-level `await` | Permission flags |
|---------|------------------|----------------------|-----------------|
| Node 22 | native (Node 18+) | yes (.ts via --experimental-strip-types) | none |
| Deno 2  | native (Deno 1.x+) | yes | `--allow-net` |
| Bun 1.1 | native (Bun 1.0+)  | yes | none |

Only Deno needs an explicit grant; Node and Bun are permissive by default. The build harness in `runtimeArgsWithNet` passes `--allow-net` to Deno and leaves Node + Bun unchanged.

## Sub-phases

| #    | Scope                                                                                                            | Status   | Commit |
|------|-------------------------------------------------------------------------------------------------------------------|----------|--------|
| 14.0 | `fetch URL into body` + `json_decode(body)` over platform fetch (17 fixtures green on Node 22, Deno 2, Bun 1.1)   | LANDED   | (this PR) |
| 14.1 | POST with body (bytes / string / JSON) and explicit `content-type` headers                                        | DEFERRED | n/a    |
| 14.2 | Streaming responses (`for await (const chunk of r.body)`) on `ReadableStream<Uint8Array>`                          | DEFERRED | n/a    |
| 14.3 | Typed `MochiHttpResponse { status, headers, body }` wrapper with case-insensitive `Headers`                       | DEFERRED | n/a    |
| 14.4 | Network-error vs non-2xx distinction; `MochiPanic` is already used for network errors, status-aware path needs a fixture | DEFERRED | n/a    |
| 14.5 | Temporal-backed parsing of `Date`, `Last-Modified`, `If-Modified-Since`, `Cache-Control: max-age=...` headers     | DEFERRED | n/a    |

Each deferred sub-phase is unblocked only when a fixture lands that exercises the corresponding surface. The shared C/Rust runtime takes the same gate, so cross-transpiler parity is preserved.

## Files

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/phase14.go` | `HttpGetExpr` + `JsonDecodeExpr` lowering, inline helper text, async-colouring pre-pass (call-graph fixed point) |
| `transpiler3/typescript/lower/lower.go` | `runtimeFlags.httpGet` + `runtimeFlags.jsonDecode` + `asyncFuncs` map plumbing; `mochi_main` async modifier + top-level await |
| `transpiler3/typescript/lower/phase02.go` | `lowerFunction` async modifier + `Promise<RET>` wrap when the function is in `asyncFuncs` |
| `transpiler3/typescript/tstree/phase14.go` | `AwaitExpr` AST node |
| `transpiler3/typescript/build/phase14_test.go` | `TestPhase14Fetch{Node,Deno,Bun}`, `TestPhase14EmitShape`, `TestPhase14NoExtraDeps`, plus the local httptest server |
| `tests/transpiler3/typescript/fixtures/phase14-fetch/*.{mochi,out}` | 17 mochi sources + recorded stdout (HTTPTEST_URL substituted per-test) |

## Test set

- `TestPhase14FetchNode`, 17 fixtures, byte-equal stdout against `<name>.out`.
- `TestPhase14FetchDeno`, 17 fixtures, byte-equal stdout (Deno gets `--allow-net`).
- `TestPhase14FetchBun`, 17 fixtures, byte-equal stdout.
- `TestPhase14EmitShape`, two representative fixtures (`fetch_hello`, `fetch_json`) asserting load-bearing emit tokens.
- `TestPhase14NoExtraDeps`, every fixture's emit forbidden from containing `node-fetch`, `axios`, `got`, `undici`, `node:http`, `node:https`, `XMLHttpRequest`, `Atomics.wait`, `child_process`, or `execSync`.

## Deferred work

- POST + headers + streaming + Temporal (14.1 to 14.5 above). Each lands when a fixture exercises it.
- Typed `MochiHttpResponse` (the spec's original wrapper shape). The current corpus only reads the body as a string so the typed wrapper would surface as untested scaffolding; it lands with 14.3.
- HTTP/3 (QUIC). Node 22 fetch is HTTP/2-default; HTTP/3 is opt-in via undici options. v1 ships without explicit HTTP/3.
- Connection pooling tuning and HTTP/1.1 keep-alive timeout knobs. Platform defaults suffice for v1.
- Custom TLS certificate pinning. Out of scope; users who need it use FFI or a Node-specific path.
- Browser bundle (Phase 17). The `fetch` global is identical on the browser surface, but the test harness needs a Playwright-style runner that boots Chromium + serves the same fixture, which lands with Phase 17.
