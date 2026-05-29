---
title: "Phase 14. Goroutine bridge"
sidebar_position: 16
sidebar_label: "Phase 14. Goroutine bridge"
description: "MEP-74 Phase 14 lands the cgo handle pool that lets cross-tier `chan T` and `func(...)` callback values survive a yield across the cgo boundary. The bridge ships a real concurrent-safe `HandlePool` plus a renderer that emits the canonical `mochi_rt.go` runtime file every wrapper package with channels or callbacks must include, plus per-channel and per-callback shim generators that produce the cgo-export surface (`_new` / `_send` / `_recv` / `_close` for channels; `_call` / `_release` for callbacks). The end-to-end sentinel renders the runtime + shims into a scratch wrapper package and compiles it via `go build -tags=mochi_wrap`."
---

# Phase 14. Goroutine bridge

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED (baseline) |
| Started        | 2026-05-30 00:30 (GMT+7) |
| Landed         | 2026-05-30 00:50 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase14GoroutineBridgeSentinel` in
`package3/go/goroutine/phase14_test.go` writes the rendered runtime plus a representative channel-shim and callback-shim into a scratch wrapper package and asserts:

- `go build -tags=mochi_wrap ./...` compiles the runtime + shim files cleanly (no cgo binary needed: the generated code imports the regular `runtime/cgo` stdlib package and uses no `C.` symbols);
- `go build ./...` (without the build tag) is also a clean compile (the generated file is gated by `//go:build mochi_wrap` so it disappears under the default tag set, leaving an empty package);
- `RenderRuntime`, `RenderChannelShim`, `RenderCallbackShim` are byte-deterministic across 10 back-to-back calls (the SHA-256 stays constant), which is load-bearing for the phase 10 `wrapper-sha256` lockfile pin;
- the rendered runtime declares exactly the three `mochi*Handle` helpers (`mochiAcquireHandle`, `mochiResolveHandle`, `mochiReleaseHandle`) and no extras.

Plus `TestPhase14HandlePoolRoundTripIntegration` exercises the real `HandlePool` in-process: a goroutine sends `42` on a `chan int64` whose handle ID was acquired from the pool, a peer goroutine resolves the same ID and receives `42`, and a `Release(id)` brings `Live()` back to 0.

Plus 22 unit tests in `goroutine_test.go`:

- handle pool (`TestHandlePoolAcquireResolveRelease`,
  `TestHandlePoolAcquireRespectsMaxHandles`,
  `TestHandlePoolAcquireUnboundedWhenMaxNegative`,
  `TestHandlePoolResolveMissReturnsFalse`,
  `TestHandlePoolReleaseUnknownReturnsFalse`,
  `TestHandlePoolConcurrentAcquireRelease`),
- `NeedsRuntime` predicate
  (`TestNeedsRuntimeDetectsChannelInFuncParam`,
  `TestNeedsRuntimeDetectsChannelInFuncResult`,
  `TestNeedsRuntimeDetectsCallback`,
  `TestNeedsRuntimeDetectsChannelNestedInSlice`,
  `TestNeedsRuntimeDetectsCallbackInMapValue`,
  `TestNeedsRuntimeFalseForPureSync`,
  `TestNeedsRuntimeTreatsParseErrorAsTrue`,
  `TestNeedsRuntimeDetectsChannelInMethod`,
  `TestNeedsRuntimeDetectsCallbackInInterfaceMethod`),
- runtime renderer
  (`TestRenderRuntimeContainsCanonicalHeader`,
  `TestRenderRuntimeFallsBackToDefaultPackageName`,
  `TestRenderRuntimeIsByteDeterministic`),
- channel shim renderer
  (`TestRenderChannelShimRequiresElemGoType`,
  `TestRenderChannelShimRequiresModuleFlatName`,
  `TestRenderChannelShimCoversTheFourSymbols`,
  `TestRenderChannelShimDefaultsSymbolBaseAndBuffer`),
- callback shim renderer
  (`TestRenderCallbackShimMatchesSignature`,
  `TestRenderCallbackShimNoResults`,
  `TestRenderCallbackShimMultipleResults`,
  `TestRenderCallbackShimRejectsBadSignature`,
  `TestRenderCallbackShimRequiresFields`).

## Lowering decisions

The goroutine package is layering-conservative: it imports `package3/go/apisurface` for the surface walk that powers `NeedsRuntime` and otherwise depends only on the Go stdlib (`runtime/cgo`, `sync`, `strings`, `fmt`, `errors`). The package splits into three concerns: a real `HandlePool` (the runtime stand-in for the generated code), a renderer that produces the wrapper's `mochi_rt.go` file, and per-channel / per-callback shim generators.

**The `HandlePool` is a real runtime object, not just a renderer's stand-in.** The bridge needs a Go-side pool object at user-machine runtime to mint, resolve, and release handle IDs when the wrapper-synth pipeline emits a `chan T` or callback shim. The pool is concurrent-safe (a `sync.Mutex` over a `map[uint64]cgo.Handle`), leak-free (`Release` calls `cgo.Handle.Delete` so the GC can reclaim the value), and bounds-checked against a caller-supplied soft cap (default 4096, matching the MEP-74 spec `goroutine-bridge.max-handles` setting). The mutex is fine-grained over the map only; the underlying `cgo.NewHandle` / `cgo.Handle.Delete` calls happen outside the critical section so a `Release` of one ID cannot block an `Acquire` of another.

**The renderer emits a `//go:build mochi_wrap` runtime file.** The generated file is scoped to the `mochi_wrap` build tag so a downstream consumer that imports the wrapper outside a cgo c-archive build (e.g. `go vet` over the source tree) does not accidentally pull in the `runtime/cgo` dependency. Phase 9 (build orchestration) sets the build tag when invoking `go build -tags=mochi_wrap`. The sentinel verifies both forms compile (with tag → runtime visible; without tag → empty package; both pass `go build`).

**The renderer is byte-deterministic.** Output is built up via `strings.Builder` with no map iteration, no time-of-day, no random IDs. The sentinel hashes the output 10 times and asserts the SHA-256 is constant, because the phase 10 lockfile pins the wrapper-sha256 and a non-deterministic renderer would force every `mochi pkg lock --check` to fail spuriously.

**The channel-as-handle surface is four cgo-export symbols.** For each exported `chan T` the wrapper needs, the renderer emits `mochi_<module>_<base>_chan_new(buf)` (returns the handle ID), `_send(id, v)` (blocking send), `_recv(id) (v, ok)` (blocking recv; `ok==false` on closed+drained), and `_close(id)` (closes + releases). The four-symbol surface is exactly what the Mochi-side extern fn emitter (phase 7) binds against, and it covers the full `chan T` semantic surface Mochi consumes (Mochi has `stream<T>` for receive-only channels, but the cgo-export side stays bidirectional so a single wrapper covers both send-only and recv-only Mochi-side aliases).

**The callback-as-handle surface is two symbols.** For each exported callback parameter type, the renderer emits `mochi_<module>_<base>_cb_call(id, args...)` (invokes the callback) and `_release(id)` (deletes the handle). The cgo C side acquires a fresh ID before passing the callback across the boundary; the Go side just resolves the ID on each call and returns the result. This is the inverse direction of the channel surface: channels are Go-owned (the Go wrapper creates the channel), callbacks are Mochi-owned (Mochi owns the function value; the wrapper just invokes it on the Go side via the resolved handle).

**`NeedsRuntime` is conservative-positive.** The walk treats parse failures as `true` so the wrapper synthesiser fails safe: an erroneous skip would leave a wrapper without the runtime file the cgo build expects, breaking the link with a low-signal "undefined symbol" error. Phase 4 (apisurface parser) catches the syntactic error separately. Pure-sync surfaces (no `chan`, no callback param or result, no exported method with either in its signature) skip the runtime entirely and pay zero `cgo.Handle` cost.

**The handle pool is per-package, not global.** Each generated wrapper package owns its own `mochiHandles` map (declared in the rendered file via `var`). A module-A leak cannot exhaust a module-B pool. The runtime-side `HandlePool` mirrors this: callers instantiate one per wrapper, so the tests can run two pools side-by-side without interference.

**The channel shim has an opinionated buffer default.** `BufferSize` defaults to 1 if unset, mirroring the MEP-74 spec `goroutine-bridge.default-buffer = 1` setting. The reason MEP-74 picks 1 rather than 0 (true Go-unbuffered) is to avoid lock-step semantics across the cgo boundary: a Mochi-side `stream.send(v)` followed immediately by `stream.send(w)` on the same goroutine would deadlock if the Go-side receive is delayed by even one runtime scheduling tick. Phase 14.1 will let callers override per-channel via the `mochi.toml` `[go.channels.<module>.<base>]` table.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/goroutine/goroutine.go` | `ErrGoroutine`, `DefaultMaxHandles`, `HandlePool`, `NewHandlePool`, `Acquire`/`Resolve`/`Release`/`Live`, `NeedsRuntime`, `RenderRuntime`, `ChannelShim` + `RenderChannelShim`, `CallbackShim` + `RenderCallbackShim`. |
| `package3/go/goroutine/goroutine_test.go` | 22 unit tests covering pool semantics, `NeedsRuntime` predicate, and all three renderers. |
| `package3/go/goroutine/phase14_test.go` | `TestPhase14GoroutineBridgeSentinel` (compiles rendered output via `go build -tags=mochi_wrap`) + `TestPhase14HandlePoolRoundTripIntegration` (in-process goroutine round-trip through a pool-resolved channel). |
| `website/docs/implementation/0074/phase-14-goroutine-bridge.md` | (this page) |

## Test set

- `TestPhase14GoroutineBridgeSentinel`
- `TestPhase14HandlePoolRoundTripIntegration`
- 22 unit tests in `goroutine_test.go`

Local run on darwin-arm64:

```
$ go test ./package3/go/goroutine/...
ok      mochi/package3/go/goroutine     6.962s
$ go test ./package3/go/...
ok      mochi/package3/go/apisurface    (cached)
ok      mochi/package3/go/build (cached)
ok      mochi/package3/go/cmd/go-ingest (cached)
ok      mochi/package3/go/cosign        (cached)
ok      mochi/package3/go/emit  (cached)
ok      mochi/package3/go/errors        (cached)
ok      mochi/package3/go/goroutine     3.184s
ok      mochi/package3/go/library       (cached)
ok      mochi/package3/go/lockfile      (cached)
ok      mochi/package3/go/moduleproxy   (cached)
ok      mochi/package3/go/publish       (cached)
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/wrapper       (cached)
```

## Closeout notes

Phase 14 lands the cgo-handle pool plus the runtime + shim renderers as a leaf module. The wrapper-synthesiser integration (calling `NeedsRuntime` + dropping the rendered files into the wrapper output) is wired into phase 6's deferred sub-phases 6.1+. Phase 14 ships standalone so phase 10 (lockfile) can already pin a stable `wrapper-sha256` for any in-test wrapper that exercises the runtime file.

Future phase 14.x reservations:

- **14.1** Per-channel `BufferSize` overrides via `mochi.toml` `[go.channels.<module>.<base>]` table.
- **14.2** `select` desugar: emit a multi-channel `_select(ids...)` shim for wrappers that need the Go-side `select` statement (the present surface is single-channel `_recv` only).
- **14.3** Backpressure plumbing: the Mochi-side `stream<T>` surface needs a `try_send` / `try_recv` (non-blocking) variant for the agent-pipeline fast path; phase 14 only emits the blocking surface.
- **14.4** Wrapper-synthesis integration (phase 6.1) that ties `NeedsRuntime` + the three renderers into the per-module wrapper output.
- **14.5** Live-cgo end-to-end gate: build a real `c-archive` from the rendered wrapper, link it into a C test driver, and verify a cross-boundary channel round-trip with the actual cgo runtime path (today's sentinel uses `runtime/cgo` only, no `C.` symbols).

Phase 15 (monomorphisation) consumes the same `NeedsRuntime` signal: a per-instantiation wrapper that exposes a `chan T` over a monomorphised generic needs the same handle pool the phase 14 surface emits, just one renderer call per instantiation.
