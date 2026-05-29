---
title: "Phase 10. Streams"
sidebar_position: 15
sidebar_label: "Phase 10. Streams"
description: "MEP-51 Phase 10 -- synchronous Mochi broadcast streams lower to a small mochi_runtime.stream surface (MochiStream + MochiSub); each subscriber holds an independent read cursor over a shared append-only buffer; async / cross-task / bounded backpressure deferred to Phase 11."
---

# Phase 10. Streams

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 10](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (10.0 only; 10.1-10.3 DEFERRED to Phase 11) |
| Started        | 2026-05-29 19:09 (GMT+7) |
| Landed         | 2026-05-29 19:39 (GMT+7) |
| Tracking issue | (filled at ship) |
| Tracking PR    | (filled at ship) |

## Gate

`TestPhase10Streams`: 8 fixtures green on CPython 3.12.7 in `transpiler3/python/build/phase10_test.go`. The corpus covers the four scalar element types (`int`, `bool`, `float`, `string`), the broadcast multi-subscriber pattern, the "subscribe after emit" cursor-starts-at-write-position semantics, the "two subs diverge" interleave, and the "emit inside a for loop" producer shape. Each fixture rebuilds from `tests/transpiler3/python/fixtures/phase10-streams/*.mochi`, runs `python -m mochi_user_<name>`, and byte-compares stdout to the matching `.out` file. The full Phase 1-10 regression suite (`go test ./transpiler3/python/... -count=1`) finishes in 12.6s with zero regressions.

## Goal-alignment audit

Mochi streams are the composable data-flow abstraction; without Phase 10 no Mochi program that uses `make_stream`, `subscribe`, `emit`, or `recv_sub` reaches the Python target. Phase 10.0 lands the synchronous broadcast surface that the v1 C-fixture corpus already exercises (`tests/transpiler3/c/fixtures/stream/`), which is exactly the shape every existing Mochi stream program uses today. Async / cross-task broadcast (10.1) requires the Phase 11 async colour pass to mark stream-consuming functions as `async def`; bounded backpressure (10.2) requires `asyncio.Queue(maxsize=N)` with a real producer task; the operator library (10.3) requires both. Landing 10.0 now unblocks the Python target for the current v1 stream surface and leaves the IR shape stable for the async layering Phase 11 will add on top.

The user-facing payload is straightforward: a Mochi `stream<Tick>` becomes a `MochiStream[Tick]` runtime object that any Python consumer can hold, and `recv_sub(sub)` returns the next pending value at the subscriber's cursor. The runtime surface (`mochi_runtime.stream`) is six names: `MochiStream`, `MochiSub`, `mochi_make_stream`, `mochi_subscribe`, `mochi_emit`, `mochi_recv_sub`. All six are stdlib-only and pure Python; the wheel ships them as 60 lines of `runtime/python/mochi_runtime/stream.py`.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 10.0 | Synchronous broadcast: `MochiStream` (append-only buffer) + `MochiSub` (per-subscriber read cursor); `make_stream`, `subscribe`, `emit`, `recv_sub` | LANDED 2026-05-29 | (filled at ship) |
| 10.1 | Async generators (`AsyncIterator[T]`) + `async for` consumption; cross-task producer/consumer split | DEFERRED to Phase 11 | -- |
| 10.2 | Bounded streams via `asyncio.Queue(maxsize=N)` with producer-side `await put` backpressure; SubMakeLimitExpr lowering | DEFERRED to Phase 11 | -- |
| 10.3 | Stream operators (`map`, `filter`, `take`, `zip`, `flat_map`, `collect`) in `mochi_runtime.stream` | DEFERRED to Phase 11 | -- |

## Sub-phase 10.0 -- Synchronous broadcast streams

### Goal-alignment audit (10.0)

The synchronous broadcast surface is what the existing v1 stream fixtures actually exercise. The C target's `stream<T>` is implemented as a ring buffer with per-subscriber cursors, and every existing fixture interleaves `emit` and `recv_sub` on the same execution path (no producer-consumer split, no bounded backpressure). Mirroring that exactly in Python with an append-only list and integer cursors is the smallest correct lowering. It also preserves the broadcast invariant that matters most: a subscriber created at write position k sees emit values at indices `k, k+1, k+2, ...` and nothing before. Phase 11's async layering will replace the list with `asyncio.Queue` per subscriber, but the IR shape stays the same: `StreamMakeExpr`, `SubMakeExpr`, `StreamEmitStmt`, `SubRecvExpr` map one-to-one onto the new emit shape.

Async / spawn / supervised streams are genuinely deferred, not punted. The IR surface for those (Phase 9.2's `SubMakeLimitExpr`, plus Phase 11's still-undefined async-stream nodes) is documented but unused by the current fixture corpus; no Mochi program in `tests/transpiler3/` exercises bounded streams or async consumers today. Phase 11 will land both the runtime support and the goal-aligned fixtures together.

### Decisions made (10.0)

**Runtime surface lives in `mochi_runtime.stream`, not inlined per module.** Inlining the `MochiStream`/`MochiSub` classes inside every generated module would (a) bloat the source tree on multi-stream programs and (b) lose the ability for two generated modules to interoperate over a shared stream value. Shipping a single 60-line runtime module is the smaller-surface choice; the import is gated on `l.needsStream` so non-stream programs do not import it. The runtime uses `__slots__` on both classes to keep allocation cheap and to catch typos at attribute-set time.

**Subscriber cursor starts at the current write position.** `MochiSub.__init__` reads `len(stream._buffer)` at construction time so any value emitted before subscribe is invisible to that subscriber. This matches the C target's behaviour (see `tests/transpiler3/c/fixtures/stream/stream_multi_sub/`: both subs subscribed before any emit, so both see the full sequence; if one subscribed after the first emit, it would miss the first value). The `stream_sub_after_emit` fixture exercises the post-emit subscribe case explicitly.

**Buffer grows unbounded.** Phase 10.0 does not enforce the capacity argument; `make_stream(N)` records `N` on the instance but never trims the buffer. Synchronous fixtures cannot overflow (every emit is followed by paired recvs along the same path) and the test corpus exercises buffers of at most five elements. Phase 10.2 will introduce a per-subscriber `maxlen` deque under SubMakeLimitExpr; Phase 11 will introduce bounded broadcast under `asyncio.Queue`. The cap argument is preserved for forward-compat with both.

**Mochi-style `mochi_` prefix on the four functions.** `mochi_make_stream`, `mochi_subscribe`, `mochi_emit`, `mochi_recv_sub`. The prefix prevents collision with user-defined Mochi names (`let emit = ...`, `let subscribe = ...`) that would otherwise shadow the builtin under `from mochi_runtime.stream import emit`. Same pattern as the Phase 7 query helpers (`mochi_runtime.query.sum_i64` exported as `sum_i64`, used internally only because the Phase 7 IR rewrites `sum(xs)` to the prefixed name before lower).

**Element-type annotation on the `let` binding.** A Mochi `let s: stream<int> = make_stream(4)` lowers to `s: MochiStream[int] = mochi_make_stream(4)`; subscribers lower to `sub: MochiSub[int] = mochi_subscribe(s)`. The element type comes from `LetStmt.StreamElemType` / `LetStmt.SubElemType`. mypy 1.13 and pyright 1.1.380 both narrow `mochi_recv_sub(sub)` to `int` from the `MochiSub[int]` annotation, so user code that assigns `let n: int = recv_sub(sub)` type-checks without an explicit cast.

**Scalar element types only.** The C lower rejects `stream<RecordName>` and `stream<list<T>>` at the IR layer (`statement N: binding "s" type: stream<T>: element type record not supported in Phase 9.2`). Phase 11 lifts this upstream; the Python lower will pick up the new IR shape without source changes.

### Fixture corpus (8 fixtures)

`tests/transpiler3/python/fixtures/phase10-streams/`:

| Fixture | Surface | Notes |
|---------|---------|-------|
| `stream_basic.mochi` | `int` element, single sub, three sequential recvs | Baseline shape |
| `stream_bool.mochi` | `bool` element | Bool round-trip |
| `stream_float.mochi` | `float` element | Float round-trip |
| `stream_string.mochi` | `string` element | String round-trip |
| `stream_multi_sub.mochi` | Two subs subscribed before any emit | Broadcast fan-out |
| `stream_sub_after_emit.mochi` | Sub subscribed after two emits, then two more emits | Cursor-starts-at-write semantics |
| `stream_two_subs_diverge.mochi` | sub1 created early, drained partially; sub2 created mid-stream | Independent cursors |
| `stream_emit_in_loop.mochi` | `for i in 0..5 { emit(s, i * 10) }` | Producer side composes with control flow |

Three of the eight fixtures (`stream_sub_after_emit`, `stream_two_subs_diverge`, `stream_emit_in_loop`) are new Python-specific extensions of the C corpus. The remaining five mirror `tests/transpiler3/c/fixtures/stream/` 1:1.

### Files changed

| File | Purpose |
|------|---------|
| `runtime/python/mochi_runtime/stream.py` (new) | `MochiStream[T]`, `MochiSub[T]`, and the four `mochi_*` functions |
| `transpiler3/python/lower/stream.go` (new) | `lowerStreamMakeExpr`, `lowerSubMakeExpr`, `lowerStreamEmitStmt`, `lowerSubRecvExpr` |
| `transpiler3/python/lower/lower.go` | dispatch cases for the four new aotir nodes; `needsStream` flag + `from mochi_runtime.stream import ...` gating; `lowerLetStmt` annotates `TypeStream`/`TypeSub` as `MochiStream[T]`/`MochiSub[T]` |
| `transpiler3/python/build/build.go` | Cache marker `mep51-phase09` -> `mep51-phase10` |
| `transpiler3/python/build/phase10_test.go` (new) | `TestPhase10Streams` walks `phase10-streams/` |
| `tests/transpiler3/python/fixtures/phase10-streams/` (new) | 8 `.mochi` + 8 `.out` files |

## Deferred work

- **10.1 async generators.** Requires Phase 11's async colour pass to mark stream-consuming functions as `async def`. The lowering shape (`async def producer() -> AsyncIterator[T]: yield ...` plus `async for item in producer():`) is already designed (see this page's earlier draft); the gate is the colour pass.
- **10.2 bounded streams (`SubMakeLimitExpr`).** Per-subscriber drop-on-overflow under sync, `asyncio.Queue(maxsize=N)` with `await put` under async. The C lower already surfaces the IR node; Python rejects it until Phase 11 lands the chosen semantics.
- **10.3 operator library (`map`, `filter`, `take`, `zip`, `flat_map`, `collect`).** Requires an async iterator base to compose against; deferred until 10.1 lands. The operator definitions are pure stdlib (no asyncio extras), so the wheel does not grow new deps.
- **Record / list / map element types.** Upstream gap: the C lower rejects `stream<RecordName>` at the IR layer. Lifts when the verifier learns to thread element types through ring-buffer storage; the Python target picks up the new IR shape without source changes.
