---
title: "Phase 14. Promise / async bridge"
sidebar_position: 16
sidebar_label: "Phase 14. async bridge"
description: "MEP-72 Phase 14: Promise / async bridge. Translates TS Promise<T> ↔ Mochi async fun, TS AsyncIterable<T> ↔ Mochi stream<T>, AbortSignal as opaque extern. No runtime singleton needed (host JS event loop is intrinsic)."
---

# Phase 14. Promise / async bridge

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase14PromiseAsync` in `package3/typescript/promise/phase14_test.go`: subtests `promise_to_async_fun`, `async_iterable_to_stream`, `iterable_to_list`, `abort_signal_extern`, `error_rejection_translation`, `microbench_50ns`, `golden_corpus`. The first translates a representative TS `Promise<string>` return into a Mochi `async fun(): string` and asserts the emitted Mochi extern compiles. The second translates `AsyncIterable<int>` into `stream<int>` and asserts the emitted Mochi consumer loop uses `for await`. The third translates `Iterable<int>` into `list<int>` (eager) and asserts the `for ... in` consumer is correct. The fourth translates a function that accepts `AbortSignal` and asserts the Mochi-side parameter is an opaque extern. The fifth translates a function that rejects via `throw new Error("...")` and asserts the rejection routes through MEP-52 phase 11's `try-catch` desugar. The sixth microbenchmarks `await Promise.resolve(0)` and asserts the per-call cost is under 100ns on darwin-arm64. The seventh runs against the 24-package fixture corpus.

## Lowering decisions

The Promise / async bridge is structurally smaller than MEP-73's Rust async bridge and MEP-74's Go goroutine bridge. The reason: the host JS runtime's event loop is intrinsic. Every runtime target (Node 22 LTS, Deno 2, Bun 1.1, browser, edge) ships its own event loop + microtask queue; the bridge adds zero code for runtime construction.

Translation table (from research note 08):

| TS construct | Mochi construct | Lowering |
|--------------|-----------------|----------|
| `Promise<T>` (return) | `async fun(): T` | direct: `await` flows through |
| `PromiseLike<T>` | `async fun(): T` | same (structurally identical) |
| `AsyncIterable<T>` (return / for-await source) | `stream<T>` | for-await loop in Mochi → `for await` in emitted TS |
| `AsyncIterator<T>` | `stream<T>` | same |
| `Iterable<T>` | `list<T>` | eager materialise (`[...iterable]`) |
| `AbortSignal` (parameter) | opaque `extern type AbortSignal` | passes through |
| `AbortController` (constructor) | global extern | exposed via `globalThis.AbortController` |
| rejection (`throw new Error(...)`) | Mochi exception | MEP-52 phase 11 try-catch desugar |

The phase ships:

- An `apply-promise.go` pass that wraps each TS-side `async function` extern entry into a Mochi `async fun` declaration; the wrapping is purely declarative (no code generation; the call site uses `await` directly).
- An `apply-async-iterable.go` pass that detects `AsyncIterable<T>` in ApiSurface and rewires the consumer's `for ... in` loop to emit a TS `for await (...)` loop.
- A `microbench/promise_bench_test.go` benchmark suite that records the per-call cost on all three runtime targets (Node, Deno, Bun) and asserts the cost stays under 100ns.

No `Runtime` singleton, no `tokio::block_on`-equivalent, no handle pool. The bridge code in `promise/` is ~200 LOC of Go (the translation table), vs MEP-73's `package3/rust/async/` ~1500 LOC and MEP-74's `package3/go/goroutine/` ~1200 LOC.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/promise/promise.go` | `ApplyPromiseToAsyncFun`, `ApplyAsyncIterableToStream`, `ApplyAbortSignalExtern` |
| `package3/typescript/promise/microbench/promise_bench_test.go` | per-runtime microbench |
| `package3/typescript/promise/phase14_test.go` | `TestPhase14PromiseAsync` sentinel |

## Test set

7 subtests as listed in the Gate section.

## Cross-references

- [Research note 08 Promise / async bridge](/docs/research/0072/08-promise-async-bridge) — the design.
- [MEP-52 phase 11 async colour pass](/docs/implementation/0052/phase-11-async) — the Mochi-side async story.
- [MEP-74 phase 14 goroutine bridge](/docs/implementation/0074/phase-14-goroutine-bridge) — the much-larger sister phase (Go has no shared event loop).
