---
title: "Phase 12. async / await (Thread + .value)"
sidebar_position: 16
sidebar_label: "Phase 12. async / await"
description: "MEP-56 Phase 12, async fun lowered to Thread.new { body }, await lowered to .value, single-threaded fixtures."
---

# Phase 12. async / await (Thread + .value)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | c6317bc4be |

## Gate

`TestPhase12AsyncAwait` in `transpiler3/ruby/build/phase12_test.go`: two inline subtests, `async_basic` and `async_two`. Each subtest compiles a Mochi source that wraps a function call in `async`, awaits the resulting future, and prints the result. The `.rb` runs under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and stdout is diffed against the recorded expectation. `async_basic` covers a single async/await round-trip (`compute(): int { 42 }` returning `42`) and `async_two` covers two concurrent futures awaited in submission order, with `times_two(3) = 6` and `times_two(7) = 14`. There are no `wantInRb` assertions; the gate is purely behavioural because the rendered Ruby shape (`Thread.new { call(...) }`, then `.value`) is short enough that the runtime correctness fully constrains the lowering.

## Lowering decisions

`aotir.AsyncExpr` lowers in `lowerExpr` (lower.go lines 739 to 747) to a `RawExpr` of the form `Thread.new { body }`, where the body is the lowered inner expression. The inline comment at lines 744 to 746 records the chain of reasoning: "Ruby Thread is the simplest fit: Thread.new { body }.value blocks until the body returns and yields its result. Async then becomes a Thread handle; Await calls .value." `aotir.AwaitExpr` lowers (lines 748 to 753) to a `MethodCall{Receiver: fut, Method: "value"}`, rendering as `fut.value`. `Thread#value` is documented to join the thread and return its block's last expression value (or re-raise its exception), which is exactly the semantics Mochi assigns to `await`.

Three Ruby concurrency primitives were considered; only Thread fits. Fiber was rejected because fibers are cooperatively scheduled: an `async f()` whose body never yields would never run unless an explicit transfer/scheduler is wired in. Mochi's `async` semantics promise the body runs concurrently with the caller without any cooperation contract, so a Thread (preemptively scheduled by the OS) matches. Ractor (Ruby 3.0+ shared-nothing parallelism) was rejected on the opposite axis: Ractors cannot share most objects, so a closure that captures an outer-scope `let x = 10` and runs inside `Ractor.new` would either deep-copy `x` or refuse to start; that breaks the implicit-capture contract Phase 7 just landed. Thread shares state by default (under the GVL) and lets `Thread.new { compute() }` reach back into the caller's locals without ceremony, which keeps the Mochi-to-Ruby mapping one-to-one.

The capture model is therefore: a Mochi `async expr` lowers to a Thread that closes over the same locals the surrounding Mochi scope sees, because Ruby's block syntax `{ ... }` captures the enclosing binding. There is no env-hash plumbing as in Phase 7's closures, because the Ruby block already implements the implicit-capture step at the language level. The price is concurrency-safety: under the GVL, only one Thread runs Ruby bytecode at a time, so the `async_two` fixture is effectively interleaved, not parallel; that is fine for Phase 12's correctness gate but means CPU-bound `async` work will not speed up on multi-core hardware until a future phase migrates to Ractor with explicit data marshalling.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `AsyncExpr` arm → `Thread.new { body }` (lines 739 to 747); `AwaitExpr` arm → `fut.value` (lines 748 to 753); both rely on the standard library's `Thread`, so no runtime require is added |
| `transpiler3/ruby/build/phase12_test.go` | `TestPhase12AsyncAwait` with 2 subtests |

## Test set

- `TestPhase12AsyncAwait/async_basic`, `async_two`.

## Closeout notes

Phase 12 landed on CRuby 3.4 with both subtests green. The `Thread + .value` shape was picked over `Concurrent::Future` from the concurrent-ruby gem because the stdlib version is dependency-free; depending on concurrent-ruby would have shifted a known-good Mochi build onto an external gem's release cadence for a feature that Ruby has supported natively since 1.9. Key implementation insight: `Thread#value` blocks the calling thread until the body finishes and either returns the last expression's value or re-raises any uncaught exception. That second property silently aligns `await` with Mochi's panic-propagation contract: if the async body raises a `Mochi::Runtime::Panic`, awaiting it re-raises in the caller, which is the same surface behaviour as the synchronous Phase 5 path. Future Phase 12.1 (cancel, timeout) will need extra machinery (`Thread#raise`, `Thread#join(timeout)`) that the current shape does not yet wire up, but neither subtest needs them and adding them now would be premature.
