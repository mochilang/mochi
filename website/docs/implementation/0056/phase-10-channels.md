---
title: "Phase 10. Channels (Thread::SizedQueue)"
sidebar_position: 14
sidebar_label: "Phase 10. Channels"
description: "MEP-56 Phase 10, chan<T> values lowered to Ruby Thread::SizedQueue with blocking push/pop semantics."
---

# Phase 10. Channels (Thread::SizedQueue)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 00bf88ebb5 |

## Gate

`TestPhase10Channels` in `transpiler3/ruby/build/phase10_test.go`: three inline subtests, `chan_basic`, `chan_bool`, and `chan_buffered`. Each subtest compiles a Mochi source that builds a `chan<T>` via `make_chan(cap)`, sends values into it via `send(ch, v)`, receives them via `recv(ch)`, and prints the results. The `.rb` runs under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and stdout is diffed against the recorded expectation. The set covers `chan<int>` with capacity 1 (one send then one recv), `chan<bool>` with capacity 2 (true/false round-trip), and `chan<int>` with capacity 3 (three sends then three recvs to confirm FIFO order is preserved). All three fixtures are single-threaded: the comment at the top of the test file flags that `cap >= number of sends before any recv` avoids the blocking-producer issue, deferring multi-thread fixtures to Phase 10.1.

## Lowering decisions

`aotir.ChanMakeExpr` lowers in `lowerExpr` (lower.go lines 821 to 829) to a `RawExpr` of the form `Thread::SizedQueue.new(cap)`. `Thread::SizedQueue` is Ruby's standard-library bounded blocking queue: `push` blocks when the queue holds `cap` items, and `pop` blocks when the queue is empty. Those semantics line up exactly with Mochi's `chan<T>`: a buffered channel of capacity N where `send` blocks the producer when full and `recv` blocks the consumer when empty. Using the unqualified `SizedQueue` would also work on modern Ruby, but the fully qualified `Thread::SizedQueue` is the documented name and makes it explicit that this is the threading primitive, not an unrelated queue type. The capacity expression is lowered through `lowerExpr` first, so `make_chan(n)` where `n` is a variable works, not just literal constants.

`aotir.ChanSendStmt` lowers in `lowerStmt` (lower.go lines 251 to 265) to an `ExprStmt` wrapping a `MethodCall{Receiver: ch, Method: "push", Args: [v], UseParens: true}`, rendering as `ch.push(v)`. The receiver expression and the value expression are both lowered recursively, so `send(make_chan(1), 42)` would inline correctly. `aotir.ChanRecvExpr` lowers in `lowerExpr` (lines 830 to 835) to a `MethodCall{Receiver: ch, Method: "pop"}` (no args, no parens), rendering as `ch.pop`. The Mochi-side method names (`send`, `recv`) map to Ruby's `push`/`pop` rather than `enq`/`deq` because `push`/`pop` are the canonical aliases on Ruby's queue classes and what most idiomatic Ruby code uses.

Choosing `Thread::SizedQueue` over `Async::Channel` or a hand-rolled mutex+condvar pair was driven by three properties: (1) it ships with CRuby's stdlib so no extra gem dependency is needed, (2) its blocking semantics on full and empty match Mochi's exactly, and (3) it is safe across Threads (the actual concurrency primitive Phase 12's `async/await` lands), so Phase 10 and Phase 12 share the same primitive without a second translation step. The inline comment at lower.go lines 826 to 828 makes the trade explicit: "matching Mochi's blocking channel semantics for the single-thread fixtures. Multi-producer/multi-consumer uses surface in Phase 10.1." The `chan<T>` type annotation (`chan<int>`, `chan<bool>`) is erased on lowering, mirroring the lowerer's behaviour for `list<T>`: Ruby is duck-typed and the upstream typecheck has already validated element types.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `ChanMakeExpr` arm → `Thread::SizedQueue.new(cap)` (lines 821 to 829); `ChanSendStmt` arm → `ch.push(v)` (lines 251 to 265); `ChanRecvExpr` arm → `ch.pop` (lines 830 to 835) |
| `transpiler3/ruby/build/phase10_test.go` | `TestPhase10Channels` with 3 subtests |

## Test set

- `TestPhase10Channels/chan_basic`, `chan_bool`, `chan_buffered`.

## Closeout notes

Phase 10 landed on CRuby 3.4 with all three subtests green. The deliberate restriction to single-thread fixtures (`cap >= sends before any recv`) lets Phase 10 prove the lowering shape without depending on Phase 12's `Thread.new` integration; once Phase 12 is in, the same `Thread::SizedQueue` primitive carries values between threads with zero extra Ruby lowerer changes. Key implementation insight: Ruby's `Queue` (unbounded) was rejected in favour of `SizedQueue` (bounded) because Mochi `make_chan(cap)` declares the cap as a contract, and silently ignoring it would be a soundness gap once a downstream phase introduces a deadlock-detection test. Phase 10.1, when it lands, will add multi-producer/multi-consumer fixtures that share a `Thread::SizedQueue` across `Thread.new` blocks.
