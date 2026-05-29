---
title: "Phase 13. Streams (Mochi::Runtime::Stream)"
sidebar_position: 17
sidebar_label: "Phase 13. Streams"
description: "MEP-56 Phase 13, stream<T> values lowered to Mochi::Runtime::Stream broadcast channels with per-subscriber queues."
---

# Phase 13. Streams (Mochi::Runtime::Stream)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | d0041ea451 |

## Gate

`TestPhase13Streams` in `transpiler3/ruby/build/phase13_test.go`: two inline subtests, `stream_basic` and `stream_multi_sub`. Each subtest compiles a Mochi source that builds a `stream<int>` via `make_stream(cap)`, subscribes one or more consumers via `subscribe(s)`, emits values via `emit(s, v)`, receives via `recv_sub(sub)`, and prints the results. The `.rb` runs under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and stdout is diffed against the recorded expectation. `stream_basic` covers a single-subscriber FIFO (`10, 20, 30` emitted and received in order); `stream_multi_sub` covers the broadcast property (two subscribers, one emit, both receive the same value). There are no `wantInRb` assertions; the gate is purely behavioural because the rendered Ruby is a sequence of method calls on a runtime class whose tested API surface is exercised by the runtime calls themselves.

## Lowering decisions

`stream<T>` is the only Mochi concurrency primitive that needs a non-stdlib runtime helper, because Ruby has no built-in broadcast channel. The Ruby lowerer leans on `Mochi::Runtime::Stream`, defined at `mochi-runtime/lib/mochi/runtime/stream.rb`, which implements a bounded MPMC broadcast over per-subscriber `Thread::SizedQueue`s with a `Mutex`-guarded subscriber list. The runtime gem is loaded once by the `require "mochi/runtime"` line in the generated file header (lower.go line 103).

`aotir.StreamMakeExpr` lowers in `lowerExpr` (lower.go lines 793 to 798) to a `RawExpr` of the form `Mochi::Runtime::Stream.new(cap)`. The capacity argument lowers through `lowerExpr` first so `make_stream(N)` where `N` is a variable resolves. The capacity is per-subscriber: every queue created by `subscribe` is a `Thread::SizedQueue.new(cap)`, so the slowest subscriber back-pressures all emits without dropping values for faster subscribers (see `stream.rb` line 16 inside `subscribe`).

`aotir.SubMakeExpr` lowers (lower.go lines 799 to 804) to a no-arg `MethodCall{Receiver: st, Method: "subscribe"}`, rendering as `s.subscribe`. The runtime method registers a new `SizedQueue` on the stream's `@subs` list under the `@lock` mutex (`stream.rb` lines 15 to 19) and returns the queue, so the Mochi-side `sub` variable holds the same `SizedQueue` that future emits will push into. `aotir.SubMakeLimitExpr` (lower.go lines 805 to 814) covers the bounded-drop variant `subscribe_limit(stream, N)`, rendering as `s.subscribe_limit(N)`; the runtime returns a `LimitedQueue` that silently drops new values once it already holds N items (`stream.rb` lines 37 to 53), giving back-pressure-tolerant consumers a way to skip rather than block.

`aotir.SubRecvExpr` lowers (lower.go lines 815 to 820) to a no-arg `MethodCall{Receiver: sub, Method: "pop"}`, rendering as `sub.pop`. Pop is the same call used for plain channels in Phase 10, because the subscriber's queue *is* a `Thread::SizedQueue` (or the `LimitedQueue` wrapper, which exposes the same `pop` interface). That uniformity means the lowerer does not need a `SubRecvLimitExpr` variant; both kinds of subscriber receive through the same node.

`aotir.StreamEmitStmt` lowers in `lowerStmt` (lower.go lines 236 to 250) to an `ExprStmt` wrapping a `MethodCall{Receiver: st, Method: "emit", Args: [v], UseParens: true}`, rendering as `s.emit(v)`. The runtime method snapshots the subscriber list under the lock and pushes the value into each queue outside the lock (`stream.rb` line 31), so emits do not serialise on the lock for the actual push and a slow subscriber back-pressures only its own queue, not the producer's hold on `@subs`.

The broadcast property tested by `stream_multi_sub` (subscribe twice, emit once, both subscribers receive) falls directly out of the runtime: `emit` iterates `@subs.dup` and pushes into each queue, so subscriber 1 and subscriber 2 each see the same `7` and the test diffs `7\n7\n`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `StreamMakeExpr` → `Mochi::Runtime::Stream.new(cap)`; `SubMakeExpr` → `s.subscribe`; `SubMakeLimitExpr` → `s.subscribe_limit(N)`; `SubRecvExpr` → `sub.pop`; `StreamEmitStmt` → `s.emit(v)` |
| `mochi-runtime/lib/mochi/runtime/stream.rb` | `Mochi::Runtime::Stream` class (Mutex-guarded subscriber list, per-sub `Thread::SizedQueue`); `LimitedQueue` wrapper for `subscribe_limit` (silently drops at the limit) |
| `transpiler3/ruby/build/phase13_test.go` | `TestPhase13Streams` with 2 subtests |

## Test set

- `TestPhase13Streams/stream_basic`, `stream_multi_sub`.

## Closeout notes

Phase 13 landed on CRuby 3.4 with both subtests green. Choosing a runtime class over an inline-lowered broadcast was unavoidable because Ruby's stdlib `Thread::Queue` and `Thread::SizedQueue` are single-consumer-per-queue: you cannot broadcast one `push` to two `pop`s without an explicit fan-out, which is exactly what `Mochi::Runtime::Stream` provides. Per-subscriber queues (rather than a single ring buffer with read cursors) was chosen because cursors would force every subscriber to lock the producer side on every read; per-sub queues let each subscriber drain independently and back-pressure only its own emits-to-it path. Key implementation insight: the `@lock.synchronize { @subs.dup }.each { |q| q.push(val) }` pattern inside `Stream#emit` is the standard "copy-then-iterate-outside-the-lock" idiom, which lets new subscribers register concurrently with an in-flight emit without the emit having to hold the lock for the whole iteration. Phase 13.1 (close, subscriber count, drop-newest variants) will extend the runtime class rather than the lowerer.
