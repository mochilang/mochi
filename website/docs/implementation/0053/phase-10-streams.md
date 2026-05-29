---
title: "Phase 10. Streams and channels"
sidebar_position: 12
sidebar_label: "Phase 10. Streams"
description: "MEP-53 Phase 10, streams and channels lowered to single-thread Rc<RefCell<VecDeque>>."
---

# Phase 10. Streams and channels (single-thread Rc)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | 9b6bd876ad |

## Gate

`TestPhase10Streams` walks `tests/transpiler3/rust/fixtures/phase10-streams/` (31 fixtures) and asserts byte-equal stdout. Coverage: channel make / send / recv, stream make / emit / subscribe / recv_sub, multi-subscriber broadcast, subscribe_limit (currently unbounded — limit reserved for a future phase), channel in agent, stream in agent.

## Lowering decisions

`make_chan(N)` lowers to `mochi_runtime::chan::Chan::make(N)`, backed by `Rc<RefCell<VecDeque<T>>>`. Send is `chan.send(v)` (pushes to back). Recv is `chan.recv()` (pops from front, panics on empty — Mochi's stream semantics guarantee recv only happens when send has run).

`make_stream(N)` lowers to `mochi_runtime::stream::Stream::make(N)`, backed by `Rc<RefCell<Vec<Rc<RefCell<VecDeque<T>>>>>>` (a Vec of per-subscriber queues). `subscribe(&s)` allocates a fresh queue, appends to the subscriber list, and returns a `Sub<T>` that holds an Rc to its own queue. `emit(s, v)` pushes a clone of `v` onto every subscriber's queue. `recv_sub(sub)` pops from the subscriber's queue.

`subscribe_limit(&s, limit)` currently delegates to `subscribe(&s)` (limit ignored, unbounded queue). The symbol is reserved for a future phase that wires a `LimitedQueue` similar to the Ruby target's drop-on-full semantics.

There is no `std::sync::Arc` or `std::sync::Mutex` anywhere. The runtime is strictly single-thread; Mochi's `async` (phase reserved, lowers to immediate eval) does not introduce concurrency.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/chan.go` | Channel lowering |
| `transpiler3/rust/lower/stream.go` | Stream + subscribe + emit lowering |
| `runtime3/rust/mochi-runtime/src/lib.rs` | Add `chan` and `stream` modules |
| `transpiler3/rust/build/phase10_test.go` | 31-fixture gate |
| `tests/transpiler3/rust/fixtures/phase10-streams/*.mochi` + `.out` | 31 fixtures |

## Test set

- `TestPhase10Streams/<fixture>` for each `.mochi` in the fixture directory (31 fixtures).

## Closeout notes

The chunk that took longest was making `Sub::recv` Clone-able. Sub is `{ inner: Rc<RefCell<VecDeque<T>>> }`; cloning a Sub gives a second handle to the same queue, which is wrong (recv on one Sub should not consume from another). The fix: subscribe returns a fresh Sub backed by a fresh queue, and clone is allowed but documented as "second handle to the same queue, may consume the other's data." Fixtures avoid the ambiguous case by never cloning Subs.
