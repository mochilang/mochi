---
title: "Phase 21. Stream subscribe_limit + saveCSV"
sidebar_position: 25
sidebar_label: "Phase 21. Stream subscribe_limit + saveCSV"
description: "MEP-56 Phase 21, drop-on-full stream subscribers and CSV writing lowered to runtime LimitedQueue and CSV.open."
---

# Phase 21. Stream subscribe_limit + saveCSV

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 7a8e62a176 |

## Gate

`TestPhase21StreamSaveCSV` in `transpiler3/ruby/build/phase21_test.go`: two subtests (`subscribe_limit_drops_overflow`, `save_csv`). The first creates a `stream<int>` with capacity 8, attaches a `subscribe_limit(s, 2)` subscriber, emits 1 / 2 / 3, and asserts that two `recv_sub(sub)` calls return `1` and `2` (the third emit is silently dropped because the subscriber's queue is already full). The second writes `[["a","b"],["c","d"]]` via `saveCSV(path, rows)`, loads it back via `loadCSV(path)`, and asserts the round-trip preserves cell content. Both run under the resolved Ruby toolchain with `-I mochi-runtime/lib`.

## Lowering decisions

`subscribe_limit` delegates to a dedicated `Mochi::Runtime::LimitedQueue` runtime class so the drop-on-full policy lives in one auditable spot; `saveCSV` lowers inline to a one-shot `require 'csv'; CSV.open(...) { ... }` block (`transpiler3/ruby/lower/lower.go` lines 306 to 317 and 805 to 814):

- `aotir.SubMakeLimitExpr` to `MethodCall{Receiver: st, Method: "subscribe_limit", Args: [limit], UseParens: true}` (lines 805 to 814). The receiver is the stream object (`Mochi::Runtime::Stream`); `subscribe_limit(limit)` is defined in `mochi-runtime/lib/mochi/runtime/stream.rb` and returns a `LimitedQueue.new(limit)` which is registered with the stream's subscriber list.
- The runtime `LimitedQueue#push` method (`mochi-runtime/lib/mochi/runtime/stream.rb`) acquires a mutex and only forwards to the underlying `Thread::Queue` when `@q.size < @limit`; the third `emit` in the gate hits the size guard and is dropped. `pop` is unchanged (blocks on the underlying queue), so the consumer side reads exactly as it would from the unlimited `subscribe` variant.
- `aotir.SubRecvExpr` to `MethodCall{Receiver: sub, Method: "pop"}` (lines 815 to 820) is shared with regular subscribers. The `LimitedQueue` exposes the same `pop` shape as a plain `Thread::Queue`, so Mochi's `recv_sub` lowering is policy-agnostic.
- `aotir.SaveCSVStmt` to `RawStmt` rendering `(require 'csv'; CSV.open(path, 'w') { |__c| (data).each { |__row| __c << __row } })` (lines 306 to 317). The lazy `require 'csv'` keeps the static prelude limited to `require "mochi/runtime"`. `CSV.open(path, 'w')` opens for writing and yields a `CSV` instance; `__c << __row` appends one row at a time which is more memory-efficient than building the full CSV string in memory and then writing it. The block auto-closes the file on exit, matching the `File.open { ... }` discipline used elsewhere in the lowerer.

The contrast between `subscribe` (unlimited, blocks emitter when full) and `subscribe_limit` (drops on full) is captured purely on the subscriber side, not on `emit`. That asymmetry is intentional: `emit` is a fan-out broadcast and must not stall the producer on one slow subscriber, so the back-pressure policy lives with each subscriber's queue.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `SaveCSVStmt` (lines 306 to 317) renders `(require 'csv'; CSV.open(...) { ... })`; `SubMakeLimitExpr` (lines 805 to 814) calls `subscribe_limit` on the stream |
| `mochi-runtime/lib/mochi/runtime/stream.rb` | `Stream#subscribe_limit(limit)` returns a registered `LimitedQueue`; `LimitedQueue#push` drops silently when `@q.size >= @limit`, `pop` blocks normally |
| `transpiler3/ruby/build/phase21_test.go` | `TestPhase21StreamSaveCSV` with 2 subtests covering drop-on-full and CSV round-trip |

## Test set

- `TestPhase21StreamSaveCSV/subscribe_limit_drops_overflow`, `save_csv`.

## Closeout notes

Phase 21 landed on CRuby 4.0 (Homebrew). The drop policy lives in `LimitedQueue` (not in `emit`) so an emitter never blocks waiting for a slow subscriber: each subscriber is responsible for handling its own back-pressure tolerance. `CSV.open(path, 'w') { |c| rows.each { c << row } }` is preferred over `CSV.generate(rows)` + `File.write` because the per-row push streams output and avoids materialising the full CSV string in memory, which matters once `saveCSV` gets used on multi-megabyte query results.
