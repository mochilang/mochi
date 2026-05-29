---
title: "Phase 9.1. Channels (make_chan, send, recv)"
sidebar_position: 25
sidebar_label: "Phase 9.1. Channels"
description: "MEP-54 Phase 9.1, Mochi's bounded ring channel mapped directly onto Go's native chan T with no runtime helper."
---

# Phase 9.1. Channels

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED (on `worktree-mep54-impl`, not yet merged to `main`) |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 14:42 (GMT+7) |
| Tracking PR    | [#22623](https://github.com/mochilang/mochi/pull/22623) |
| Commit         | 6c8caa8f52 |

## Gate

8 fixtures: `chan_int` (cap 1, send 42, recv, print), `chan_bool` (cap 2, true / false), `chan_string` (cap 2, hello / world), `chan_float` (cap 2, 1.5 / 2.25), `chan_multi` (cap 3, 10 / 20 / 30), `chan_five` (cap 5, 1 through 5), `chan_interleave` (mixed send / recv against half-full channel), `chan_loop_send` (while loop driving send + recv pairs). 269 total transpiler3/go fixtures green.

## Lowering decisions

Mochi's bounded ring channel maps directly onto Go's native `chan` type, so Phase 9.1 emits `make(chan T, int(cap))`, `c <- v`, and `<-c` without any runtime helper. Three aotir nodes dispatch:

- `aotir.ChanMakeExpr` -> `make(chan T, int(cap))`. The element type is rendered inline via a `gotree.RawExpr{Src: "chan " + elem}` so the new chan shape doesn't need a fresh AST node. The capacity expression is lowered first via `lowerExpr`, then wrapped in an `int(...)` cast because Mochi's `cap` is `int64` and Go's `make` wants the machine-width `int`.
- `aotir.ChanSendStmt` -> `gotree.SendStmt{Chan: ch, Value: val}`, rendering as `c <- v`.
- `aotir.ChanRecvExpr` -> `gotree.UnaryExpr{Op: "<-", X: ch}`, rendering as `<-c`. The Phase 2 `UnaryExpr` operand-parenthesisation rule handles the case where the receive sits inside another unary expression (`-<-c`) without emitting the ambiguous `--<-c`.

`TypeChan` is added to `letTypeText` via `LetStmt.ChanElemType` so `var c chan<int> = make_chan(1)` gets the right Go annotation. `Param` and `Function` do not carry `ChanElemType` yet, so channels remain locals for now (matching the JVM phase-10 baseline). Lifting channels into function signatures lands in Phase 9.1.1 when a fixture motivates it.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/types.go` | `lowerChanType(elem)` -> `chan Elem` |
| `transpiler3/go/lower/expr.go` | `ChanMakeExpr` -> `make(chan T, int(cap))`; `ChanRecvExpr` -> `<-c` |
| `transpiler3/go/lower/stmt.go` | `ChanSendStmt` -> `c <- v`; `letTypeText` handles `TypeChan` |
| `tests/transpiler3/go/fixtures/chan_*/` | 8 fixtures |

## Test set

- 8 `TestPhase1Hello/chan_*` subtests covering scalar element types, capacities 1 / 2 / 3 / 5, half-full interleave, while-loop driven send / recv.

## Closeout notes

Phase 9.1 was a small, self-contained patch (3 dispatch arms + 1 type helper) because Go's native `chan` matches Mochi's semantics almost perfectly: bounded by `make` capacity, blocking on full send / empty recv. Skipping the runtime helper saves both code size and a per-program initialisation step. Phase 9.2 (streams + subscribers) is the natural sequel, and Phase 9.1.1 (channels as function parameters and returns) is queued behind whichever phase first writes a fixture that needs it.
