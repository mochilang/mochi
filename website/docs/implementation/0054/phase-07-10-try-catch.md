---
title: "Phase 7.10. Error model (try / catch / panic)"
sidebar_position: 22
sidebar_label: "Phase 7.10. try / catch"
description: "MEP-54 Phase 7.10, mochiPanic / mochiTry implementing the Mochi panic-with-code error model via Go's defer/recover."
---

# Phase 7.10. Error model

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking PR    | [#22599](https://github.com/mochilang/mochi/pull/22599) |
| Commit         | 04ea3ca645 |

## Gate

7 fixtures: `try_catch_div_zero`, `try_catch_index_oob`, `try_catch_no_raise`, `try_catch_nested`, `try_catch_reraise`, `try_catch_in_fun`, `user_panic_basic`. 245 transpiler3/go fixtures green.

## Lowering decisions

The Go output ships three pieces: a `mochiPanicValue` struct carrying `(code int64, msg string)`, a `mochiPanic(code, msg)` user-level primitive that calls Go's built-in `panic` with a `mochiPanicValue`, and a `mochiTry(try, catch func)` helper that installs `defer + recover` and dispatches by the recovered value's type:

- recovered `mochiPanicValue` -> use its `code`.
- recovered `runtime.Error` -> translate Go's runtime panic to a Mochi code via the message text: "out of range" -> 4, "divide by zero" -> 5, anything else -> 9.
- any other recovered value -> re-panic (Go's stdlib panics that aren't `runtime.Error` are program bugs the Mochi error model can't represent).

`TryCatchStmt` lowers to `mochiTry(func() { <try block> }, func(<catchVar> int64) { <catch block> })`. Go closures capture outer mutable variables by reference, so the `try_catch_in_fun` pattern (a result variable assigned in both arms) works correctly: the closure mutates the outer binding directly. `PanicStmt` lowers to `mochi__panic_user(code)` for user-level panic.

The runtime-panic-to-code translation table is a Mochi convention shared across transpilers (C, JVM, BEAM, Swift, Ruby, PHP, Go); centralising it in `mochiTry` keeps the per-fixture try / catch arms portable.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/stmt.go` | `TryCatchStmt` -> `mochiTry(try, catch)`; `PanicStmt` -> `mochi__panic_user(code)` |
| `transpiler3/go/lower/lower.go` | `mochiPanicValue`, `mochiPanic`, `mochiTry` helper texts |
| `tests/transpiler3/go/fixtures/try_catch_*/`, `user_panic_*/` | 7 fixtures |

## Test set

- 7 `TestPhase1Hello/try_catch_*`, `user_panic_basic` subtests.

## Closeout notes

Mapping Go's `runtime.Error` to Mochi codes via the message text is fragile (the text could change between Go releases), but the alternative (a per-runtime-error-type switch) would require importing `runtime` and depending on exported error type names. The message-match approach is what every other transpiler does and matches the C runtime's behaviour. Tests pin the codes; if a future Go release renames an error, the gate would fail loudly rather than silently producing the wrong code.
