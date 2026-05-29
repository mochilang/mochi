---
title: "Phase 11. Try / catch / panic"
sidebar_position: 13
sidebar_label: "Phase 11. Errors"
description: "MEP-53 Phase 11, try / catch / panic lowered via panic::catch_unwind with an i64 payload."
---

# Phase 11. Try / catch / panic (panic::catch_unwind)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | [#22584](https://github.com/mochilang/mochi/issues/22584) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | 00d3ee0f28 |

## Gate

`TestPhase11Errors` walks `tests/transpiler3/rust/fixtures/phase11-errors/` (36 fixtures) and asserts byte-equal stdout. Coverage: explicit `panic(code)`, try / catch with code binding, panic from division by zero (code 5), panic from out-of-bounds index (code 4), nested try / catch, try without catch (re-panic), catch with mixed-type bodies.

## Lowering decisions

```mochi
try { risky() } catch e { print(e) }
```

lowers to:

```rust
match mochi_runtime::panic::catch(|| {
    risky();
}) {
    Some(__code) => {
        let e: i64 = __code;
        mochi_runtime::io::print_i64(e);
    }
    None => {}
}
```

`mochi_runtime::panic::catch` wraps `std::panic::catch_unwind(panic::AssertUnwindSafe(f))`, downcasts the payload to extract an `i64`, and falls back to a message-string map for stdlib panics:

- "out of bounds" / "index out of" → code 4
- "divide by zero" / "attempt to divide" / "remainder" → code 5
- everything else → code 1

`mochi_runtime::panic::raise(code)` calls `panic::panic_any(code)`. `silence_hook()` installs a `panic::set_hook(Box::new(|_| {}))` exactly once via `Once`, so panic messages don't leak to stderr — important because tests diff stdout but also need stderr clean (a non-empty stderr would cause downstream tools to flag the binary as failing even with stdout matching).

The choice to use `panic::catch_unwind` rather than `Result<T, E>` is deliberate: Mochi's `try` block can wrap arbitrary statements, not just expression-level operations. Routing through `Result` would require either a full effect-system colouring pass or wrapping every fallible op site (every division, every index) in `?`. The catch_unwind approach is single-site, decouples the panic-raising code from the catching code, and matches the semantic shape of Mochi's source-level `try` exactly.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/try.go` | try / catch / panic lowering |
| `runtime3/rust/mochi-runtime/src/lib.rs` | Add `panic` module |
| `transpiler3/rust/build/phase11_test.go` | 36-fixture gate |
| `tests/transpiler3/rust/fixtures/phase11-errors/*.mochi` + `.out` | 36 fixtures |

## Test set

- `TestPhase11Errors/<fixture>` for each `.mochi` in the fixture directory (36 fixtures).

## Closeout notes

`AssertUnwindSafe` is required because closures that capture `&mut` state are not `UnwindSafe` by default. This is a Rust quirk: a panic can leave a `&mut` in an unspecified state, so the compiler refuses unless the user asserts safety. For Mochi semantics, this is always safe because a panic always returns the caught code and never continues with a torn state. The assertion is wrapped inside `mochi_runtime::panic::catch`, so user code never sees it.
