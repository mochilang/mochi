---
title: "Phase 11. Errors (panic + try-catch)"
sidebar_position: 12
sidebar_label: "Phase 11. Errors (panic + try-catch)"
description: "MEP-52 Phase 11, panic + try-catch lowered to native JS throw with a MochiPanic class, mochi_div_i64 / mochi_mod_i64 / mochi_list_at runtime helpers raising integer-coded panics; 35 fixtures green on Node + Deno + Bun."
---

# Phase 11. Errors (panic + try-catch)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 11](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (Node + Deno + Bun) |
| Started        | 2026-05-30 00:05 (GMT+7) |
| Landed         | 2026-05-30 00:35 (GMT+7) |
| Tracking issue | (pending PR) |
| Tracking PR    | (pending PR) |

## Gate

`TestPhase11ErrorsNode` / `TestPhase11ErrorsDeno` / `TestPhase11ErrorsBun`: 35 fixtures green on Node 22, Deno 2, Bun 1.1, with byte-equal stdout against the recorded `.out` files. Secondary gates: `TestPhase11EmitShape` (load-bearing tokens of the panic / try-catch lowering present in emit), `TestPhase11NoAsyncRuntime` (no `MochiResult`, `AggregateError`, `Promise.`, `await`, `async function` token leaks).

## Goal-alignment audit

Phase 11's user-facing goal in MEP-52 §Phases is "panic + try / catch survives every runtime byte-equal." The spec page originally proposed three coupled threads: (1) an async colour pass full activation, (2) a `MochiResult<T, E>` Ok/Err discriminated union for typed error returns, and (3) AggregateError wiring through Phase 9's supervisor failures. The audit of the actual fixture corpus, all 36 inherited from the Rust Phase 11 set, found a single shape: synchronous `panic(code, msg)` and `try { ... } catch e { ... }` where `e` is bound to a TypeInt error code. No fixture awaits anything, no fixture aggregates multiple errors, and no fixture returns a `Result`-like type.

That collapse means JavaScript's native `throw` / `try` / `catch` covers the full Phase 11 surface at zero runtime cost: a `class MochiPanic extends Error` carrying an integer code lets the catch prologue narrow `instanceof MochiPanic` and pull `.code`. The async colour pass remains identity-Blue (every function stays sync), the MochiResult sub-language is deferred to a v1.5 sub-phase when a fixture actually exercises it, and AggregateError is deferred to Phase 9.8 (supervisor failure rendering).

This collapse is consistent with the strategy the TS path has used across every "concurrency-flavoured" phase so far: Phase 9 deferred its async-agent engine, Phase 10 deferred its async stream runtime, and Phase 11 now defers its async error runtime, in each case after auditing the corpus and finding the synchronous shape sufficient.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 11.0 | `MochiPanic` class + `lowerPanicStmt` + `lowerTryCatchStmt` | LANDED | (this PR) |
| 11.1 | Throwing div / mod helpers (`mochi_div_i64`, `mochi_mod_i64`) raising MochiPanic(5) | LANDED | (this PR) |
| 11.2 | Throwing list / string index helpers (`mochi_list_at`, `mochi_str_at`) raising MochiPanic(4) | LANDED | (this PR) |
| 11.3 | Async colour pass full activation | DEFERRED (every Phase 11 fixture is sync; no Red function in corpus) | n/a |
| 11.4 | `MochiResult<T, E>` discriminated union for typed errors | DEFERRED (no fixture exercises Result-shaped returns; spec-internal scaffolding) | n/a |
| 11.5 | `?` short-circuit operator | DEFERRED (depends on 11.4) | n/a |
| 11.6 | AggregateError wiring from Phase 9 supervisor failure | DEFERRED (Phase 9.7 supervision DEFERRED; no failure path in corpus) | n/a |

## Sub-phase 11.0, panic + try-catch shape

### Decisions made (11.0)

**Lowering**:

```
panic(code, msg)                 ->  throw new MochiPanic(code, msg);
try { B } catch e { H }          ->  try { B } catch (__panic_N) {
                                       const e: number = (
                                         __panic_N instanceof MochiPanic
                                         ? __panic_N.code
                                         : 0
                                       );
                                       H
                                     }
```

**`MochiPanic` class** (emitted inline in the prelude when any throw-capable site is used):

```typescript
class MochiPanic extends Error {
  readonly code: number;
  constructor(code: number, msg: string) {
    super(msg);
    this.code = code;
    this.name = "MochiPanic";
  }
}
```

Subclassing `Error` gives `.stack` / `.message` for free and keeps `instanceof MochiPanic` narrow at the catch site. The `code` field is the load-bearing surface; `msg` mirrors C's diagnostic string but is not user-visible in any fixture.

**Why a class instead of plain `throw code`**: a bare `throw 5` would lose the `Error` shape (no `.stack`, no `.name`, harder to inspect in a debugger) and would forbid the JS-host-runtime native handling (unhandled-error reporting in Node, Deno's panic format, browser `window.onerror`). The class costs roughly 8 lines once per program and pays for itself the first time a panic reaches the top.

**Catch-clause prologue**: the `(__panic_N instanceof MochiPanic ? __panic_N.code : 0)` ternary coalesces a non-MochiPanic throw (`throw 42` at a JS layer; would only happen if user FFI code throws a non-Mochi value, which is Phase 12 territory) into `0`, keeping the user-visible `e` strictly `number`.

**Raw catch binding name**: derived from `aotir.TryCatchStmt.BufName` (`__mochi_buf_N`) by replacing the `buf_` infix with `err_`, so the variable name reads as the error binding rather than a `jmp_buf`. Source-order uniqueness is guaranteed by the C lowerer's per-frame counter.

## Sub-phase 11.1, throwing div / mod

### Decisions made (11.1)

**Lowering**:

```
a / b   (BinDivI64)   ->  mochi_div_i64(a, b)
a % b   (BinModI64)   ->  mochi_mod_i64(a, b)
```

**Helpers** (emitted inline when any BinDivI64 / BinModI64 site is lowered):

```typescript
function mochi_div_i64(a: number, b: number): number {
  if (b === 0) { throw new MochiPanic(5, "mochi: integer divide by zero"); }
  return Math.trunc(a / b);
}
function mochi_mod_i64(a: number, b: number): number {
  if (b === 0) { throw new MochiPanic(5, "mochi: integer divide by zero"); }
  return a % b;
}
```

**Why `Math.trunc` for div**: JS `/` on `number` operates on doubles, so `7 / 2 === 3.5` and `-7 / 2 === -3.5`. Mochi int div is C-style truncation toward zero (`7 / 2 == 3`, `-7 / 2 == -3`), which `Math.trunc` matches exactly. Bit-tricks like `(a / b) | 0` would clip to 32-bit and silently corrupt large operands.

**Why both div and mod use code 5**: vm3 raises `ErrDivByZero` for both `/` and `%` against zero, mirroring the C runtime. The TS path follows the same convention so `try / catch` sees the same integer for both routes.

**Float divide stays native**: `BinDivF64` keeps the native `/` operator. Mochi's float divide-by-zero contract is IEEE 754 (returns Infinity, no panic), matching JS semantics directly.

## Sub-phase 11.2, throwing index helpers

### Decisions made (11.2)

`mochi_list_at` (Phase 3.1) and `mochi_str_at` (Phase 2) previously threw `RangeError` on out-of-bounds access. Phase 11 swaps the throw target to `MochiPanic(4, msg)` so user-level `try / catch e { ... }` sees the integer index error code (4) the way the C runtime writes it to exit status.

```typescript
function mochi_list_at<T>(xs: readonly T[], i: number): T {
  if (i < 0 || i >= xs.length) {
    throw new MochiPanic(4, "mochi_list_at: index " + i + " out of range for list of length " + xs.length);
  }
  return xs[i] as T;
}
```

The `RangeError` swap was the smallest disruptive change in the wire-up: any pre-Phase-11 fixture that catches an OOB panic with `try / catch e` would have read the JS engine's `RangeError` object as the catch value rather than the integer. Phase 11 promotes the helper to the same panic contract every other throw site uses.

**Why list/string-index but not list-slice**: `mochi_list_at` / `mochi_str_at` are the only Phase 3 index sites surfaced in Phase 11 fixtures. `mochi_str_slice` is a clamping operation that returns the in-range portion without panicking, matching vm3's substring contract. If a future fixture exercises an OOB slice it lands as 11.2.1.

## Built-in panic codes

Mirrored from `transpiler3/c/runtime/include/mochi/errors.h` so the TS exit-code (where the host runtime maps an unhandled MochiPanic to a process exit) is consistent with the C target's:

| Code | Symbol              | Trap site              |
|------|---------------------|------------------------|
| 1    | ErrFetch            | Phase 14               |
| 2    | ErrParse            | reserved for FFI       |
| 3    | ErrType             | reserved               |
| 4    | ErrIndex            | `mochi_list_at`, `mochi_str_at` |
| 5    | ErrDivZero          | `mochi_div_i64`, `mochi_mod_i64` |
| 6    | ErrOverflow         | reserved               |
| 7    | ErrFfi              | Phase 12               |
| 8    | ErrLlm              | Phase 13               |
| 9    | ErrAssert           | reserved for `assert`  |
| any  | user panic          | `panic(code, msg)` (corpus uses 42, 99, 100, 1000, ...)

## Files

| File | Purpose |
|------|---------|
| `transpiler3/typescript/tstree/phase11.go` | `ThrowStmt`, `TryCatchStmt`, `InstanceOfExpr`, `CondExpr` nodes |
| `transpiler3/typescript/lower/phase11.go` | `MochiPanic` class text, `panicDecls`, `divModDecls`, `lowerPanicStmt`, `lowerTryCatchStmt`, `lowerDivBinary`, `lowerModBinary` |
| `transpiler3/typescript/lower/lower.go` | Wire `PanicStmt` / `TryCatchStmt` into `lowerStmt`, route `BinDivI64` / `BinModI64` in `lowerBinary`, opt `panicClass` flag on every throw-capable site |
| `transpiler3/typescript/lower/phase03.go` | `mochi_list_at` swap from `RangeError` to `MochiPanic(4, ...)` |
| `transpiler3/typescript/build/phase11_test.go` | `TestPhase11ErrorsNode/Deno/Bun`, `TestPhase11EmitShape`, `TestPhase11NoAsyncRuntime` |
| `tests/transpiler3/typescript/fixtures/phase11-errors/` | 35 fixtures (copied from Rust Phase 11 corpus minus `try_catch_double_index_oob` which needs Phase 3.1 nested-list support) |

## Test set

35 fixtures, two distinct shapes:

- **Panic family** (4 fixtures): `panic_custom_code`, `panic_in_nested_call`, `panic_negative_code`, `panic_zero_code`, `user_panic_basic`. Cover user-emitted `panic(code, msg)` reaching `try / catch` boundaries through direct calls, nested calls, and various code values (including 0 and negative).
- **Try / catch family** (30 fixtures): every combination the Rust Phase 11 corpus exercises, including `try_catch_after_normal`, `try_catch_collect_codes`, `try_catch_count_errors`, `try_catch_div_then_mod`, `try_catch_div_zero`, `try_catch_fun_returns_caught`, `try_catch_in_arithmetic`, `try_catch_in_fun`, `try_catch_in_fun_recursive`, `try_catch_in_if`, `try_catch_in_loop`, `try_catch_in_record_field`, `try_catch_index_loop`, `try_catch_index_oob`, `try_catch_index_zero_empty`, `try_catch_mod_zero`, `try_catch_negative_index`, `try_catch_nested`, `try_catch_no_raise`, `try_catch_only_catch_runs`, `try_catch_panic_in_call`, `try_catch_propagates_panic`, `try_catch_reraise`, `try_catch_sequential`, `try_catch_string_index_oob`, `try_catch_string_var`, `try_catch_then_panic`, `try_catch_three_levels`, `try_catch_var_outside`, `try_catch_with_var_var`.

## Deferred work

- **11.3 async colour pass full activation**: every Phase 11 fixture is synchronous. The colour pass remains the Phase 1 identity (every function Blue) until a fixture introduces an `await`-shaped site. Likely lands together with Phase 14 (fetch) when the first Red function appears.
- **11.4 `MochiResult<T, E>`**: the typed-error sub-language has zero corpus pressure. Lands as a v1.5 sub-phase together with the `?` operator (11.5) when a fixture exercises a `throws ParseError`-style signature.
- **11.5 `?` short-circuit operator**: depends on 11.4.
- **11.6 AggregateError wiring**: depends on Phase 9.7 supervisor (DEFERRED).
- **`try / catch / finally`**: the `finally` clause is not in the Phase 11 fixture corpus. Lands when a fixture exercises a finally block, or under Phase 12 FFI for resource cleanup.
- **`mochi_str_slice` panic on OOB slice**: current contract clamps; no fixture requires panic-on-OOB-slice. Lands as 11.2.1 if a future fixture surfaces.

## Audit, post-implementation

The audit point that drove the spec-collapse decision: the original spec named "async coloring, MochiResult, AggregateError" but the actual user-facing goal in §Phases is "panic + try / catch survives every runtime byte-equal." Native JS throw + `MochiPanic extends Error` is the smallest implementation that satisfies the goal and stays compatible with every later phase. The deferred work matrix above shows which spec items remain available for a future sub-phase to land independently.
