---
title: "Phase 2. Primitives and control flow"
sidebar_position: 4
sidebar_label: "Phase 2. Primitives + control flow"
description: "MEP-45 Phase 2 tracking: int/float/bool arithmetic, comparisons, short-circuit, if/while/for, functions, divide-by-zero panic."
---

# Phase 2. Primitives and control flow

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 2](/docs/mep/mep-0045#phase-2-primitives-and-control-flow) |
| Status         | IN PROGRESS |
| Started        | 2026-05-22 19:30 (GMT+7) |
| Landed         | — |
| Tracking issue | [#22074](https://github.com/mochilang/mochi/issues/22074) |
| Tracking PR    | — |

## Gate

Arithmetic + control-flow suite (~50 fixtures: int/float ops, comparisons, if/else, while, for-in over int range, recursion) compiles and runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

Primitives + control flow is the smallest set that gets a real (non-toy) Mochi program to compile. Without these the C-AOT target can't host any computation; with these it can host arithmetic-heavy fixtures like the benchmark loops (`fib_iter`, `sum_loop`, `nsieve`). Aligns with the user-facing goal of "one Mochi source, one native binary".

## Sub-phases

| #   | Scope                                                                                              | Status      | Commit | PR |
|-----|----------------------------------------------------------------------------------------------------|-------------|--------|----|
| 2.0 | `int` (`int64_t`), `float` (`double`), `bool`; arithmetic; comparisons; short-circuit `&&` / `||`  | IN PROGRESS | —      | — |
| 2.1 | `if`/`else`, `while`, `return`, `break`, `continue`                                                | NOT STARTED | —      | — |
| 2.2 | `for x in start..end` (int range); user-defined multi-arg functions                                | NOT STARTED | —      | — |
| 2.3 | Integer divide-by-zero raises `MOCHI_ERR_DIVZERO` (checked profile); UB under `--fast-int`         | NOT STARTED | —      | — |
| 2.4 | Float NaN propagation matches vm3 byte-for-byte (IEEE 754 round-trip on `%.17g`)                   | NOT STARTED | —      | — |

## Sub-phase 2.0 -- 2026-05-22 (GMT+7)

### Goal-alignment audit (2.0)

The smallest extension of Phase 1 that lets the C-AOT pipeline compile programs that compute anything. Without 2.0 the only legal program is `print("string literal")`; with 2.0 the entire arithmetic + boolean expression layer compiles. Strict slice: no statements other than `print(<expr>)`, no variables (those land in 2.1 with `let`/`var`/`if`/`while`).

### Decisions made (2.0)

- **Type set named after the C ABI.** `TypeInt = int64_t`, `TypeFloat = double`, `TypeBool = int (0/1)`. The Mochi-level names (`int`, `float`, `bool`) survive into `Type.String()` because Phase 17's reproducibility hashing keys off those exact strings; later phases that introduce wider/narrower numeric variants will add new enum tags rather than rename existing ones.
- **Bool ABI: int rather than C99 `_Bool`.** The runtime print function takes `int` so the emit pass can pass comparison results (already int 0/1 in C) without an explicit cast and so the header stays free of `<stdbool.h>`.
- **`BinOp` enum is monomorphic per type.** `BinAddI64` and `BinAddF64` are distinct tags so the emit pass picks the C operator from `Op` alone. Avoids a typed switch in cBinOp.
- **`BinaryExpr.Result` is stored explicitly.** Keeps `Type()` independent of the BinOp enum ordering so a future renumbering can't silently change observed types.
- **Operator precedence follows the parser.** Mochi's grammar lists `+ - * / %` and `== != < <= > >=` and `&& ||` at the same `BinaryExpr` level, so the lowerer left-associates everything. Fixtures that need explicit grouping use `()`.
- **Mixed-type arithmetic is a lower-time error.** `int + float` rejects with "operator wants both int or both float" instead of inserting an implicit widening, because Mochi semantics require an explicit `as float` cast (which lands in Phase 3 alongside conversions).
- **`!=` on booleans accepted, ordering rejected.** `true < false` would not compile in vm3 either; the lowerer surfaces a "Phase 2.0 only allows == / !=" diagnostic for the relational ops.
- **Short-circuit `&&` / `||` lowers to C's `&&` / `||`.** They preserve short-circuit semantics natively, so no IR-level branching is needed for Phase 2.0 fixtures. Phase 2.4 retests this when NaN/Inf operands enter the comparison set.
- **Unary `-` and `!` lowered inside-out.** The parser collects multiple `-` operators left-to-right; the lowerer applies them right-to-left (`--x` -> `-(-x)`) so the emit always sees a well-formed unary chain.
- **`INT64_MIN` rendering.** `emitInt64Lit` special-cases `-1<<63` as `(-INT64_C(9223372036854775807) - INT64_C(1))` to avoid `9223372036854775808` (which doesn't fit in `int64_t`) appearing in the emitted source.
- **Float literal rendering.** `emitFloatLit` calls Go's `strconv.FormatFloat(v, 'g', -1, 64)` so the emitted source carries the shortest round-trip decimal, then forces a decimal point on integer-valued floats (`1` -> `1.0`) and wraps the literal as `(double)(...)` so the C compiler never narrows to `float`.
- **`%.17g` for `mochi_print_f64` (placeholder).** Phase 2.0 fixtures pick float values whose `%.17g` output already matches Go's `strconv.FormatFloat 'g' -1 64`. Phase 2.4 lifts `runtime/c/src/mochi_str.c`'s `mochi_f64_format` into the MEP-45 runtime so every double prints byte-equal to vm3, including NaN/Inf.
- **Lower rejects 2.1+ shapes loudly.** `let`, `if`, `for`, user `fun`, etc. each surface "Phase 2.0" in the error so a corpus regression that broadens the source surface fails fast instead of being silently miscompiled.
- **Fixtures gate the suite.** `tests/transpiler3/c/fixtures/primitives/<name>/{<name>.mochi, expect.txt}`; the `TestPhase2Primitives` walker picks up new directories without test-file edits, so adding a fixture is a one-step operation.

### Test set (2.0)

- `transpiler3/c/aotir/verifier_test.go::TestVerifyPrimitives` -- positive + negative coverage for the new Builtins, BinaryExpr, UnaryExpr type checks.
- `transpiler3/c/aotir/verifier_test.go::TestTypeStringRoundTrip` -- pins `Type.String()` identifiers (used by Phase 17 reproducibility hashing).
- `transpiler3/c/emit/emit_test.go::TestEmitDispatch` -- per-shape emission spot checks (int literal min, float trailing-zero, binary, unary, short-circuit).
- `transpiler3/c/lower/lower_reject_test.go::TestLowerRejectsPhase21Plus` -- pins the 2.0 surface boundary; 2.1+ shapes must error with a "Phase 2.0" diagnostic.
- `transpiler3/c/build/phase02_test.go::TestPhase2Primitives` -- end-to-end gate across every `tests/transpiler3/c/fixtures/primitives/<name>` directory (35 fixtures at landing time).

## Decisions made

_Per-sub-phase decisions appear under each "Sub-phase X.Y" section above._

## Deferred work

_Tuple return values: Phase 3 alongside records. Big-int / fixed-width ints: not in v1._

## Closeout notes

_Fill in after gate green._
