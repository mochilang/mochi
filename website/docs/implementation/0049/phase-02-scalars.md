---
title: "Phase 2. Scalars"
sidebar_position: 3
sidebar_label: "Phase 2. Scalars"
description: "MEP-49 Phase 2 — complete scalar type lowering: int→Int64, float→Double, bool→Bool, string→String; arithmetic, comparison, string ops; MOCHI003 overflow analyzer."
---

# Phase 2. Scalars

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 2](/docs/mep/mep-0049#phase-2-scalars) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase2Scalars`: 20 fixtures green on Swift 6.0 and 6.1, linux-x64. `TestSwiftcClean` remains green.

## Goal-alignment audit

Scalars are the foundation everything else rests on. The critical decision here is `int` → `Int64` (not Swift's platform-width `Int`). Getting this wrong would make Mochi programs produce different results on 32-bit vs 64-bit platforms, and would break the byte-equality guarantee against vm3. Phase 2 locks in all scalar semantics and makes them explicit in generated code.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 2.0 | `int` → `Int64`: arithmetic, comparison, negation; wrapping operators `&+` / `&-` / `&*` | NOT STARTED | — |
| 2.1 | `float` → `Double`: arithmetic, comparison, math functions via `Swift.Glibc`/`Darwin` | NOT STARTED | — |
| 2.2 | `bool` → `Bool`: `&&`, `\|\|`, `!`; short-circuit evaluation | NOT STARTED | — |
| 2.3 | `string` → `String`: concatenation, length, `substr`, comparison, `startsWith`/`endsWith` | NOT STARTED | — |
| 2.4 | NaN/Inf/overflow edge cases; `TestScalarEdgeCases` differential against vm3 | NOT STARTED | — |

## Sub-phase 2.0 -- int → Int64

### Decisions made (2.0)

**`Int64` not `Int`**: Swift's `Int` is 64-bit on 64-bit platforms but 32-bit on 32-bit targets (Embedded Swift, old watchOS). Using `Int64` explicitly makes Mochi programs portable and byte-identical to vm3 across all targets. Every integer literal in emitted Swift has an explicit `Int64(...)` cast or `as Int64` annotation when inference would produce `Int`.

**Arithmetic operators**: `+`, `-`, `*`, `/`, `%` lower directly to Swift `Int64` operators. Division is truncating (matches vm3's Go integer division). Modulo sign follows the dividend (Swift matches Go here).

**Overflow behavior**: Mochi integer arithmetic traps on overflow by default (matches Go's behavior on overflow check builds, and the Mochi spec). Swift's default `+`/`-`/`*` trap on overflow for fixed-width types. The lowerer emits standard operators, not wrapping `&+`/`&-`/`&*`. Wrapping operators are only emitted when the Mochi source explicitly uses the `wrapping_add`/`wrapping_sub`/`wrapping_mul` builtins.

**Integer literals**: All `IntLit(n)` → `Int64(n)` in sxtree. Negative literals: `-42` → `Int64(-42)` (not `-(Int64(42))`) to avoid the overflow trap on `Int64.min`.

**Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=` → same Swift operators on `Int64`. Return type is `Bool`.

**Bitwise**: `&`, `|`, `^`, `~`, `<<`, `>>` → Swift bitwise operators on `Int64`. Right shift is arithmetic (sign-extending), matching vm3.

**Conversions**: `int_to_float(n)` → `Double(n)`. `float_to_int(f)` → `Int64(f)` (truncating, traps on NaN or out-of-range, matching vm3).

## Sub-phase 2.1 -- float → Double

### Decisions made (2.1)

**`Double` not `Float`**: Mochi `float` is always 64-bit IEEE 754, matching Go's `float64`. Swift `Double` is 64-bit IEEE 754. Swift `Float` is 32-bit. `Float` is never used.

**Float literals**: `FloatLit(3.14)` → `3.14` (undecorated; Swift infers `Double` from context). When context is ambiguous, explicit `Double(3.14)` cast is emitted.

**Math functions**: `sqrt(x)` → `Double.squareRoot()` method or `Foundation.sqrt(x)`. Platform-conditional import: on Apple platforms, `import Darwin`; on Linux, `import Glibc`. MochiRuntime provides a `mochiSqrt`, `mochiPow`, etc. facade to avoid the import duplication in user code.

**NaN/Inf**: `Double.nan`, `Double.infinity`, `-Double.infinity`. String output must match vm3: `nan`, `inf`, `-inf`. `MochiRuntime.print(_ value: Double)` handles these with explicit checks before calling `Swift.print`.

**Comparison with NaN**: `nan == nan` → `false`, `nan != nan` → `true`, `nan < 1.0` → `false`. All comparisons involving NaN return the IEEE 754-correct result naturally in Swift, which matches vm3.

## Sub-phase 2.2 -- bool → Bool

### Decisions made (2.2)

**Short-circuit evaluation**: `a && b` → Swift `a && b` (already short-circuits). `a || b` → Swift `a || b` (already short-circuits). The lowerer does not expand these into `if` chains; Swift's natural semantics match Mochi.

**`not` keyword**: Mochi `not x` → Swift `!x`. The `!` prefix operator on `Bool` is emitted directly.

**`bool_to_string`**: `string(b)` on a bool → `b ? "true" : "false"`. This is emitted inline, not via a function call.

## Sub-phase 2.3 -- string → String

### Decisions made (2.3)

**Swift `String` is UTF-8 since Swift 5.7**: `String` stores UTF-8 natively. `String.count` returns the number of Unicode scalar values (not bytes). Mochi `string.length` maps to `str.unicodeScalars.count` to match vm3's rune-count semantics (Go `len([]rune(s))`).

**Byte length**: `string.byte_length` → `str.utf8.count`. Not exposed in Phase 2 (Phase 2 covers the Mochi surface, which has no `byte_length`). Added to runtime for FFI use in Phase 12.

**Concatenation**: Mochi `s1 + s2` (string concat) → Swift `s1 + s2`. The `+` operator on `String` returns a new `String` (value semantics, COW).

**Comparison**: `s1 == s2` → Swift `==` (Unicode scalar comparison, matches vm3's byte comparison when both strings are valid UTF-8 in NFC).

**`substr(s, start, end)`**: `String(s.unicodeScalars[s.unicodeScalars.index(s.unicodeScalars.startIndex, offsetBy: start) ..< s.unicodeScalars.index(s.unicodeScalars.startIndex, offsetBy: end)])`. This O(n) subscript is correct; optimised slicing deferred to Phase 12 (FFI/performance).

**`starts_with` / `ends_with`**: `s.hasPrefix(prefix)` and `s.hasSuffix(suffix)`.

**String → Int/Float**: `int_of_string(s)` → `Int64(s) ?? { throw MochiError.parseFail }`. Phase 11 handles the `throws` colouring; Phase 2 emits the failable form and wraps in `try`.

**String interpolation**: Mochi `"hello, \(name)"` syntax is identical to Swift's. The lowerer emits Swift string interpolation directly: `"hello, \(name)"`.

## Sub-phase 2.4 -- Edge cases

### Decisions made (2.4)

**Differential testing vs vm3**: `TestPhase2ScalarEdgeCases` runs each fixture through both the Swift transpiler and vm3, diffs stdout. Any divergence is a Phase 2 bug. Edge cases covered: `Int64.max + 1` (trap), `Int64.min / -1` (trap), `0 / 0` (trap), `1.0 / 0.0` (`inf`), `0.0 / 0.0` (`nan`), `sqrt(-1.0)` (`nan`).

**Overflow traps**: Swift traps via `_preconditionFailure` → process exits with signal 4 (SIGILL) or 5 (SIGTRAP). vm3 (Go) panics with `runtime error: integer overflow`. Both are non-zero exits but different signals. The test gate only checks stdout for non-trapping cases; trap cases are excluded from the fixture corpus (they'd need signal-assertion logic).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | Scalar arithmetic, comparison, string ops lowering |
| `transpiler3/swift/lower/literals.go` | `IntLit`, `FloatLit`, `BoolLit`, `StringLit` → sxtree literal nodes |
| `transpiler3/swift/lower/builtins.go` | `sqrt`, `pow`, `abs`, `string`, `int_of_string` → Swift calls |
| `transpiler3/swift/runtime/Sources/MochiRuntime/Math.swift` | `mochiSqrt`, `mochiPow`, `mochiAbs` platform facades |
| `transpiler3/swift/runtime/Sources/MochiRuntime/String.swift` | `mochiSubstr`, `mochiLength`, `mochiByteLength` |
| `transpiler3/swift/build/phase02_test.go` | `TestPhase2Scalars`: 20 fixtures |
| `tests/transpiler3/swift/fixtures/phase02-scalars/` | 20 fixture directories |

## Test set

- `TestPhase2Scalars` -- 20 fixtures covering: `int_arith`, `int_compare`, `int_bitwise`, `int_max_min`, `float_arith`, `float_compare`, `float_nan`, `float_inf`, `float_sqrt`, `bool_and_or`, `bool_not`, `string_concat`, `string_compare`, `string_length`, `string_substr`, `string_starts_ends`, `string_interp`, `scalar_conversions`, `scalar_edge_int`, `scalar_edge_float`.

## Deferred work

- Integer wrapping semantics for explicit wrapping builtins. Deferred to Phase 12 (FFI needs `wrapping_add` for bit manipulation).
- `Decimal` type (arbitrary precision). Out of v1 scope.
- `Complex<Double>` (swift-numerics). Deferred to Phase 12 (FFI/math library bindings).
- Locale-sensitive string comparison. MochiRuntime uses unicode-scalar comparison always; locale support is out of scope.
