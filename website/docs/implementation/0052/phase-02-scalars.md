---
title: "Phase 2. Scalars"
sidebar_position: 3
sidebar_label: "Phase 2. Scalars"
description: "MEP-52 Phase 2, int via bigint/number monomorphisation, float, bool, string (UTF-16 vs code-point semantics), bytes, all comparison and arithmetic operators; 30 fixtures."
---

# Phase 2. Scalars

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 2](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase2Scalars`: 30 fixtures green on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gates: `tsc --strict --noUncheckedIndexedAccess --exactOptionalPropertyTypes` zero diagnostics; eslint clean (`no-mixed-operators` enforced, `bigint` and `number` never mixed); prettier fixed point.

Fixture areas: int arithmetic (bigint and number monomorphisation), float (IEEE 754 edge cases, NaN, ±Inf, ±0), bool short-circuit, string (UTF-16 vs code-point `len`, slice, index, concat, codepoint iteration), bytes (`Uint8Array` construction, indexing, slicing), comparisons (eq, ne, lt, le, gt, ge), control flow (if/else, while, for).

## Goal-alignment audit

Phase 2 establishes the scalar value-type vocabulary every later phase reuses. The single load-bearing decision is the `int → bigint OR number` monomorphisation rule; getting it wrong cascades into mixed-type errors at `tsc` time and silent overflow at runtime. The string code-point semantics are the next-load-bearing decision: TypeScript `String.prototype.length` returns UTF-16 code units, Mochi `len(s)` returns code points, so the emitter must route through `mochiStrLen` for every length read.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 2.1 | `int` monomorphisation: default `bigint`, specialise to `number` when IR proves the value fits in [-(2^53-1), 2^53-1] and the producer never overflows; never mix in one expression | NOT STARTED | n/a |
| 2.2 | `float`: IEEE 754 binary64, NaN / ±Inf / ±0 stringification matches vm3 (`strconv.FormatFloat(f, 'g', -1, 64)`) | NOT STARTED | n/a |
| 2.3 | `bool` short-circuit (`&&`, `\|\|`); `!` negation; comparisons (`===`, `!==`, `<`, `<=`, `>`, `>=`) | NOT STARTED | n/a |
| 2.4 | `string` UTF-16 internal storage; `mochiStrLen(s)`, `mochiStrAt(s, i)`, `mochiStrSlice(s, a, b)` runtime helpers for code-point semantics | NOT STARTED | n/a |
| 2.5 | `bytes` (`Uint8Array`) construction, indexing (`u8[i]` returns `number | undefined` under `--noUncheckedIndexedAccess`, runtime-guarded), slicing | NOT STARTED | n/a |
| 2.6 | Control flow: `if`/`else`, `while`, `for` (numeric range and for-of), `break`, `continue`; lowering preserves SSA-like structure for `tsc --strict` to type-narrow | NOT STARTED | n/a |

## Sub-phase 2.1, int via bigint or number

### Decisions made (2.1)

**Default representation**: `bigint`. Mochi `int` is arbitrary precision; `bigint` is the only TypeScript primitive that matches.

**Specialisation to `number`**: monomorphisation specialises a per-occurrence IR type to `number` when:

1. The static type is bounded such that all values fit in `[-9007199254740991, 9007199254740991]` (`Number.MAX_SAFE_INTEGER`); for example a loop counter ranged `for i in 0..len(xs)`.
2. The producer never overflows (no `*`, `**`, `<<`, `+`, `-` that might exceed the bound; arithmetic that proves safe via range analysis stays as `number`).
3. All consumers also use `number` (no flow into a `bigint` slot).

If any of these fails the whole flow falls back to `bigint`. The IR carries a `Repr` field per integer type so the emitter never has to re-derive.

**Literal suffix**: `bigint` literal is `42n`; `number` literal is `42`. The emitter emits the suffix consistently. Mixing in a single expression is a `tsc` error (`Operator '+' cannot be applied to types 'bigint' and 'number'`); the emitter refuses to emit such a mixture (would indicate a monomorphisation bug).

**Division**: Mochi `/` on integers is floor division, on floats is IEEE division. `bigint` `/` in TypeScript is truncated toward zero, which matches Mochi for non-negative operands but diverges for negative; the emitter routes `a / b` for `int` through a `mochiBigDiv(a, b)` runtime helper that adjusts the rounding for negatives.

## Sub-phase 2.2, float

### Decisions made (2.2)

**Type**: `number` (IEEE 754 binary64).

**Stringification**: vm3 prints `42.0` as `42` and `3.14` as `3.14`; the emitter uses `mochiFloatStr(f)` which is `String(f)` for finite non-zero values, then handles edge cases:

- `NaN` → `"NaN"`
- `+Infinity` → `"Infinity"`
- `-Infinity` → `"-Infinity"`
- `+0`, `-0` → `"0"` (vm3 drops the sign)
- Whole-valued floats (`42.0`) → `"42"` (matches `String(42.0)`)

For arithmetic operators (`+`, `-`, `*`, `/`, `%`) Mochi `float` lowers to the TypeScript primitive operators directly. NaN-propagation matches the host runtime, which is uniform across V8, SpiderMonkey, and JavaScriptCore on the IEEE 754 contract.

## Sub-phase 2.3, bool

### Decisions made (2.3)

**Type**: `boolean`.

**Short-circuit**: Mochi `a && b` and `a || b` lower to TypeScript `a && b` and `a || b`. TypeScript's logical operators are short-circuit by spec.

**Negation**: `!a` lowers to `!a`.

**Comparisons**: Mochi `==` and `!=` lower to `===` and `!==` (the emitter never uses `==`/`!=` because of their coercion rules, which fail `tsc --strict` lint via `@typescript-eslint/eqeqeq: error`). Ordering operators (`<`, `<=`, `>`, `>=`) lower directly.

**Coercion**: Mochi never coerces bool to int. The TypeScript runtime would happily coerce (`Number(true) === 1`) but the type system catches at compile time.

## Sub-phase 2.4, string

### Decisions made (2.4)

**Storage**: `string` (UTF-16 code units internally).

**Length**: `len(s)` is code points, not code units. The emitter emits `mochiStrLen(s)`, a runtime helper:

```typescript
// @mochi/runtime/string
export function mochiStrLen(s: string): bigint {
  let n = 0n;
  for (const _ of s) n++;
  return n;
}
```

(`for ... of` over a string iterates code points by spec, not code units.) The return type is `bigint` because `len` returns Mochi `int`, which defaults to `bigint`. Monomorphisation specialises to `number` if all consumers tolerate it.

**Indexing**: `s[i]` in Mochi is `mochiStrAt(s, i)`, which iterates `i + 1` code points and returns the last one as a length-1-or-2 string (a surrogate pair is one code point even though it occupies two UTF-16 units).

**Slicing**: `s[a:b]` is `mochiStrSlice(s, a, b)`, which advances by code point and returns the corresponding substring.

**Concatenation**: `a + b` lowers to `a + b` directly. UTF-16 concatenation is safe at the boundary because any well-formed UTF-16 prefix concatenated with any well-formed UTF-16 suffix is well-formed UTF-16 (no lone surrogates introduced).

**String literals**: emitted as double-quoted with `\xNN`, `\uNNNN`, `\u{NNNNNN}` for non-printable characters. The emitter prefers `\u{...}` over surrogate pairs for code points above U+FFFF (`\u{1F600}` rather than `😀`).

## Sub-phase 2.5, bytes

### Decisions made (2.5)

**Type**: `Uint8Array`.

**Literal**: a `bytes` literal `b"\x00\x01\x02"` lowers to `new Uint8Array([0x00, 0x01, 0x02])`.

**Indexing**: `b[i]` lowers to `mochiBytesAt(b, i)`, which performs the bounds check that Mochi requires and is needed under `--noUncheckedIndexedAccess` anyway (`b[i]` typed as `number | undefined`).

**Slicing**: `b[a:b]` lowers to `b.slice(a, b)` (fresh array, matches Mochi independence semantics).

**No `Buffer`**: Node's `Buffer` is a `Uint8Array` subclass with extra encoding helpers, but it is Node-specific. The emitter never uses `Buffer`; `TextEncoder` / `TextDecoder` cover UTF-8 needs cross-runtime.

## Sub-phase 2.6, control flow

### Decisions made (2.6)

**`if`/`else`**: lower direct. The emitter always emits braces (`{ ... }`) for the body, even single-statement; `--strict` rules and prettier prefer braced bodies.

**`while`**: lower direct.

**`for i in 0..n`**: lowers to either `for (let i = 0n; i < <n>; i++)` (bigint) or `for (let i = 0; i < <n>; i++)` (number). The IR's monomorphised type for `i` drives the choice.

**`for x in xs`**: lowers to `for (const x of xs)` for arrays, sets, and iterators; `for (const [k, v] of m)` for maps.

**`break`, `continue`**: direct.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/scalars.go` | int/float/bool/string/bytes literal and operator lowering |
| `transpiler3/typescript/lower/monomorphise.go` | int Repr choice (bigint vs number) per occurrence |
| `transpiler3/typescript/lower/controlflow.go` | if/else, while, for, break, continue lowering |
| `runtime3/typescript/src/string/index.ts` | `mochiStrLen`, `mochiStrAt`, `mochiStrSlice` |
| `runtime3/typescript/src/numeric/index.ts` | `mochiBigDiv`, `mochiFloatStr` |
| `runtime3/typescript/src/bytes/index.ts` | `mochiBytesAt`, `mochiBytesSlice` |
| `transpiler3/typescript/build/phase02_test.go` | `TestPhase2Scalars` |
| `tests/transpiler3/typescript/fixtures/phase02-scalars/` | 30 fixture directories |

## Test set

- `TestPhase2Scalars`, 30 fixtures across the six areas, four-runtime execution.
- `TestPhase2NoMixedNumeric`, asserts no emitted `.ts` file contains `bigint` and `number` in the same expression.
- `TestPhase2StringCodepoints`, fixture exercises emoji (U+1F600) where `len` must equal 1, not 2.

## Deferred work

- `bigint` to `number` aggressive defaulting (Open Q1). Phase 2 ships the conservative rule.
- Temporal (Mochi `time`, `duration`). Deferred to Phase 14 alongside fetch (HTTP `Date` header parsing pulls Temporal in).
