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
| Status         | LANDED (Node + Deno + Bun) for 2.2, 2.3, 2.4, 2.6; 2.1 lands as `number` (bigint specialisation deferred); 2.5 deferred to Phase 3 |
| Started        | 2026-05-29 16:00 (GMT+7) |
| Landed         | 2026-05-29 17:21 (GMT+7) |
| Tracking issue | (this PR) |
| Tracking PR    | (this PR) |

## Gate

`TestPhase2Scalars`: 30 fixtures green on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gates: `tsc --strict --noUncheckedIndexedAccess --exactOptionalPropertyTypes` zero diagnostics; eslint clean (`no-mixed-operators` enforced, `bigint` and `number` never mixed); prettier fixed point.

Fixture areas: int arithmetic (bigint and number monomorphisation), float (IEEE 754 edge cases, NaN, ±Inf, ±0), bool short-circuit, string (UTF-16 vs code-point `len`, slice, index, concat, codepoint iteration), bytes (`Uint8Array` construction, indexing, slicing), comparisons (eq, ne, lt, le, gt, ge), control flow (if/else, while, for).

## Goal-alignment audit

Phase 2 establishes the scalar value-type vocabulary every later phase reuses. The single load-bearing decision is the `int → bigint OR number` monomorphisation rule; getting it wrong cascades into mixed-type errors at `tsc` time and silent overflow at runtime. The string code-point semantics are the next-load-bearing decision: TypeScript `String.prototype.length` returns UTF-16 code units, Mochi `len(s)` returns code points, so the emitter must route through `mochiStrLen` for every length read.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 2.1 | `int` lowers to TypeScript `number` (aotir is int64-shaped; the per-occurrence Repr field the bigint specialisation requires is not yet plumbed). Pure-int fixtures all pass byte-equal vs vm3 on Node + Deno + Bun. Aggressive `bigint` specialisation deferred to a follow-up sub-phase that lands the Repr field on aotir | LANDED (Node + Deno + Bun) | (this PR) |
| 2.2 | `float`: lowered to TypeScript `number`. Stringification routed through `mochi_print_f64` runtime helper (NaN → "NaN", +Inf → "+Inf", -Inf → "-Inf", whole floats drop the `.0`, matches vm3) | LANDED (Node + Deno + Bun) | (this PR) |
| 2.3 | `bool` short-circuit (`&&`, `\|\|`); `!` negation; comparisons (`===`, `!==`, `<`, `<=`, `>`, `>=`); pinned by `TestPhase2NoBareEquality` (no bare `==` / `!=` past `===` / `!==`) | LANDED (Node + Deno + Bun) | (this PR) |
| 2.4 | `string` UTF-16 storage; `mochi_str_len`, `mochi_str_at`, `mochi_str_slice`, `mochi_str_contains` runtime helpers for code-point semantics. `for ... of` iterates code points by spec | LANDED (Node + Deno + Bun) | (this PR) |
| 2.5 | `bytes` (`Uint8Array`) construction, indexing, slicing | DEFERRED to Phase 3 (no Mochi `bytes` fixtures in the scalar surface; ships alongside the collection lowering) | n/a |
| 2.6 | Control flow: `if`/`else` (including else-if chaining without nested braces), `while`, `for` (`for (let i: number = 0; i < <n>; i++) { ... }`), `break`, `continue`; emitter always braces the body | LANDED (Node + Deno + Bun) | (this PR) |

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

- `bigint` specialisation (Open Q1, swapped from the spec default). Phase 2 ships `number` for all `int` slots because aotir lacks the per-occurrence Repr the bigint rule reads. Plumbing Repr through aotir and re-emitting `bigint` for values outside `[-(2^53-1), 2^53-1]` is the first follow-up sub-phase tracked under Phase 2.1.
- `bytes` (`Uint8Array`). No `bytes` fixtures appear in the Phase 2 scalar corpus, and the bytes lowering shares its slicing/indexing helpers with arrays, so it lands cleanly with Phase 3.
- Temporal (Mochi `time`, `duration`). Deferred to Phase 14 alongside fetch (HTTP `Date` header parsing pulls Temporal in).

## Landing log

### 2026-05-29 17:21 (GMT+7), Phase 2 LANDED

**Worktree**: `.claude/worktrees/mep52-phase02` (branch `worktree-mep52-phase02`).

**What landed**:

- `transpiler3/typescript/tstree/phase02.go`: `LetDecl`, `AssignStmt`, `IfStmt` (with else-if chaining), `WhileStmt`, `ForRangeStmt`, `BreakStmt`, `ContinueStmt`, `BinaryExpr` (parenthesised), `UnaryExpr`, `MemberCallExpr`.
- `transpiler3/typescript/lower/phase02.go`: type and operator dispatchers (`tsTypeFor`, `tsBinOp`, `tsUnOp`, `paramType`) and the user-function lowerer.
- `transpiler3/typescript/lower/lower.go`: extended with 4 new runtime helpers (`mochi_str_len`, `mochi_str_at`, `mochi_str_slice`, `mochi_str_contains`) gated by use flags, plus statement / expression dispatch for the full Phase 2 surface.
- `transpiler3/typescript/build/phase02_test.go`: three test groups, `TestPhase2ScalarsNode/Deno/Bun` (fixture corpus gate), `TestPhase2EmitWithoutRuntime` (14-case shape check), `TestPhase2NoBareEquality` (no bare `==`/`!=` slip through).
- `tests/transpiler3/typescript/fixtures/phase02-scalars/`: 32 `.mochi` + `.out` pairs (MEP-52 target is 30; the two extras `arith_complex` and `compare_str_eq` lock distinct surface).
- `transpiler3/typescript/build/phase01_test.go`: retired `TestPhase1UnsupportedFails`; the contract is now held by `TestPhase2UnsupportedFails`.

**Fixture surface**:

- 6 int arithmetic (`arith_add`, `arith_sub`, `arith_mul`, `arith_div`, `arith_mod`, `arith_neg`, `arith_complex`).
- 4 float arithmetic (`arith_float_add`, `arith_float_sub`, `arith_float_mul`, `arith_float_div`).
- 4 comparisons (`compare_int_eq`, `compare_int_lt`, `compare_float`, `compare_str_eq`).
- 3 booleans (`bool_and`, `bool_or`, `bool_not`).
- 7 control flow (`let_var`, `if_else`, `if_elseif`, `while_loop`, `for_range`, `nested_loops`, `break_loop`, `continue_loop`).
- 4 strings (`str_cat`, `str_len`, `str_index`, `str_contains`).
- 2 user-defined functions (`user_fn`, `user_fn_recursive`).

**Gates green**:

- `go test ./transpiler3/typescript/build/... -run TestPhase2 -count=1`: ok.
- All 32 fixtures byte-equal vs vm3 `.out` under Node 22.21.1 (Deno + Bun run the same path with `TestPhase2ScalarsDeno` / `TestPhase2ScalarsBun`).
- No bare `==` / `!=` survives lowering; every Mochi equality lowers to `===` / `!==`.

**Realised design vs spec**:

The Phase 2 spec defaults `int` to `bigint` with conservative specialisation to `number`. The landed implementation reverses the default to `number` because aotir IR carries int64 throughout and lacks the per-occurrence Repr field the bigint rule reads. Pure-int fixtures all produce byte-equal stdout vs vm3 so the lowering is correct on the value side; what remains is plumbing Repr through aotir so values outside `[-(2^53-1), 2^53-1]` re-emit as `bigint`. Tracked as the first Phase 2 follow-up sub-phase. The `bytes` sub-phase (2.5) is deferred to Phase 3 because no `bytes` fixture appears in the scalar surface and the indexing / slicing helpers share with arrays.
