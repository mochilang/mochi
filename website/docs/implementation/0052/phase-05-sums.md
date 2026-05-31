---
title: "Phase 5. Sum types"
sidebar_position: 6
sidebar_label: "Phase 5. Sum types"
description: "MEP-52 Phase 5, Mochi sum types lowered to TypeScript discriminated unions over a literal kind tag with exhaustive switch enforced by tsc strict; match-to-switch-tag lowering; 24 fixtures across Node 22, Deno 2, Bun 1.1."
---

# Phase 5. Sum types

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 5](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (Node + Deno + Bun); 5.3 (pattern guards) + 5.4 (record-payload variants) deferred |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 21:24 (GMT+7) |
| Tracking issue | (this PR) |
| Tracking PR    | (this PR) |

## Gate

`TestPhase5Sums{Node,Deno,Bun}`: 24 fixtures byte-equal on Node 22, Deno 2, Bun 1.1. Floor is 20; shipped 24. Plus four shape gates: `TestPhase5EmitShape` (load-bearing tokens per surface), `TestPhase5WildcardSkipsUnreachable` (wildcard arm suppresses the never-witness), `TestPhase5UnreachableEmittedOnce` (helper deduped per program), `TestPhase5MatchTempIsLocal` (scrutinee capture is local to a block, counter is per-function and byte-stable).

Browser (Phase 17) is out of scope here; the literal-tag discriminated union form is browser-clean and will fold into the Phase 17 bundle without rework.

## Goal-alignment audit

Sum types are how Mochi expresses tagged choice. TypeScript has no native sum type, but discriminated unions over a literal tag are the canonical pattern (Microsoft promotes it as the recommended way to model sum types since TS 2.0). MEP-52 commits to `type Foo = A | B | C` over a literal `kind` tag with exhaustive `switch (x.kind)` plus a `default: mochiUnreachable(x)` witness call. The match-to-decision-tree pass shared with the C path emits flat switch arms keyed on the literal `kind` string.

Phase 5 is load-bearing for Phase 6 (closures want match in higher-order callbacks), Phase 7 (query DSL wants match in `where` predicates that dispatch on row-tag types), and Phase 8 (datalog wants match over fact-shape unions in stratified rules). Every later phase that introduces destructuring reads from this surface, so it has to land tsc-strict-clean across all three runtimes before the rest of the stack composes.

## Sub-phases

| #   | Scope | Status | Commit |
|-----|-------|--------|--------|
| 5.0 | Sum declaration to `type` alias over `{readonly kind: "A"; ...} \| {readonly kind: "B"; ...}`; unit variants `{readonly kind: "C"}` | LANDED | (this PR) |
| 5.1 | Match-to-switch lowering: scrutinee captured in `__mochi_match_<n>` inside a `BlockStmt`, pattern bindings as `const` reads, arms as labelled `case`s | LANDED | (this PR) |
| 5.2 | Exhaustiveness: `mochiUnreachable(x: never): never` in the `default` arm of every exhaustive switch; wildcard arms opt out | LANDED | (this PR) |
| 5.3 | Pattern guards: `match x { A(n) when n > 0 => ... }` | DEFERRED (no C-side support yet; will land alongside the C transpiler's guard work) | n/a |
| 5.4 | Sum types with record payloads (`Either { Left(err: ParseError), Right(value: User) }`) | DEFERRED (current corpus exercises scalar payloads, which compose with Phase 4 records once a fixture demands it; no surface needs it for Phase 6 or Phase 7 entry) | n/a |

## Sub-phase 5.0, Sum to discriminated union

### Decisions made (5.0)

**Mochi**: `type Shape = Circle(r: int) | Square(side: int)`

**TypeScript (as shipped)**:

```typescript
type Shape =
  | { readonly kind: "Circle"; readonly r: number }
  | { readonly kind: "Square"; readonly side: number };
```

**Variant constructor (as shipped)**: a constructor call like `Circle(5)` lowers to an inline object literal widened back to the union with an `as` cast at the call site, not a factory function:

```typescript
const s: Shape = { kind: "Circle", r: 5 } as Shape;
```

The `as Shape` is load-bearing. Without it, tsc's const-aliasing control-flow analysis narrows the initialiser's static type down to the singleton literal `{kind:"Circle"; r:5}`, after which a subsequent `case "Square"` arm fails TS2678 ("Type '"Square"' is not comparable to type '"Circle"'") even though the runtime would be correct. The cast widens the slot back to the union so downstream match arms type-check.

Why inline construction rather than `const Circle = (r: number): Shape => ({...})` arrow factories: every variant site already has the union type available at the construction site (from the let slot, the parameter type, or the return type), so the cast costs no extra tokens and lets the emit stay byte-stable without an upstream symbol-table pass that places factories ahead of first use. Factory functions remain a viable alternative if Phase 14 (fetch) or Phase 17 (browser bundle) ever needs an exported constructor; they are not needed for Phase 5's surface.

**`readonly` on every field**: surfaces Mochi's immutable-by-default semantics in the type system. Nothing in the emit ever mutates a variant value in place.

**Unit variants**: a variant with no fields, e.g. `North` in `type Dir = North | South | East | West`, lowers to `{readonly kind: "North"}` and constructs as `{ kind: "North" } as Dir`.

## Sub-phase 5.1, Match to switch

### Decisions made (5.1)

**Mochi**: `match s { Circle(r) => r * r  Square(side) => side * side }`

**TypeScript (as shipped)**:

```typescript
let __match1!: number;
{
  const __mochi_match_1: Shape = s;
  switch (__mochi_match_1.kind) {
    case "Circle": {
      const r: number = __mochi_match_1.r;
      __match1 = (r * r);
      break;
    }
    case "Square": {
      const side: number = __mochi_match_1.side;
      __match1 = (side * side);
      break;
    }
    default: {
      mochiUnreachable(__mochi_match_1);
      break;
    }
  }
}
const area: number = __match1;
```

**Scrutinee capture into `__mochi_match_<n>`**: the match wraps its switch in a `BlockStmt` and reads the scrutinee once into a fresh local. Two matches in the same function get distinct counter values (`__mochi_match_1`, `__mochi_match_2`, ...); the counter resets per function so byte-equal regenerated emits stay stable. The block scopes the temp so it doesn't leak.

**`let __matchN!: T;` definite-assignment assertion**: the C lowerer emits the result slot as a mutable `LetStmt` with no initialiser (so each `case` arm assigns into it). TypeScript's CFA can't see across `switch` arms into the assignments, so the only way to keep the emit tsc-strict-clean without sacrificing immutability elsewhere is to declare the slot with `!` and let the exhaustiveness witness in `default:` (or the wildcard arm) guarantee at least one arm runs.

**Pattern bindings as `const` reads**: `Circle(r) => r * r` lowers to `const r: number = __mochi_match_1.r;` inside the `case "Circle":` block, then uses `r` in the arm body. Two arms can introduce the same binding name with different field types because each arm has its own block.

**`break;` rather than `return`**: the match emit is uniform whether the match sits in expression position (assigns the result var) or statement position (runs side effects). `break` keeps the lowering one shape; `return` would only work for expression-position matches inside functions whose return type matches the match type.

## Sub-phase 5.2, Exhaustiveness

### Decisions made (5.2)

**`mochiUnreachable` runtime helper (as shipped)**:

```typescript
function mochiUnreachable(x: never): never {
  throw new Error("mochi: unreachable match arm");
}
```

The helper is emitted inline at the top of the generated file, exactly once per program that contains an exhaustive match. The `unreachable bool` runtime flag in the lowerer tracks whether any match site asks for it; programs that only use wildcard matches don't pay for the helper.

**Use site**: every exhaustive match (no `_` arm) emits `default: { mochiUnreachable(__mochi_match_<n>); break; }`. If a future code change adds a variant to a union without adding a case, tsc rejects the `mochiUnreachable` call at the witness site because `__mochi_match_<n>` widens past `never` — surfacing the missed arm at compile time, before any runtime hit.

**Wildcard opt-out**: a match with a `_ => expr` arm skips the witness call entirely; the wildcard's body becomes the `default:` arm body. The user has explicitly asked for non-exhaustive matching and accepts that adding a variant won't trip tsc. Mixing `_` with named arms works the same way (`req_wildcard_mixed.mochi`).

**Why not omit the `default`**: TypeScript's `--noFallthroughCasesInSwitch` enforces fallthrough only, not exhaustiveness. Without `default + mochiUnreachable`, a missing variant compiles fine and crashes at runtime when the missing branch is hit. The witness pattern is the standard idiom and the emitter always emits it for exhaustive matches.

## Sub-phase 5.3, Pattern guards (DEFERRED)

The C transpiler does not yet ship guard support, so MEP-52 cannot ship guards without first picking up that work. Guards (`match x { A(n) when n > 0 => ... }`) are not on Phase 6 or Phase 7's critical path; deferred until a fixture demands them.

## Sub-phase 5.4, Sum with record payloads (DEFERRED)

The current corpus exercises scalar payload fields (`int`, `float`, `bool`, `string`) and the typing surface composes cleanly with Phase 4 records once a fixture introduces a record-payload variant. Deferred until Phase 6 or Phase 7 surfaces such a fixture; no new code change is anticipated since the lowering already reads `UnionVarRef.UnionName` and the type computation already handles `aotir.TypeUnion`.

## Files (as shipped)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/phase05.go` | `lowerMatchStmt`, `lowerVariantLit`, `lowerUnionDecl`; counter reset per function; `mochiUnreachable` helper emission gated on `runtimeFlags.unreachable` |
| `transpiler3/typescript/tstree/phase05.go` | `BlockStmt` (containing block for scrutinee capture) + `TypeAssertExpr` (`as UnionName` widening) |
| `transpiler3/typescript/lower/phase02.go` | parameter / return type plumbing now passes `UnionName` into `tsTypeForLetSlot` |
| `transpiler3/typescript/lower/phase03.go` | `tsTypeForLetSlot` accepts `unionName string` and handles `aotir.TypeUnion` via `tsTypeForUnion` |
| `transpiler3/typescript/tstree/phase02.go` | `LetDecl` print supports nil `Init` via the definite-assignment-assertion form `let NAME!: TYPE;` |
| `transpiler3/typescript/build/phase05_test.go` | `TestPhase5SumsNode/Deno/Bun`, `TestPhase5EmitShape`, `TestPhase5WildcardSkipsUnreachable`, `TestPhase5UnreachableEmittedOnce`, `TestPhase5MatchTempIsLocal` |
| `tests/transpiler3/typescript/fixtures/phase05-sums/` | 24 fixtures with vm3-recorded `.out` |

## Test set (as shipped)

- `TestPhase5SumsNode`, `TestPhase5SumsDeno`, `TestPhase5SumsBun`: 24 fixtures byte-equal each, 72 sub-tests total.
- `TestPhase5EmitShape`: four representative fixtures, asserts `type Shape =`, `readonly kind: "Circle"`, `as Shape`, `switch (__mochi_match_`, `mochiUnreachable(`, and the helper declaration form.
- `TestPhase5WildcardSkipsUnreachable`: `req_wildcard.mochi` must not emit a `mochiUnreachable(__mochi_match_` call (user opted out of exhaustiveness).
- `TestPhase5UnreachableEmittedOnce`: four multi-match fixtures, each must emit exactly one `function mochiUnreachable(` declaration.
- `TestPhase5MatchTempIsLocal`: `req_multi_match.mochi` must capture into `__mochi_match_1` and `__mochi_match_2` (per-function counter is byte-stable).

## Fixture corpus (24)

Surface area coverage:

| Fixture | Surface area |
|---------|--------------|
| `req_circle_square` | Two-arm payload variants with int field |
| `req_square_match` | Single non-default-variant binding |
| `req_bool_field` | `bool` payload field |
| `req_float_field` | `float` payload field |
| `req_string_field` | `string` payload field |
| `req_mixed_field_types` | Four variants, each with a different field type (int / string / bool / unit) |
| `req_function_param` | Sum as function parameter type, match in function body |
| `req_function_return` | Sum as function return type (Result-style Ok/Err) |
| `req_match_in_function` | Multi-arm match inside a function body |
| `req_unit_enum` | Three-arm exhaustive over unit-only variants |
| `req_multi_match` | Two distinct matches in one program (counter reset, helper dedup) |
| `req_nested_match` | Match in the arm of another match |
| `req_two_unions` | Two distinct sum types in one program |
| `req_let_then_match` | Match where scrutinee is a let-bound local |
| `req_wildcard` | Wildcard arm only — no witness call |
| `req_wildcard_mixed` | Wildcard mixed with constructor arms |
| `req_match_returns_bool` | Match returning bool through a function |
| `req_match_in_expr` | Match as RHS of `let` |
| `req_match_stmt_units` | Match in statement position (side-effecting arms) |
| `req_local_double_match` | Two consecutive matches sharing the same scrutinee |
| `req_three_arm_exhaustive` | Three-arm exhaustive on payload variants through fn call |
| `req_match_string_via_int` | Multi-variant carrying int field, returning string |
| `req_neg_path` | Negative-int arm result |
| `req_match_in_loop` | Match in `for ... in` loop body, classifier pattern |

## Goal-alignment audit (passed)

The Phase 5 gate moves the user-facing goal: a Mochi program that uses sum types and `match` lowers to TypeScript that runs byte-equal under all three Node 22, Deno 2, Bun 1.1 runtimes, with tsc-strict-clean exhaustiveness via the `mochiUnreachable` witness. The two deferred sub-phases (5.3 guards, 5.4 record-payload variants) do not block Phase 6 (closures) or Phase 7 (query DSL) entry; they will land if a downstream fixture demands them.

## Deferred work

- Pattern guards (5.3): no C-side support yet.
- Record-payload variants (5.4): no fixture demands it; lowering already plumbs `UnionName` through type computation, so the work is fixture-only when it lands.
- Or-patterns (`A | B => ...`): not in Mochi surface; not blocked here.
- View patterns / pattern synonyms: not in MEP-52 scope.
- Browser target: deferred to Phase 17 (the literal-tag emit is browser-clean by construction).
