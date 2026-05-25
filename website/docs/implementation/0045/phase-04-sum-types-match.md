---
title: "Phase 4. Sum types and Maranget pattern matching"
sidebar_position: 6
sidebar_label: "Phase 4. Sum types + match"
description: "MEP-45 Phase 4 tracking: tagged-union sum types, match-as-expression and match-as-statement lowering."
---

# Phase 4. Sum types and Maranget pattern matching

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 4](/docs/mep/mep-0045#phase-4-sum-types-and-maranget-pattern-matching) |
| Status         | IN PROGRESS |
| Started        | 2026-05-25 15:48 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

14-fixture suite under `tests/transpiler3/c/fixtures/sum_types/`: scalar-field variants (int, float, bool, string), unit variants, functions taking/returning union types, match-as-expression (producing int/float/bool/string results), match-as-statement (print calls in arms), wildcard arms, multiple matches on the same union in one scope, two union types co-existing. All 14 fixtures compile + run byte-equal vs vm3. `TestPhase4SumTypes` gate is green.

## Goal-alignment audit

Sum types and pattern matching are core to idiomatic Mochi. Without them the transpiler cannot handle any realistic Mochi program that uses `option<T>`, `result<T,E>`, or any user-defined variant type. Phase 4.0 unlocks all of these. Aligns directly with the user-facing goal.

## Sub-phases

| #   | Scope | Status | Commit | PR |
|-----|-------|--------|--------|----|
| 4.0 | Sum-type lowering: `TypeUnion` aotir type; `struct pkg_S { uint8_t tag; union { ... } u; }` C representation with inline variant constructors; `VariantLit`, `UnionVarRef`, `VariantFieldAccess`, `MatchStmt` IR nodes in `aotir/program.go`; verifier, lower, emit passes updated; match-as-expression (result-var pre-declaration with nil Init, assignment in arms) + match-as-statement (unit-returning print/call in arms); `CallExpr.ResultUnionName` added to propagate union identity through function return bindings; `TestPhase4SumTypes` gate (14 fixtures). Scalar primitive variant fields only (int, float, bool, string). Unit variants supported. | LANDED 2026-05-25 15:48 (GMT+7) | — | — |
| 4.1 | Maranget decision-tree pass: `transpiler3/c/lower/match.go` canonicalizes `MatchStmt` arms (unique-tag validation, ascending-tag sort); for single-column patterns the pass confirms the already-optimal `switch(tag)` structure; `TestPhase4Maranget` gate reuses the 14 Phase 4.0 sum-type fixtures | LANDED 2026-05-25 21:08 (GMT+7) | — | — |
| 4.2 | Exhaustiveness check at type-check time: enforced by shared type checker (error T050) before lower; defense-in-depth `default: mochi_panic_index()` in emitted C switch | LANDED 2026-05-25 20:59 (GMT+7) | — | — |
| 4.3 | Property test: `theft`-generated random pattern set decides identically to reference naive matcher (10000 cases per CI run) | NOT STARTED | — | — |

## Decisions made

**C representation for unions.** Each Mochi union `type S = A(x: int) | B(y: string)` emits:

```c
typedef struct pkg_S {
    uint8_t tag;
    union {
        struct { int64_t x; } A;
        struct { const char * y; } B;
    } u;
} pkg_S;

static inline pkg_S pkg_S_A(int64_t x) {
    pkg_S __v; __v.tag = 0; __v.u.A.x = x; return __v;
}
static inline pkg_S pkg_S_B(const char * y) {
    pkg_S __v; __v.tag = 1; __v.u.B.y = y; return __v;
}
```

**Match lowering.** Each `match` lowers to a C `switch` on the tag, wrapped in its own `{}` block to prevent name collision when the same union is matched multiple times in one C scope:

```c
{
    const pkg_S __mochi_match_S = expr;
    switch (__mochi_match_S.tag) {
        case 0: { const int64_t x = __mochi_match_S.u.A.x; ...; break; }
        case 1: { const char * y = __mochi_match_S.u.B.y; ...; break; }
        default: { ...; break; }
    }
}
```

**Match-as-expression.** The lowerer pre-declares a mutable result variable (`LetStmt` with `Mutable=true, Init=nil`) before the `MatchStmt`. Each arm assigns into it. The verifier was updated to allow `nil Init` on mutable `LetStmt` nodes. The emitter emits the uninitialized C declaration, then the switch assigns the result in each arm.

**Union identity propagation.** The parallel-field pattern used for records (`RecordName`) and lists (`ElemType`) was extended to union types via `UnionName` on `LetStmt`, `Param`, `Function`, and a new `ResultUnionName` on `CallExpr`. This allows the lower pass to track which union a variable belongs to across function call boundaries.

**String field double-const fix.** Variant fields of type `string` store as `const char *`. The arm binding emitter was updated to detect when the C type already starts with `const ` and not add another `const ` prefix (which produced `const const char *`, a compilation warning).

## Bug fixes in this phase

- `inferMatchResultType`: speculative type inference for match-as-expression now injects pattern-variable bindings (from the union decl) into the temporary scope before lowering the arm result expression. Previously, `Circle(r) => r * r` would fail because `r` was not in scope, causing fallback to the target's union type and mislabeling the result as `TypeUnion` instead of `TypeInt`.
- `exprUnionName(CallExpr)`: previously returned `""` for all `CallExpr` nodes. Added `ResultUnionName` field to `CallExpr` so functions returning union types propagate identity into `let` bindings.
- `emitMatchStmt`: previously used a flat `const pkg_S __mochi_match_S = ...` declaration, which collides when two `match` statements on the same union appear in the same C scope. Fixed by wrapping each match in `{}`.
- `lowerMatchBodyWithScope` (statement-position arms): previously tried to lower `print(v)` as an expression via `lowerExpr`, which rejected it (`print() returns unit`). Fixed by routing through `lowerExprStmt` when `resultVar == ""`.

## Phase 4.1: Maranget pass decisions

**Goal alignment.** The user-facing goal is correct, deterministic code generation for `match` expressions. For Mochi's current single-column pattern language (one tag per `match`), the Maranget (2008) optimal decision tree is trivially `switch(tag)` — already produced by `emitMatchStmt`. Phase 4.1 adds the validation and canonicalization layer so arm ordering is deterministic regardless of source order.

**Why sort by ascending tag.** The variant tag is the `uint8_t` field emitted as `case N:` in C. Sorting arms ascending puts lower tags first, matching source-declaration order (tags are assigned by the lowerer in declaration order). This makes the emitted switch easier to read and gives the C compiler + branch predictor a clean, predictable sequence.

**Duplicate-tag detection.** `canonicalizeMatchStmt` validates that no two arms share the same tag. This catches lowerer bugs early (a duplicate tag would produce unreachable `case N:` code in C). The type checker already rejects duplicate variants at the source level, so this is defense-in-depth for the IR layer.

**No new IR nodes.** Phase 4.1 keeps `MatchStmt` as the representation after the pass. The emitter continues to handle `MatchStmt` directly. A future multi-column pattern extension would introduce `SwitchOnTagStmt` / `CaseStmt` IR nodes if the emitter became too complex; for now the single-pass approach is simpler.

**Gate.** `TestPhase4Maranget` in `build/phase04_1_test.go` runs `runFixtureSuite(t, "sum_types")` — the same 14 fixtures as Phase 4.0. All 14 pass byte-equal after the Maranget pass is wired in, confirming the pass is a correct no-op for well-formed programs.

## Deferred work

- Recursive variant fields (variants whose field type references the same union) are rejected in Phase 4.0 by `scalarVariantFieldType`. Phase 4.1+ will add boxing for recursive variants.
- Niche optimisation for `?T` (null = None) deferred.
- Maranget decision-tree optimisation (Phase 4.1): current lowering uses a simple `switch` on tag, which is already optimal for non-nested patterns.
- GADT support: not in v1.

## Closeout notes

_Fill in after gate fully green (all 4 sub-phases)._
