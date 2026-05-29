---
title: "Phase 5. Sum types"
sidebar_position: 6
sidebar_label: "Phase 5. Sum types"
description: "MEP-52 Phase 5, Mochi sum types lowered to TypeScript discriminated unions over a literal kind tag with exhaustive switch enforced by tsc strict; match-to-switch-tag lowering; 40 fixtures."
---

# Phase 5. Sum types

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 5](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase5Sums`: 40 fixtures green on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gate: `tsc --noFallthroughCasesInSwitch` enforced; every `switch (x.kind)` either has all variants present (exhaustive) or an explicit default that calls `mochiUnreachable(x)`, whose parameter type is `never`. Compile-time exhaustiveness checking is the headline win.

## Goal-alignment audit

Sum types are how Mochi expresses tagged choice. TypeScript has no native sum type, but discriminated unions over a literal tag are the canonical pattern (Microsoft promotes it as the recommended way to model sum types since TS 2.0). MEP-52 commits to `type Foo = A | B | C` over a literal `kind` tag with exhaustive `switch (x.kind)` enforced by the `--strict` flag bundle plus `--noFallthroughCasesInSwitch`. The match-to-decision-tree pass shared with MEP-45 through MEP-51 maps cleanly onto `switch` because the discriminator is the literal `kind` string.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 5.0 | Sum declaration to `type` alias over `{kind: "A"; ...} \| {kind: "B"; ...}`; variant constructors as factory functions | NOT STARTED | n/a |
| 5.1 | Match-to-switch lowering: `match x { A(a) => ... B(b) => ... }` to `switch (x.kind) { case "A": ... case "B": ... }` with type-narrowing | NOT STARTED | n/a |
| 5.2 | Exhaustiveness: `mochiUnreachable(x: never): never` in the default arm of every emitted switch; tsc rejects unhandled variants | NOT STARTED | n/a |
| 5.3 | Nested patterns and guards: `match x { A(B(b)) => ... A(C(_)) if b > 0 => ... }` lowered through the decision-tree pass | NOT STARTED | n/a |
| 5.4 | Sum types with payload records (`A(User)` where `User` is a Phase 4 record) | NOT STARTED | n/a |

## Sub-phase 5.0, Sum to discriminated union

### Decisions made (5.0)

**Mochi**: `sum Shape { Circle(r: float), Square(s: float), Triangle(a: float, b: float, c: float) }`

**TypeScript**:

```typescript
// src/generated/shape.ts
export type Shape =
  | { readonly kind: "Circle"; readonly r: number }
  | { readonly kind: "Square"; readonly s: number }
  | { readonly kind: "Triangle"; readonly a: number; readonly b: number; readonly c: number };

export const Circle = (r: number): Shape => ({ kind: "Circle", r });
export const Square = (s: number): Shape => ({ kind: "Square", s });
export const Triangle = (a: number, b: number, c: number): Shape =>
  ({ kind: "Triangle", a, b, c });
```

**Why type-alias-with-literal-tag rather than class hierarchy**: a class hierarchy (`abstract class Shape; class Circle extends Shape; ...`) requires `instanceof` checks at match time, which TypeScript can narrow but only via `if (x instanceof Circle)` chains, not `switch`. The literal-tag form gives `switch (x.kind) { case "Circle": ... }` with `case "Circle":` narrowing `x` to the `Circle` variant automatically. This is the idiomatic TS pattern for sum types and is the form the Microsoft handbook recommends.

**`readonly kind`**: the tag is `readonly` so the type-narrowing in the `case` arm is sound across mutation (no observer can mutate `x.kind` after narrowing).

**Constructor functions**: `Circle`, `Square`, `Triangle` are arrow functions, not classes. They are cheaper at construction (no allocation overhead beyond the object literal) and they sit naturally next to the type alias.

## Sub-phase 5.1, Match to switch

### Decisions made (5.1)

**Mochi**: `match s { Circle(r) => 3.14159 * r * r, Square(s) => s * s, Triangle(a, b, c) => heron(a, b, c) }`

**TypeScript**:

```typescript
function area(s: Shape): number {
  switch (s.kind) {
    case "Circle":   return 3.14159 * s.r * s.r;
    case "Square":   return s.s * s.s;
    case "Triangle": return heron(s.a, s.b, s.c);
  }
}
```

**Type narrowing**: TypeScript's flow analysis narrows `s` to `{kind: "Circle"; r: number}` inside `case "Circle":`, making `s.r` typed as `number` without an assertion. This is the canonical TS discriminated-union pattern and is well-supported by `tsc --strict`.

**Decision-tree pass**: the IR-level match-to-decision-tree pass (shared with MEP-45 through MEP-51) already linearises nested patterns into a sequence of single-level tests; the TypeScript emitter only sees flat patterns by the time it gets called.

**Bindings**: variant-binding patterns (`Circle(r) => ...`) lower to inline field reads (`s.r`), not local bindings. The Mochi name `r` survives only if the IR pass kept it as an internal alias; otherwise the emitter inlines.

## Sub-phase 5.2, Exhaustiveness

### Decisions made (5.2)

**`mochiUnreachable` runtime helper**:

```typescript
// @mochi/runtime/exhaustiveness
export function mochiUnreachable(x: never): never {
  throw new Error(`unreachable: ${JSON.stringify(x as unknown)}`);
}
```

**Use site**: the emitter always emits a `default` arm that calls `mochiUnreachable(s)`:

```typescript
function area(s: Shape): number {
  switch (s.kind) {
    case "Circle":   return 3.14159 * s.r * s.r;
    case "Square":   return s.s * s.s;
    case "Triangle": return heron(s.a, s.b, s.c);
    default: return mochiUnreachable(s);
  }
}
```

**tsc enforcement**: if a programmer (or the emitter) drops a variant from the switch, `s` is no longer typed as `never` in the `default` arm and `mochiUnreachable(s)` fails to type-check (the `s` argument is the missing-variant union, not `never`). This is the standard exhaustiveness idiom for discriminated unions and is the reason every emitted switch carries an explicit `default`.

**Why not omit the `default`**: `--noFallthroughCasesInSwitch` does not enforce exhaustiveness, only fallthrough. Without `default + mochiUnreachable`, a missing variant compiles fine and crashes at runtime when the missing branch is hit. The pattern above is bulletproof and the emitter always emits it.

## Sub-phase 5.3, Nested patterns and guards

### Decisions made (5.3)

The decision-tree pass linearises everything before the TypeScript emitter sees it, so the emitter never confronts a deeply nested pattern. Guards (`if cond`) lower to `if`/`else if` chains inside the relevant `case`:

```typescript
case "Circle": {
  if (s.r > 0) return 3.14159 * s.r * s.r;
  if (s.r === 0) return 0;
  return mochiUnreachable(s as never);
}
```

(`s as never` is required when guards do not cover every value of the narrowed variant; the IR pass marks such cases and the emitter emits an explicit cast.)

## Sub-phase 5.4, Sum with record payloads

### Decisions made (5.4)

**Mochi**: `sum Either { Left(err: ParseError), Right(value: User) }`

**TypeScript**:

```typescript
import { ParseError } from "./parse_error.ts";
import { User } from "./user.ts";

export type Either =
  | { readonly kind: "Left"; readonly err: ParseError }
  | { readonly kind: "Right"; readonly value: User };

export const Left = (err: ParseError): Either => ({ kind: "Left", err });
export const Right = (value: User): Either => ({ kind: "Right", value });
```

The record types (`ParseError`, `User`) are imported from their respective generated files (Phase 4 layout). Cross-file imports use `.ts` extensions in source; `tsc` rewrites to `.js` on emit.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/sums.go` | Sum declaration to type alias + constructor functions |
| `transpiler3/typescript/lower/match.go` | Match-to-switch lowering; tag emission; exhaustiveness default arm |
| `runtime3/typescript/src/exhaustiveness/index.ts` | `mochiUnreachable(x: never): never` |
| `transpiler3/typescript/build/phase05_test.go` | `TestPhase5Sums` |
| `tests/transpiler3/typescript/fixtures/phase05-sums/` | 40 fixtures |

## Test set

- `TestPhase5Sums`, 40 fixtures four-runtime.
- `TestPhase5Exhaustiveness`, a hand-edited fixture removes one `case`; the gate asserts `tsc` reports the missing-variant error.
- `TestPhase5NoFallthrough`, asserts no emitted `case` arm falls through to the next (every arm ends in `return`, `throw`, or `break`).

## Deferred work

- View patterns (`match x { Circle(r) when r > 0 => ... }`). The guard form lands in 5.3 above; "view patterns" in the Haskell sense are not in Mochi.
- Or-patterns (`A | B => ...`). Lower to `case "A": case "B":` with shared body; the decision-tree pass already produces this.
- Pattern synonyms. Not in MEP-52 scope.
