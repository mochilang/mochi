---
title: "Phase 6. Closures and higher-order functions"
sidebar_position: 7
sidebar_label: "Phase 6. Closures"
description: "MEP-52 Phase 6, Mochi closures to TypeScript arrow functions, nested function declarations, higher-order function passing; closure-conversion pass output mapped to TS captures; 30 fixtures."
---

# Phase 6. Closures and higher-order functions

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 6](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase6Closures`: 30 fixtures green on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gate: `@typescript-eslint/no-shadow` clean; `strictFunctionTypes` and `strictBindCallApply` enforced; no emitted `Function` constructor.

## Goal-alignment audit

Closures are how Mochi parameterises behaviour: every `map`, `filter`, `fold`, every event handler, every agent message handler is a closure that captures surrounding scope. The TypeScript surface gives us arrow functions (`(x) => x + 1`) with lexical `this` capture, which matches Mochi's closure semantics exactly. The closure-conversion pass shared with MEP-45 through MEP-51 already explicitates the captured environment as a record; the TypeScript emitter does not need to reproduce it. Higher-order functions land here because they are the consumer for closures.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 6.0 | Anonymous closures: `(x: int) -> x + 1` to `(x: bigint): bigint => x + 1n` | NOT STARTED | n/a |
| 6.1 | Named function declarations at module scope to `export function f(...)` | NOT STARTED | n/a |
| 6.2 | Nested function declarations to inner `const f = (...) => ...` (block-scoped const) | NOT STARTED | n/a |
| 6.3 | Higher-order parameters and returns; function-type lowering to `(t: T) => R` (sync) and `(t: T) => Promise<R>` (async, Phase 11 colours) | NOT STARTED | n/a |
| 6.4 | Captured-mutable-variable lowering: `let mut x = 0; (() -> x = x + 1)()` via boxed cell (`{ value: bigint }`) when capture is shared and mutated | NOT STARTED | n/a |

## Sub-phase 6.0, Anonymous closures

### Decisions made (6.0)

**Mochi**: `(x: int) -> x + 1`

**TypeScript**: `(x: bigint): bigint => x + 1n`

**Arrow vs `function`**: arrow function is preferred. It captures `this` lexically (matches Mochi semantics; Mochi has no implicit `this`). It is more concise. eslint (`prefer-arrow-callback`) prefers it.

**Return type annotation**: always emitted. `tsc --strict` infers but the explicit annotation surfaces the IR-derived return type in the source for code review.

**Implicit `return`**: for single-expression bodies, the emitter uses the concise form (`(x: bigint): bigint => x + 1n`). For multi-statement bodies it uses the block form with explicit `return`.

## Sub-phase 6.1, Named function declarations

### Decisions made (6.1)

**Mochi**: `fun add(a: int, b: int) -> int { a + b }`

**TypeScript** (module scope):

```typescript
export function add(a: bigint, b: bigint): bigint {
  return a + b;
}
```

**Why `function` keyword for module-level, arrow for nested**: `function` declarations are hoisted (callable before declaration in the same module). This matches Mochi's "all module functions are simultaneously in scope" semantic. Nested functions inside another function are not hoisted in Mochi (they only exist after their `let` line); arrow functions assigned to `const` give exactly that.

**`export`**: every module-scope function is `export`ed. Mochi's visibility rules (defaulting to private unless declared `pub`) are enforced at the type level via TypeScript's module boundary plus per-symbol re-export filtering in `src/index.ts`.

## Sub-phase 6.2, Nested function declarations

### Decisions made (6.2)

**Mochi**: `fun outer() { fun inner(x: int) -> int { x + 1 }; print(inner(2)) }`

**TypeScript**:

```typescript
export function outer(): void {
  const inner = (x: bigint): bigint => x + 1n;
  print(inner(2n));
}
```

`const` (block-scoped) is the right binding form: an inner function is not hoisted to the top of the surrounding function, only to the line of its declaration.

## Sub-phase 6.3, Higher-order parameters and returns

### Decisions made (6.3)

**Function type lowering**:

| Mochi                       | TypeScript                                |
|-----------------------------|-------------------------------------------|
| `fun(int) -> int`           | `(x: bigint) => bigint`                   |
| `fun(int, int) -> int`      | `(a: bigint, b: bigint) => bigint`        |
| `async fun(int) -> int`     | `(x: bigint) => Promise<bigint>` (Phase 11) |
| `fun(fun(int) -> int) -> int` | `(f: (x: bigint) => bigint) => bigint`  |

**Higher-order example**:

```typescript
export function apply(f: (x: bigint) => bigint, x: bigint): bigint {
  return f(x);
}
```

**Currying**: Mochi does not have language-level auto-currying. A Mochi `fun add(a, b)` returns a 2-ary function; partial application uses an explicit closure (`(b) -> add(5, b)`). The TS emitter does not synthesise curried forms.

**Variance**: TypeScript's `strictFunctionTypes` makes function parameter positions contravariant. The IR-level variance analysis (shared with MEP-50 Kotlin) feeds this; the TS emitter annotates `<in T, out R>` modifiers when the IR signals an explicitly-variant type parameter on a generic function.

## Sub-phase 6.4, Captured mutable variables

### Decisions made (6.4)

**Problem**: TypeScript's `let` lets a closure capture a mutable binding lexically:

```typescript
let x = 0;
const inc = () => { x = x + 1; };
inc();
console.log(x); // 1
```

This works for single-function capture but breaks when the captured value is held in a long-lived shape (e.g. an agent's state) and the closure-conversion pass demands a boxed cell. The closure-conversion pass (shared with MEP-45 etc.) tags every captured mutable variable as either "shared mutable" (multiple closures mutate; needs a cell) or "exclusive mutable" (one closure mutates, others may read; the `let` binding suffices).

**Shared mutable**: lower to a cell record:

```typescript
type Cell<T> = { value: T };
const x: Cell<bigint> = { value: 0n };
const inc = () => { x.value = x.value + 1n; };
const get = (): bigint => x.value;
```

**Exclusive mutable**: stay with `let`:

```typescript
let x: bigint = 0n;
const inc = () => { x = x + 1n; };
```

The IR analysis is the source of truth; the emitter never makes the call independently.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/closures.go` | Arrow vs function lowering; nested const; capture rewriting |
| `transpiler3/typescript/lower/funtype.go` | Mochi function type to TS arrow type lowering |
| `transpiler3/typescript/lower/cells.go` | Shared-mutable capture to Cell<T> rewriting |
| `transpiler3/typescript/build/phase06_test.go` | `TestPhase6Closures` |
| `tests/transpiler3/typescript/fixtures/phase06-closures/` | 30 fixtures |

## Test set

- `TestPhase6Closures`, 30 fixtures four-runtime.
- `TestPhase6NoFunctionConstructor`, asserts no emitted `.ts` contains `new Function(...)` or `Function(...)`.
- `TestPhase6Variance`, fixtures with contravariant function parameter positions exercise tsc `strictFunctionTypes`.

## Deferred work

- Function-type variance annotations (`<in T, out R>`) where the IR derives invariance vs variance. The Phase 6 emitter ships the default invariant form; explicit `in`/`out` annotations are added when the IR demands.
- Tagged template literals (`html\`...\``). Not in Mochi surface.
- Generators inside closures (`function*`). Generators land in Phase 7 (query DSL) and Phase 10 (streams).
