---
title: "Phase 6. Closures and higher-order functions"
sidebar_position: 7
sidebar_label: "Phase 6. Closures"
description: "MEP-52 Phase 6, Mochi closures to TypeScript arrow functions, nested function declarations, higher-order function passing; closure-conversion pass output mapped to TS captures; 25 fixtures green on Node + Deno + Bun."
---

# Phase 6. Closures and higher-order functions

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 6](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (Node + Deno + Bun) |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 22:40 (GMT+7) |
| Tracking issue | mochilang/mochi#22754 |
| Tracking PR    | mochilang/mochi#22755 |

## Gate

`TestPhase6ClosuresNode` / `TestPhase6ClosuresDeno` / `TestPhase6ClosuresBun`: 25 fixtures green on Node 22, Deno 2, Bun 1.1, byte-equal stdout against vm3. Floor 20, shipped 25.

Secondary gates landed:

- `TestPhase6EmitShape`: token-level assertions on emitted source, asserting the arrow form `(x: number): number =>` and the function-type lowering `(__p0: number) => number` for higher-order parameters.
- `TestPhase6NoLiftedTopLevel`: asserts no `function __anon_` or `function __shim_` appears in the emitted source. The aotir closure-conversion pass lifts capturing bodies; the TypeScript emitter inlines them back at the FunLit call site.
- `TestPhase6EnvPrefixStripped`: asserts no literal `__e->` substring in emitted source. Aotir uses `__e->field` for captured-variable reads (C-friendly); TS strips that prefix so lexical capture in the arrow body just works.

Browser (Chromium 130) deferred to Phase 17 alongside JSR + Jupyter.

## Goal-alignment audit

Closures are how Mochi parameterises behaviour: every `map`, `filter`, `fold`, every event handler, every agent message handler is a closure that captures surrounding scope. The TypeScript surface gives us arrow functions (`(x) => x + 1`) with lexical `this` capture, which matches Mochi's closure semantics exactly. The aotir closure-conversion pass shared with the C transpiler explicitates captures as a synthetic env record; the TypeScript emitter does not reproduce that record (lexical capture handles it for free) and instead **un-lifts** the lifted function body back into an arrow at the original FunLit site.

## As-shipped lowering

The aotir closure-conversion pass produces three shapes the TS lowerer has to handle. Each one has a single, narrow TypeScript translation:

| aotir node          | What aotir/C does                                | What TypeScript does                       |
|---------------------|--------------------------------------------------|--------------------------------------------|
| `FunLit`            | references a lifted top-level `__anon_N` body    | inline the lifted body as `ArrowExpr` at the use site |
| `ClosureEnvStmt`    | declares + populates the env record on the stack | no-op (returns nil from the lowerer)       |
| `FunCapture` reads  | rewrites VarRef.Name to `__e->fieldname`         | strip the `__e->` prefix; lexical scope handles capture |
| Shim FunRefs        | wraps free functions as `__shim_<name>` for fun-typed slots | collapse to the underlying `mochi__<name>` identifier |
| Function-typed slot | renders as a C function-pointer typedef          | render as `(__p0: T0, __p1: T1) => R`      |

The lifted `__anon_*` and `__shim_*` top-level functions are filtered out of the userDecls pass entirely (`lower.go` skips any function with `IsLifted == true` or whose name matches `__anon_` / `__shim_`). The body is re-located to where the FunLit is used.

### Example: anonymous closure with capture

**Mochi**:

```
let base = 10
let offset = 5
let compute = fun(x: int): int => base + offset + x
print(compute(3))
```

**Aotir** (post closure-conversion, illustrative):

```
function __anon_0(__e: env_0*, x: int) -> int { return __e->base + __e->offset + x; }
ClosureEnvStmt env_0 { base, offset }
LetStmt compute = FunLit{FuncName: "__anon_0", Captures: [base, offset]}
```

**TypeScript** (as shipped):

```typescript
const base: number = 10;
const offset: number = 5;
const compute: (__p0: number) => number = (x: number): number => {
    return ((base + offset) + x);
};
console.log(String(compute(3)));
```

Note: no `function __anon_0`, no env record, no `__e->` prefix. Aotir's machinery exists; the TS lowerer just doesn't render it.

### Example: function returning a closure

**Mochi**:

```
fun make_adder(n: int): fun(int): int {
  return fun(x: int): int => x + n
}
let add5 = make_adder(5)
print(add5(3))
```

**TypeScript**:

```typescript
function mochi__make_adder(n: number): (__p0: number) => number {
    const f: (__p0: number) => number = (x: number): number => {
        return (x + n);
    };
    return f;
}
const add5: (__p0: number) => number = mochi__make_adder(5);
console.log(String(add5(3)));
```

### Example: higher-order parameter

**Mochi**:

```
fun apply(f: fun(int): int, x: int): int { return f(x) }
let double = fun(x: int): int => x * 2
print(double(5))
```

**TypeScript**:

```typescript
function mochi__apply(f: (__p0: number) => number, x: number): number {
    return f(x);
}
const double: (__p0: number) => number = (x: number): number => {
    return (x * 2);
};
console.log(String(double(5)));
```

The `apply` function ships in the emit (it exercises the function-type parameter surface) even though the fixture calls `double(5)` directly. This is intentional: vm3 has a known bug where calling a higher-order function-typed parameter returns nil, so the fixture has to call closures directly to keep the stdout-equivalence gate honest. The TS emit still validates the function-type lowering shape (`TestPhase6EmitShape`).

## Sub-phases (as shipped)

| # | Scope | Status |
|---|-------|--------|
| 6.0 | Anonymous closures: `fun(x: int): int => x + 1` to `(x: number): number => x + 1` (arrow, concise body when single expression) | LANDED |
| 6.1 | Named function declarations at module scope to `function mochi__<name>(...)` (Phase 2 inheritance) | LANDED |
| 6.2 | Nested closures captured in a `const` slot to `const f: T = (...) => ...` (block-scoped) | LANDED |
| 6.3 | Higher-order parameters and returns; function-type lowering to `(__p0: T0, __p1: T1) => R` | LANDED |
| 6.4 | Captured-immutable-variable lowering via lexical scope (no cell wrapping needed; Mochi `let` is immutable at the TS surface) | LANDED |
| 6.5 | Captured-mutable-variable lowering (`let mut` boxed cells) | DEFERRED (no fixture exercises mutable capture in Phase 6; aotir does not yet emit `MutLet` for captured-and-mutated bindings; revisit when Phase 9 agents need it) |
| 6.6 | Nested-capturing closures (inner closure captures from outer's outer scope) | DEFERRED (aotir Phase 5.1 closure-conversion limitation; the C transpiler has the same gap; revisit when aotir multi-level capture lands) |
| 6.7 | Void-returning closures (`fun(): void`) | DEFERRED (vm3 / aotir require an explicit `: T` return annotation in fun expressions in Phase 5.0; fixtures use a string-returning closure as a stand-in) |

## Sub-phase 6.0, Anonymous closures

### Decisions made (6.0)

**Mochi**: `fun(x: int): int => x + 1`

**TypeScript**: `(x: number): number => x + 1`

**Arrow vs `function`**: arrow function is the only form used at expression position. `function` expressions would also work, but the arrow form is more concise, captures `this` lexically (matches Mochi semantics; Mochi has no implicit `this`), and eslint (`prefer-arrow-callback`) prefers it. `function` is still used at statement position for top-level Mochi `fun` declarations (Phase 2 emit), because the hoisting matches Mochi's "all module functions are simultaneously in scope" semantic.

**Number representation**: numeric literals are emitted as plain JS `number`, not `bigint`. This is the Phase 2.1 decision (bigint deferred), inherited here. A future "strict int" variant would emit `1n` and tag the return type as `bigint`; not in Phase 6's scope.

**Return type annotation**: always emitted on the arrow head. `tsc --strict` would infer, but the explicit annotation surfaces the IR-derived return type in the source for code review and for `strictFunctionTypes` clarity.

**Concise vs block body**: for single-expression bodies, the lowerer emits `(x: number): number => x + 1`. For multi-statement bodies, the block form with explicit `return`. The lowerer detects single-Return-stmt and switches to concise form (`ExprBody` field on `tstree.ArrowExpr`).

**Arrow as call callee**: when an arrow expression is used directly as a call callee (`(fun(x) => x)(3)`), the lowerer wraps it in `tstree.ParenExpr` so the emit reads `((x: number): number => x)(3)` and tsc parses it as call-of-arrow, not as a malformed expression statement.

## Sub-phase 6.1, Named function declarations

### Decisions made (6.1)

**Mochi**: `fun add(a: int, b: int): int { a + b }`

**TypeScript**:

```typescript
function mochi__add(a: number, b: number): number {
    return (a + b);
}
```

**`mochi__` prefix**: every user-defined function (and let-binding) is prefixed `mochi__` at emit. This is the Phase 1 decision (reserve every JS identifier collision: `console`, `Map`, `Set`, `Promise`, etc.). Top-level Mochi `fun` declarations carry the prefix; lifted aotir bodies (`__anon_*`, `__shim_*`) are skipped entirely, so the prefix never appears for them.

**`function` keyword for module-level**: `function` declarations are hoisted (callable before declaration in the same module). This matches Mochi's "all module functions are simultaneously in scope" semantic. Nested closures inside another function are not hoisted in Mochi (they only exist after their `let` line); arrow functions assigned to `const` give exactly that.

**`export`**: Phase 15 (npm package) decides which symbols are re-exported. Phase 6 ships the function as a top-level `function mochi__<name>(...)`; the index.ts re-export filter is Phase 15's job.

## Sub-phase 6.2, Nested closures in `const`

### Decisions made (6.2)

**Mochi**:

```
fun outer(): int {
  let inner = fun(x: int): int => x + 1
  return inner(2)
}
```

**TypeScript**:

```typescript
function mochi__outer(): number {
    const inner: (__p0: number) => number = (x: number): number => {
        return (x + 1);
    };
    return inner(2);
}
```

`const` (block-scoped) is the right binding form: an inner closure is not hoisted to the top of the surrounding function, only to the line of its declaration. The type annotation on the `const` slot uses `(__p0: T0) => R` form (see 6.3).

The aotir IR emits the lifted body as a separate `__anon_N` function and a `LetStmt` whose RHS is a `FunLit`. The TS lowerer ignores the `__anon_N` top-level function and inlines its body as the arrow expression on the `LetStmt` RHS.

## Sub-phase 6.3, Higher-order parameters and returns

### Decisions made (6.3)

**Function type lowering** (aotir `TypeFun` with `FunSig{ParamTypes, ReturnType}`):

| Mochi                            | TypeScript                                          |
|----------------------------------|-----------------------------------------------------|
| `fun(int): int`                  | `(__p0: number) => number`                          |
| `fun(int, int): int`             | `(__p0: number, __p1: number) => number`            |
| `fun(int): string`               | `(__p0: number) => string`                          |
| `fun(fun(int): int, int): int`   | `(__p0: (__p0: number) => number, __p1: number) => number` |

**`__p0`, `__p1` synthetic param names**: TypeScript function-type literals require parameter names. The aotir `FunSig` only carries parameter types (not names), so the lowerer synthesises `__p0`, `__p1`, ... These names are only for the type literal; they do not leak into runtime binding.

**Higher-order example**:

```typescript
function mochi__apply(f: (__p0: number) => number, x: number): number {
    return f(x);
}
```

**Currying**: Mochi does not have language-level auto-currying. A Mochi `fun add(a, b)` returns a 2-ary function; partial application uses an explicit closure (`fun(b): int => add(5, b)`). The TS emitter does not synthesise curried forms.

**Variance**: TypeScript's `strictFunctionTypes` makes function parameter positions contravariant. Phase 6 ships the default invariant form. Explicit `<in T, out R>` variance annotations are deferred (no fixture surfaces the gap; revisit with Phase 7 generic queries).

**Async colour**: Phase 11 will add `(__p0: T) => Promise<R>` for async-coloured function types. Phase 6 is sync-only.

## Sub-phase 6.4, Lexically captured immutable variables

### Decisions made (6.4)

Mochi `let` is immutable. A closure captures `let` bindings by reference at the TypeScript surface, and the reference is to a `const` (Phase 2 emits `let` Mochi bindings as TS `const`). JavaScript's lexical-scope semantic gives correct capture for free; no cell wrapping needed.

**Mochi**:

```
let base = 10
let f = fun(x: int): int => x + base
```

**TypeScript**:

```typescript
const base: number = 10;
const f: (__p0: number) => number = (x: number): number => {
    return (x + base);
};
```

The aotir closure-conversion pass tags `base` as a `FunCapture` with `SrcName: "base"` and rewrites the body's VarRef to `__e->base`. The TS lowerer strips the `__e->` prefix in `lowerVarRef` and `lowerUnionVarRef` (the latter path handles VarRefs originating inside sum-type match arms, which would otherwise route through Phase 5's union-aware reader).

### Why no cell record (`{value: T}`)

Mochi has no `let mut` in the Phase 6 fixture corpus. When mutable capture lands (Phase 9 agents likely surfaces it first), aotir will tag the binding as shared-mutable and emit a different IR shape (an explicit `LetMutStmt` or analogous). The TS emitter then maps that to a `Cell<T>` record:

```typescript
type Cell<T> = { value: T };
const x: Cell<number> = { value: 0 };
const inc = (): void => { x.value = x.value + 1; };
```

This path is intentionally deferred to keep Phase 6 narrow: 25 fixtures, all immutable capture, all passing.

## Deferred work (carry to Phase 9 / Phase 11)

- **Mutable capture (6.5)**: requires aotir IR change. Phase 9 likely surfaces it via agent state.
- **Multi-level nested capture (6.6)**: aotir Phase 5.1 closure-conversion limitation. C transpiler hits the same gap. Revisit when aotir multi-level capture lands.
- **Void-returning closures (6.7)**: vm3 / aotir require an explicit `: T` return annotation in `fun(...): T => ...` syntax in Phase 5.0. Fixture corpus uses a string-returning closure as a stand-in for void semantics. Revisit when Phase 5.0 syntactic limitation lifts.
- **Higher-order function-typed param invocation in vm3**: vm3 has a known bug where calling a function-typed parameter returns nil. Fixtures call closures directly (`double(5)` not `apply(double, 5)`) to keep the stdout-equivalence gate honest. The TS emit still validates the function-type lowering shape via `TestPhase6EmitShape`. Revisit when vm3 higher-order dispatch lands.
- **Function-type variance (`<in T, out R>`)**: no fixture surfaces it. Revisit with Phase 7 query DSL generics.
- **Browser target**: deferred to Phase 17 alongside JSR + Jupyter.

## Files (as shipped)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/tstree/phase06.go` | `ArrowExpr` and `ParenExpr` AST nodes |
| `transpiler3/typescript/lower/phase06.go` | FunLit, ClosureEnvStmt, FunCallExpr lowering; `__e->` strip; `__anon_*`/`__shim_*` skip; `tsTypeForFunSig` |
| `transpiler3/typescript/lower/lower.go` | Lifted-function skip on userDecls pass; `liftedByName` lookup; ClosureEnvStmt dispatch |
| `transpiler3/typescript/lower/phase02.go` | `paramType` and `returnType` route TypeFun via `tsTypeForFunSig` |
| `transpiler3/typescript/lower/phase05.go` | `lowerUnionVarRef` strips `__e->` prefix |
| `transpiler3/typescript/build/phase06_test.go` | `TestPhase6Closures{Node,Deno,Bun}`, `TestPhase6EmitShape`, `TestPhase6NoLiftedTopLevel`, `TestPhase6EnvPrefixStripped` |
| `tests/transpiler3/typescript/fixtures/phase06-closures/` | 25 fixtures |

## Fixture corpus (25)

Closure shape (8): `req_closure_simple`, `req_closure_two_arg`, `req_closure_float`, `req_closure_bool_return`, `req_closure_string_return`, `req_closure_block_body`, `req_closure_multiple_types`, `req_closure_in_function`.

Capture shape (9): `req_capture_int`, `req_capture_float`, `req_capture_bool`, `req_capture_string`, `req_capture_multi`, `req_capture_adder`, `req_capture_counter_sim`, `req_capture_in_function`, `req_capture_in_block`.

Higher-order shape (4): `req_higher_order_param`, `req_higher_order_two_arg`, `req_funref_value`, `req_nested_closures`.

Misc (4): `req_two_closures`, `req_closure_void_call`, `req_closure_zero_arg`, `req_closure_with_loop`.
