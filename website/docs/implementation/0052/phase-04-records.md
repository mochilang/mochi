---
title: "Phase 4. Records"
sidebar_position: 5
sidebar_label: "Phase 4. Records"
description: "MEP-52 Phase 4, Mochi records as TypeScript class with readonly fields, private constructor, static of() factory; structural equality; multi-file module layout; 35 fixtures."
---

# Phase 4. Records

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 4](/docs/mep/mep-0052#phase-plan) |
| Status         | IN PROGRESS (4.0 + 4.2 LANDED; 4.1 + 4.3 + 4.4 DEFERRED) |
| Started        | 2026-05-29 20:00 (GMT+7) |
| Landed         | n/a (umbrella PR open) |
| Tracking issue | (this PR) |
| Tracking PR    | (this PR) |

## Gate

`TestPhase4Records`: 35 fixtures green on all four runtimes. Secondary gates: tsc strict (`strictPropertyInitialization` enforced), eslint clean (`@typescript-eslint/no-unsafe-assignment`, `consistent-type-imports`), prettier fixed point. The first phase that emits more than one user module per project, so the multi-file layout under `src/generated/` is the structural gate.

## Goal-alignment audit

Records are Mochi's nominal product type. The TypeScript surface offers four candidate lowerings (a) plain object literal with type alias, (b) `interface` plus factory function, (c) `class` with `readonly` fields, (d) `class` with `readonly` fields plus private constructor plus static `of()` factory. The MEP-52 abstract commits to (d) because it preserves Mochi record identity at runtime (`instanceof` discrimination), blocks accidental mutation at the type level, supports method dispatch (Mochi records can have methods, which lower to class methods), and gives a single hook for structural equality. The cost is roughly 50 bytes of constructor overhead per instance.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 4.0 | `type Pt { x: int, y: int }` to `class Pt { readonly x: number; readonly y: number; private constructor(opts); static of(opts): Pt }` | LANDED (Node + Deno + Bun) | inherited from Phase 3.4 |
| 4.1 | Record methods (`fun (p: Pt) mag() -> int { ... }`) lower to instance methods on the generated class | DEFERRED (vm3 bug in method bodies returning composite arithmetic) | n/a |
| 4.2 | Structural equality: per-record `mochi_eq_<R>(a, b): boolean` helper that ANDs scalar-field comparisons; `==` dispatches to the helper, `!=` wraps in unary `!`; helper emitted once per record name in `prog.Records` source order, only when the program uses `==` / `!=` on values of that record | LANDED (Node + Deno + Bun) | (this PR) |
| 4.3 | Multi-file module layout under `src/generated/`; one record per file by default; per-package directory structure preserved | DEFERRED (single-file `src/index.ts` works for Phase 15 npm pack; no consumer is paying for multi-file complexity) | n/a |
| 4.4 | Identifier mangling: TypeScript reserved-word collisions (`class_`, `import_`) and JS globals (`Object_`, `Promise_`) per MEP-52 §12 | DEFERRED (no fixture surfaces a collision today; will revisit if Phase 5+ fixtures use reserved words as record field names) | n/a |

## Sub-phase 4.0, class with readonly fields and static of()

### Decisions made (4.0)

**Generated class shape**:

```typescript
// src/generated/user.ts
export class User {
  readonly id: bigint;
  readonly name: string;

  private constructor(id: bigint, name: string) {
    this.id = id;
    this.name = name;
  }

  static of(fields: { id: bigint; name: string }): User {
    return new User(fields.id, fields.name);
  }
}
```

**Why private constructor + static factory**: a public constructor with positional parameters is harder to evolve (adding a field is a breaking call-site change for every consumer). A static `of({...})` factory takes a single options object, which is additive: a new optional field never breaks an existing call site. The private constructor also blocks `new User(...)` from outside the module, which is necessary to keep Mochi's nominal identity (no third party can fake a `User` instance).

**Field naming**: Mochi field `user_id` → TS `userId` (camelCase). Mochi field `id` → TS `id`. The Mochi original name is preserved as a JSDoc `@mochiName user_id` on the field for round-tripping by Mochi tooling.

**Type annotations**: `readonly id: bigint` (the IR-picked Repr drives the choice between `bigint` and `number`). `readonly` is mandatory; the emitter never emits a mutable record field. If the Mochi source declares a record with a mutable field, it is rejected at type-check time (Phase 4 supports immutable records only; mutable record fields land in Phase 9 as agent state).

**Property initialization**: `strictPropertyInitialization` requires every field be assigned in the constructor or have an initializer. The emitter always assigns in the constructor body.

## Sub-phase 4.1, Record methods

### Decisions made (4.1)

**Mochi record method**: `fun (u: User) greet() -> string { "hello, " + u.name }` lowers to an instance method on the generated class:

```typescript
export class User {
  readonly id: bigint;
  readonly name: string;
  private constructor(id: bigint, name: string) { this.id = id; this.name = name; }
  static of(fields: { id: bigint; name: string }): User {
    return new User(fields.id, fields.name);
  }
  greet(): string {
    return "hello, " + this.name;
  }
}
```

Mochi methods receive `self` (or `u` here) explicitly; the emitter remaps the explicit-self parameter to `this` inside the method body. The Mochi original name is preserved as `@mochiSelf u` on the method.

**Function-style method calls**: Mochi `greet(u)` (function-style call) and `u.greet()` (method-style) both lower to `u.greet()`. The emitter chooses method-style for readability; functional-style is only emitted when the IR signals an externally-defined free function that takes a record by parameter.

## Sub-phase 4.2, Structural equality

### Decisions made (4.2) — as shipped

**Default**: per-instance identity via JavaScript `===` is wrong for records (`Pt.of({x:1,y:2}) === Pt.of({x:1,y:2})` is `false` because they are two distinct instances). The MEP-52 spec sketched a runtime helper using `Object.keys` reflection plus a `mochiDeepEq` recurse; in implementation we chose a simpler, faster, type-aware emit.

**Generated helper, one per record name**: each record name the program compares via `==` or `!=` gets a typed `mochi_eq_<R>(a: R, b: R): boolean` helper inlined into the same module. The body is a single `return` of `(a.f1 === b.f1) && (a.f2 === b.f2) && ...` (parenthesised per field, AND-joined left-to-right).

```typescript
function mochi_eq_Pt(a: Pt, b: Pt): boolean {
  return (a.x === b.x) && (a.y === b.y);
}

function mochi_eq_User(a: User, b: User): boolean {
  return (a.id === b.id) && (a.name === b.name);
}
```

**Why a typed helper, not `mochiRecordEq` with `Object.keys`**: (1) `Object.keys` is reflection over enumerable own properties, which the V8/Deno/Bun JITs cannot speculatively specialise; the typed `(a.x === b.x) && (a.y === b.y)` form inlines to a sequence of property loads the JIT can fold. (2) `Object.keys` returns string-keyed entries, but Mochi field names are known at emit time, so the generic helper would force unnecessary indirection. (3) The typed helper is shorter to emit and stays inside the same module (no `@mochi/runtime/equality` import to wire up). (4) Bundle cost: a record with N fields produces one helper that's roughly `12 + 23*N` bytes minified, regardless of how many `==` sites reference it; the `Object.keys` helper would be a constant ~120 bytes plus a runtime branch on every call.

**`==` lowering**: `BinEqRec` lowers to `CallExpr{ mochi_eq_<R>, [lhs, rhs] }`. The aotir `BinaryExpr` doesn't carry the record name directly (Op + Left + Right + Result are the only carrier fields), so the lowerer recovers it via a TS-side `tsExprRecordName(e.Left)` mirroring the C-side helper. The Mochi frontend's typechecker has already proven both operands carry the same record name before stamping `BinEqRec`, so reading from `Left` alone is sufficient.

**`!=` lowering**: `BinNeRec` lowers to a unary `!` wrap around the same helper call: `!mochi_eq_<R>(a, b)`. We deliberately chose a wrap over a parallel `mochi_ne_<R>` helper because (1) `!=` on records has no short-circuit semantics for the frontend to preserve (it is pure boolean negation), (2) the wrap saves us doubling the helper surface, (3) the JIT inlines `!` over a boolean-returning call to a single branch with no extra cost.

**Helper emit order is `prog.Records` source order, not call-site order**: the lowerer walks `BinEqRec` sites during body lowering and stamps each referenced record name into a `recordEqFlags map[string]bool`. The prelude assembler then walks `prog.Records` (already in source order from the Mochi file) and emits a helper for every flagged name. This ordering is byte-stable across runs even when the lowerer visits call sites in a different order (future parallel lowerer changes will not regress Phase 16 reproducibility).

**Helper emit is opt-in**: a record declared but never compared via `==` or `!=` gets NO helper. The bundle pays only for what the program uses. `TestPhase4_2UnusedRecordNoHelper` is the assertion gate.

**Helper emit is exactly once per record name**: a program with N `==` sites against the same record type still gets exactly one helper declaration. `TestPhase4_2HelperEmittedOnce` is the assertion gate.

**Field-type coverage**: the four scalar primitives (int → number, float → number, bool → boolean, string → string). All four use TS `===` (JS string equality is byte-wise; number equality is IEEE which matches vm3 for NaN-free fixtures; bool equality is trivial). Nested-record fields are gated out by aotir's Phase 3.0 `buildRecordDecl` (the verifier rejects records with non-scalar fields). When Phase 5 lifts that gate, `buildRecordEqDecl` will grow a recursive `mochi_eq_<Inner>(a.f, b.f)` case for nested-record fields.

**Empty record special case**: a record with zero fields produces `return true;` (any two instances of an empty record are structurally equal). aotir's verifier permits empty records; the helper has to handle them.

**No `equals` instance method**: the MEP-52 abstract suggested per-class `equals(other: User): boolean` methods overriding a `mochiRecordEq` fallback. We dropped the instance-method form for now because (1) Mochi `==` is a free-function dispatch in the IR, not a method call, (2) adding an instance method would require knowing at class emit time which records get compared, which is the same information we already use for the free-function form, just emitted on the wrong side, (3) consumers of the generated TS use `mochi_eq_Pt(a, b)`, not `a.equals(b)`, so the instance method would be dead emit.

### Files (as shipped, 4.2)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/phase04.go` | Adds `tsExprRecordName` (record-name extraction for binary operands), `lowerRecordEq` (BinEqRec/BinNeRec dispatch), `recordEqDecls` (walks `prog.Records` and emits one helper per flagged name in source order), `buildRecordEqDecl` (renders one helper body) |
| `transpiler3/typescript/lower/lower.go` | Threads `recordEqFlags map[string]bool` on the lowerer; branches `lowerBinary` to `lowerRecordEq` for BinEqRec / BinNeRec; wires `recordEqDecls` into the prelude after `runtimeSetDecls` and before `userDecls` |
| `transpiler3/typescript/build/phase04_2_test.go` | Five tests: `TestPhase4_2RecordEquality{Node,Deno,Bun}` (fixture corpus byte-equal stdout), `TestPhase4_2EmitWithoutRuntime` (per-fixture token assertions), `TestPhase4_2UnusedRecordNoHelper` (opt-in helper), `TestPhase4_2HelperEmittedOnce` (one helper per record name), `TestPhase4_2HelperOrder` (helpers in `prog.Records` source order) |
| `tests/transpiler3/typescript/fixtures/phase04.2-record-equality/` | 28 .mochi/.out fixture pairs covering int/float/bool/string fields, multi-field records, two/three record types, self-equality, count-eq-loops, &&/\|\| chains, negation, function returns, var reassignment, while loops, field-access operands, unused-record skip |

## Sub-phase 4.3, Multi-file layout

### Decisions made (4.3)

**One record per file**: Phase 4 is the first phase emitting more than one `.ts` file per project. Records each get their own file under `src/generated/`. The file name is the snake_case form of the record name (`record User` to `user.ts`, `record HttpRequest` to `http_request.ts`).

**Package structure**: Mochi package `foo.bar` with `record User` becomes `src/generated/foo/bar/user.ts`. The package's `index.ts` (also generated) re-exports everything in the package.

**Imports**: cross-file imports use the `.ts` extension in source (`import { User } from "./user.ts"`). `tsc --rewriteRelativeImportExtensions` (TS 5.6) rewrites these to `.js` at emit time.

**`tsconfig` updates**: each emitted package becomes a project reference in the root composite `tsconfig.json` only when the user invokes the multi-package CLI option; Phase 4 single-package mode keeps the project-references list to the four runtime configs.

## Sub-phase 4.4, Identifier mangling

### Decisions made (4.4)

**TypeScript reserved words**: identifiers that collide with TS keywords get a trailing underscore. The full list per MEP-52 §12:

`class_, function_, import_, export_, new_, delete_, void_, typeof_, instanceof_, if_, else_, for_, while_, do_, switch_, case_, default_, break_, continue_, return_, throw_, try_, catch_, finally_, var_, let_, const_, null_, true_, false_, this_, super_, extends_, implements_, interface_, enum_, async_, await_, yield_, static_, public_, private_, protected_, readonly_, abstract_, as_, is_, from_, of_, in_, type_, namespace_, module_, declare_, package_, with_`

**JavaScript globals**: `Object`, `Array`, `Function`, `Promise`, `Map`, `Set`, `Symbol`, `Error`, `console`, `globalThis` get the trailing-underscore treatment when the Mochi identifier matches. This is conservative; the TS type system would accept the un-mangled name (shadowing is legal) but the IDE confusion cost is non-trivial.

**Round-tripping**: every mangled name carries a `@mochiName` JSDoc on its declaration:

```typescript
/** @mochiName class */
export const class_ = 42n;
```

Mochi tooling reading the emitted source recovers the original name.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/records.go` | Record declaration to class; static of() factory; field readonly enforcement (shipped as `phase03_4.go`) |
| `transpiler3/typescript/lower/methods.go` | Record method to instance method; explicit-self to `this` rewrite (deferred with 4.1) |
| `transpiler3/typescript/lower/equality.go` | Per-record helper generation; `BinEqRec` / `BinNeRec` dispatch (shipped as `phase04.go`) |
| `transpiler3/typescript/emit/layout.go` | Multi-file layout under `src/generated/`; package directory tree; per-package `index.ts` (deferred with 4.3) |
| `transpiler3/typescript/lower/mangle.go` | Reserved-word and global identifier mangling; `@mochiName` JSDoc emission (deferred with 4.4) |
| `runtime3/typescript/src/equality/index.ts` | `mochiRecordEq`, `mochiDeepEq` (superseded by per-record helper emit) |
| `transpiler3/typescript/build/phase04_2_test.go` | `TestPhase4_2RecordEquality{Node,Deno,Bun}` + four shape gates (shipped) |
| `tests/transpiler3/typescript/fixtures/phase04.2-record-equality/` | 28 fixtures (shipped) |

## Test set (as shipped, 4.2)

- `TestPhase4_2RecordEqualityNode`, `TestPhase4_2RecordEqualityDeno`, `TestPhase4_2RecordEqualityBun`: 28 fixtures byte-equal stdout against vm3 reference.
- `TestPhase4_2EmitWithoutRuntime`: seven representative fixtures asserted against the helper signature, field-wise comparison shape, `!=` wrap, and unused-record skip.
- `TestPhase4_2UnusedRecordNoHelper`: a record never compared via `==` produces no helper emit.
- `TestPhase4_2HelperEmittedOnce`: programs with multiple `==` sites against the same record name produce exactly one helper declaration.
- `TestPhase4_2HelperOrder`: helpers appear in `prog.Records` source order (Phase 16 reproducibility gate).

## Deferred work

- 4.1 record methods. Blocked by vm3 returning wrong values for record method bodies that evaluate composite arithmetic; without a working vm3 reference, byte-equal stdout cannot be asserted. Will unblock once vm3 lands a fix or once Phase 7 ships an aotir-direct gate.
- 4.3 multi-file layout. Premature; single-file `src/index.ts` works for Phase 15 npm pack and keeps source-map stability simple. Will revisit when a consumer surfaces a need for per-record file splitting.
- 4.4 identifier mangling. No fixture surfaces a TS-keyword collision today (none of the 28 Phase 4.2 fixtures uses a reserved word as a field or record name). Will revisit when a fixture in Phase 5+ exercises this.
- Mutable record fields (Mochi `var` field in a record). Deferred to Phase 9 (agents have mutable state by definition).
- Record inheritance / extension. Not in MEP-52 scope; Mochi records are flat.
- Serialisation hooks (`toJSON`, `fromJSON`). Phase 4 emits `JSON.stringify`-friendly classes (own enumerable readonly fields); custom serialisation is a v2 add.
- Nested-record fields (record fields whose type is another record). aotir's Phase 3.0 verifier rejects these today; when Phase 5 lifts the gate, `buildRecordEqDecl` will grow a recursive `mochi_eq_<Inner>(a.f, b.f)` case.
- List-field and map-field record equality. aotir already has `BinEqList` / `BinEqMap` opcodes but the TS lowerer does not route those through structural helpers yet; Phase 4.2 ships only `BinEqRec` / `BinNeRec`.

## Landing log

- 2026-05-29 20:00 (GMT+7): Started Phase 4.2 work on `worktree-mep52-phase04.2`.
- 2026-05-29 20:38 (GMT+7): All 28 fixtures green on Node + Deno + Bun; emit-shape tests green; opt-in / once / order assertions green.
