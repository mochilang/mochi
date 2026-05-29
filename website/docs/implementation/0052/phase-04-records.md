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
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase4Records`: 35 fixtures green on all four runtimes. Secondary gates: tsc strict (`strictPropertyInitialization` enforced), eslint clean (`@typescript-eslint/no-unsafe-assignment`, `consistent-type-imports`), prettier fixed point. The first phase that emits more than one user module per project, so the multi-file layout under `src/generated/` is the structural gate.

## Goal-alignment audit

Records are Mochi's nominal product type. The TypeScript surface offers four candidate lowerings (a) plain object literal with type alias, (b) `interface` plus factory function, (c) `class` with `readonly` fields, (d) `class` with `readonly` fields plus private constructor plus static `of()` factory. The MEP-52 abstract commits to (d) because it preserves Mochi record identity at runtime (`instanceof` discrimination), blocks accidental mutation at the type level, supports method dispatch (Mochi records can have methods, which lower to class methods), and gives a single hook for structural equality. The cost is roughly 50 bytes of constructor overhead per instance.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 4.0 | `record User { id: int, name: string }` to `class User { readonly id: bigint; readonly name: string; private constructor(...); static of(...): User }` | NOT STARTED | n/a |
| 4.1 | Record methods (`fun (u: User) greet() -> string { ... }`) lower to instance methods on the generated class | NOT STARTED | n/a |
| 4.2 | Structural equality: `mochiRecordEq(a, b)` runtime helper plus per-record `equals(other)` instance method (Phase 4 emits the helper-based form; the per-class override is generated only when the IR sees `==` between two records of the same type) | NOT STARTED | n/a |
| 4.3 | Multi-file module layout under `src/generated/`; one record per file by default; per-package directory structure preserved (`record foo.bar.User` to `src/generated/foo/bar/user.ts`) | NOT STARTED | n/a |
| 4.4 | Identifier mangling: TypeScript reserved-word collisions (`class_`, `import_`) and JS globals (`Object_`, `Promise_`) per MEP-52 §12 | NOT STARTED | n/a |

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

### Decisions made (4.2)

**Default**: per-instance identity via JavaScript `===` is wrong for records (`User.of({id: 1n, name: "x"}) === User.of({id: 1n, name: "x"})` is `false`).

**Runtime helper**: `mochiRecordEq(a, b)` walks the field list:

```typescript
// @mochi/runtime/equality
export function mochiRecordEq<T extends object>(a: T, b: T): boolean {
  if (a === b) return true;
  if (a.constructor !== b.constructor) return false;
  for (const key of Object.keys(a)) {
    const av = (a as Record<string, unknown>)[key];
    const bv = (b as Record<string, unknown>)[key];
    if (!mochiDeepEq(av, bv)) return false;
  }
  return true;
}
```

`mochiDeepEq` handles primitive `===` for bigint, number, string, boolean; structural equality for arrays, Maps, Sets, and other records; NaN-aware equality (`Number.isNaN(a) && Number.isNaN(b)` is true).

**Per-class override**: when the IR sees `==` between two record values of the same type, the emitter generates a typed `equals(other: User): boolean` method that inlines the field-by-field check (no `Object.keys` reflection in the hot path). This is the recommended path; the runtime helper is the fallback for generic-record contexts.

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
| `transpiler3/typescript/lower/records.go` | Record declaration to class; static of() factory; field readonly enforcement |
| `transpiler3/typescript/lower/methods.go` | Record method to instance method; explicit-self to `this` rewrite |
| `transpiler3/typescript/lower/equality.go` | Per-type `equals` method generation; `mochiRecordEq` fallback dispatch |
| `transpiler3/typescript/emit/layout.go` | Multi-file layout under `src/generated/`; package directory tree; per-package `index.ts` |
| `transpiler3/typescript/lower/mangle.go` | Reserved-word and global identifier mangling; `@mochiName` JSDoc emission |
| `runtime3/typescript/src/equality/index.ts` | `mochiRecordEq`, `mochiDeepEq` |
| `transpiler3/typescript/build/phase04_test.go` | `TestPhase4Records` |
| `tests/transpiler3/typescript/fixtures/phase04-records/` | 35 fixtures |

## Test set

- `TestPhase4Records`, 35 fixtures four-runtime.
- `TestPhase4StructuralEquality`, fixtures asserting `==` between distinct instances with the same field values returns `true`.
- `TestPhase4PrivateConstructor`, asserts a hand-edited `.ts` calling `new User(...)` from outside the module fails tsc.

## Deferred work

- Mutable record fields (Mochi `var` field in a record). Deferred to Phase 9 (agents have mutable state by definition).
- Record inheritance / extension. Not in MEP-52 scope; Mochi records are flat.
- Serialisation hooks (`toJSON`, `fromJSON`). Phase 4 emits `JSON.stringify`-friendly classes (own enumerable readonly fields); custom serialisation is a v2 add.
