---
title: "Phase 15. Monomorphisation"
sidebar_position: 17
sidebar_label: "Phase 15. Monomorphise"
description: "MEP-72 Phase 15: `[ts.monomorphise]` manifest section + per-instantiation extern entries. Resolves TS generics that cannot be erased to per-instantiation Mochi extern fns."
---

# Phase 15. Monomorphisation

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase15Monomorphise` in `package3/typescript/monomorphise/phase15_test.go`: subtests `manifest_parse`, `instantiation_resolve`, `extern_emit_per_instantiation`, `conditional_resolution`, `mapped_resolution`, `dedupe_instantiations`, `golden_corpus`. The first parses a `[[ts.monomorphise]]` entry and asserts the AST contains the expected fields. The second resolves a generic call site (`z.object<{ name: string, age: number }>(...)`) into a concrete instantiation. The third emits a per-instantiation extern entry (e.g., `extern fn z_object__person(shape: ...): Schema<Person>`). The fourth resolves a conditional type at the call site (`InferOutput<Schema<T>>` where `T = Person` resolves to `Person`). The fifth resolves a mapped type at the call site (`Partial<Person>` resolves to `{ name?: string, age?: int }`). The sixth dedupes structurally-equivalent instantiations across multiple call sites. The seventh runs against the 24-package fixture corpus and asserts the golden monomorphisation count per package.

## Lowering decisions

Most TS generics are type-erased on the JS side: `function f<T>(x: T): T { return x; }` becomes `function f(x) { return x; }` at runtime. The Mochi side can carry the same erased shape via `extern fn f<T>(x: T): T`.

A subset of TS generics cannot be erased and require monomorphisation:

1. **Conditional types** that depend on the generic parameter: `function infer<T>(x: T): T extends Schema<infer U> ? U : never`.
2. **Mapped types** of depth >= 1 that depend on the generic parameter: `function partial<T>(x: T): { [K in keyof T]?: T[K] }`.
3. **Higher-kinded type calls** that compose multiple generics: `Pipe<A, B, C>` style.

The bridge handles these via a manifest table:

```toml
[[ts.monomorphise]]
package = "zod"
function = "object"
instantiation = "Person"
shape = "{ name: string, age: number }"
output-name = "z_object__person"

[[ts.monomorphise]]
package = "zod"
function = "infer"
instantiation = "PersonSchema"
input-type = "Schema<Person>"
output-name = "z_infer__person_schema"
output-type = "Person"
```

For each entry, the bridge generates an additional Mochi extern fn declaration in the shim file:

```mochi
// generated from [[ts.monomorphise]]
extern fn z_object__person(shape: map<string, Schema<...>>): Schema<Person>
extern fn z_infer__person_schema(schema: Schema<Person>): Person
```

The Mochi-side caller writes:

```mochi
let person_schema = z.object__person({ name: z.string(), age: z.number() })
let parsed: Person = z.infer__person_schema(person_schema)
```

The MEP-52 phase 1+ emitter unwraps the monomorphised call into the original generic call at emit time; the host JS runtime sees the original generic function. The monomorphisation is purely a Mochi-side type-discipline mechanism.

Two structurally-equivalent instantiations (same shape modulo field renames) are deduplicated; the bridge picks one canonical name (alphabetical by output-name) and aliases the rest.

The user is not required to hand-author monomorphisation entries; phase 15 also runs a pass that auto-detects call sites needing monomorphisation (the `--auto` mode of `mochi pkg lock`) and writes the entries on the user's behalf. Auto-detection runs the TS compiler against the Mochi-emitted TS source and reads `tsc --noEmit --diagnostics`; type errors at call sites with generic parameters trigger an entry suggestion.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/monomorphise/manifest.go` | parser for `[[ts.monomorphise]]` |
| `package3/typescript/monomorphise/instantiator.go` | resolves a generic call site against the ApiSurface |
| `package3/typescript/monomorphise/emitter.go` | emits the per-instantiation extern entries |
| `package3/typescript/monomorphise/dedupe.go` | structural equivalence + alias resolution |
| `package3/typescript/monomorphise/auto.go` | auto-detection of needed instantiations |
| `package3/typescript/monomorphise/phase15_test.go` | `TestPhase15Monomorphise` sentinel |

## Test set

7 subtests as listed in the Gate section.

## Cross-references

- [Research note 05 §8 Generics + monomorphisation](/docs/research/0072/05-type-mapping#8-generics--monomorphisation) — the design.
- [MEP-74 phase 15 monomorphise](/docs/implementation/0074/phase-15-monomorphise) — the sister Go-side monomorphisation phase.
