---
title: "MEP-72 Note 05: Type mapping table"
sidebar_position: 6
sidebar_label: "05. Type mapping"
description: "The complete closed translation table from TypeScript types to Mochi types: scalars (number↔float, bigint↔int, string↔string, boolean↔bool), collections (T[]↔list, Map↔map, Set↔set, Record↔map), nullables (T | null↔T?), discriminated unions↔sum types, interfaces↔records, classes↔extern types, Promise<T>↔async fun, AsyncIterable<T>↔stream, the refusal set, the monomorphisation rule."
---

# 05. Type mapping table

This note documents the complete closed type-mapping table the bridge applies. It is informative; the normative reference is [MEP-72 §Abstract §3](/docs/mep/mep-0072).

## 1. Scalar types

| TS type | Mochi type | Notes |
|---------|------------|-------|
| `number` | `float` | All TS numbers are IEEE-754 doubles; Mochi `float` matches. |
| `number` (in integer position via JSDoc `@type {integer}` or branded `Integer<T>`) | `int` | Integer branding triggers Mochi-int mapping; overflow-guarded at the boundary. |
| `bigint` | `int` | Mochi's int is bigint-discriminated under MEP-52 phase 2.1. |
| `string` | `string` | Direct mapping; both sides are UTF-16 internally. |
| `boolean` | `bool` | Direct mapping. |
| `void` | `unit` | Function return only. |
| `undefined` | `nil` | Coalesced with `null` (see §3). |
| `null` | `nil` | Coalesced with `undefined` (see §3). |
| `any` | (skipped with SkipReport) | The user can override per item. |
| `unknown` | (skipped with SkipReport) | The user can override per item. |
| `never` | `unit` (in return position) | Marks unreachable. |

## 2. Collection types

| TS type | Mochi type | Notes |
|---------|------------|-------|
| `T[]` | `list<T>` | T must be in-table. |
| `Array<T>` | `list<T>` | Same as `T[]`. |
| `ReadonlyArray<T>` | `readonly list<T>` (fallback: `list<T>` with comment) | Readonly view from MEP-52 phase 3.1 deferred work. |
| `Map<K, V>` | `map<K, V>` | K must be `string` or integer; V must be in-table. |
| `ReadonlyMap<K, V>` | `readonly map<K, V>` (fallback: `map<K, V>` with comment) | |
| `Set<T>` | `set<T>` | T must be in-table. |
| `ReadonlySet<T>` | `readonly set<T>` (fallback: `set<T>` with comment) | |
| `Record<K, V>` | `map<K, V>` | K must be `string` or integer; V must be in-table. |
| `[A, B, C]` (tuple) | `tuple<A, B, C>` | All elements must be in-table. |

## 3. Nullable types

TypeScript distinguishes `null` and `undefined`; Mochi has only `nil`. The bridge coalesces:

| TS type | Mochi type |
|---------|------------|
| `T | null` | `T?` |
| `T | undefined` | `T?` |
| `T | null | undefined` | `T?` |

This is a documented asymmetry: a TS function that distinguishes `null` from `undefined` (rare; usually a code smell) cannot be precisely modelled. The user can override per binding with a hand-written extern fn that takes `T?` plus a separate `nullness: int` parameter.

## 4. Object types

| TS type | Mochi type | Notes |
|---------|------------|-------|
| `{ a: A, b: B }` (anonymous) | `record { a: A, b: B }` | All fields must be in-table; one out-of-table field skips the whole record. |
| `interface I { a: A, b: B }` | `record I { a: A, b: B }` | Same as anonymous. |
| `class C { method(...): T }` | `extern type C` (opaque) | Class state is Mochi-opaque; methods become extern fns taking `self: C`. |
| `abstract class A` | (skipped with SkipReport) | Abstract classes have no constructor; cannot be instantiated from Mochi. |

## 5. Union types

| TS union | Mochi mapping |
|----------|---------------|
| `T | null` | `T?` (see §3) |
| `{kind: "a", ...} | {kind: "b", ...}` (tagged union with literal discriminator) | Mochi sum type with `a` and `b` variants |
| `"foo" | "bar" | "baz"` (string literal union) | Mochi sum type with nullary variants |
| `number | string` (heterogeneous, non-literal) | (skipped with SkipReport) |
| `T | U` (other) | (skipped with SkipReport) |

## 6. Function types

| TS type | Mochi type |
|---------|------------|
| `(a: A, b: B) => C` | `fun(a: A, b: B) -> C` |
| `(a: A) => Promise<B>` | `async fun(a: A) -> B` |
| `<T>(arg: T) => T` (generic) | (skipped without monomorphise entry) |
| `(...args: A[]) => B` (variadic) | `fun(args: list<A>) -> B` (the variadic suffix coalesces to a list) |
| `(this: T, a: A) => B` (with `this` parameter) | (skipped with SkipReport) |

## 7. Generic and parameterised types

Generics are refused by default. The user opts in per item via `[ts.monomorphise]`:

```toml
[ts.monomorphise]
items = [
    { item = "zod.ZodObject", T = "MyShape" },
    { item = "lodash.cloneDeep", T = "MyType" },
]
```

The bridge emits a separate extern fn declaration per monomorphisation.

## 8. Mapped types (named built-ins)

The bridge eagerly resolves the following mapped-type built-ins at bind time:

- `Pick<T, K>` → resolved structural type
- `Omit<T, K>` → resolved structural type
- `Partial<T>` → resolved structural type (every field becomes optional)
- `Required<T>` → resolved structural type (every field becomes required)
- `Readonly<T>` → resolved structural type (every field becomes readonly)
- `Record<K, V>` → `map<K, V>` (see §2)
- `Awaited<T>` → unwraps `Promise<T>` to `T`
- `ReturnType<F>` → resolved return type
- `Parameters<F>` → resolved parameter tuple

Any other mapped type (user-authored or library-authored) is skipped with SkipReport.

## 9. Conditional types

A conditional type `T extends U ? X : Y` is eagerly resolved at the export-position bind site. If the resolution depends on a generic parameter, the bridge SkipReports unless the parameter is monomorphised.

## 10. Promise and async types

| TS type | Mochi type |
|---------|------------|
| `Promise<T>` | `async fun(): T` (in return position) |
| `PromiseLike<T>` | `async fun(): T` (same as `Promise<T>`) |
| `AsyncIterable<T>` | `stream<T>` |
| `AsyncIterator<T>` | `stream<T>` (the iterator interface is structural) |
| `Iterable<T>` | `iter<T>` |
| `Iterator<T>` | `iter<T>` |

Async / await ceremony lives on the Mochi side (MEP-52 phase 11.3 colour pass); the bridge does no per-call wrapping.

## 11. TypeScript-only constructs

| TS construct | Mochi mapping |
|--------------|---------------|
| `enum E { A, B }` (numeric) | Mochi sum type with `A` and `B` nullary variants (carries numeric values for serialisation parity) |
| `enum E { A = "a", B = "b" }` (string) | Mochi sum type with `A` and `B` nullary variants (carries string values) |
| `const enum E { A, B }` | Inlined at bind time (same as numeric enum) |
| `namespace N { ... }` | Resolved to nested Mochi namespace via dot syntax |
| `module "..."` (ambient) | Resolved through the package's `package.json` `exports` |
| `declare const x: T` | Mochi `extern var x: T` |
| `declare function f(...): T` | Mochi `extern fn f(...): T` |

## 12. Refusal cases (SkipReport)

| TS construct | Reason |
|--------------|--------|
| Branded types whose witnesses are not exported | Cannot construct values from Mochi. |
| Conditional types beyond eager resolution | Depend on call-site generic. |
| Mapped types beyond the named built-ins | Open-ended; the user can add a hand-written extern type. |
| Intersection of two object types with overlapping non-identical members | Merged shape is undefined. |
| Declaration-merging across multiple `.d.ts` files where the merged shape is not a strict union | Resolution is order-dependent. |
| Ambient module declarations with no corresponding runtime export | No callable to bind. |
| `this` parameter polymorphism | Mochi has no `this`. |
| Default exports that are arrow-function literals whose inferred type is `() => any` | Underspecified. |
| Decorator-injected metadata | Runtime introspection-only. |
| `keyof T` projected through a generic | Depends on the monomorphisation. |
| `typeof <runtime-expression>` in type position | Resolution requires runtime evaluation. |
| `infer` clauses in generic position | Same as conditional types. |

## 13. Cross-references

- [MEP-72 §Abstract §3](/docs/mep/mep-0072) — the normative table.
- [[04-tsdoc-dts-ingest]] — how the bridge gets the type info.
- [[02-design-philosophy]] §3 — why a closed table.
