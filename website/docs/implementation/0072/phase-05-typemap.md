---
title: "Phase 5. Type mapping"
sidebar_position: 7
sidebar_label: "Phase 5. Type mapping"
description: "MEP-72 Phase 5: closed TS-to-Mochi type table. Every TS type kind from the ApiSurface either maps to a Mochi type, an extern type, or a SkipReport."
---

# Phase 5. Type mapping

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase5TypeMap` in `package3/typescript/typemap/phase05_test.go`: subtests `scalars`, `arrays_tuples`, `records_maps`, `unions`, `intersections`, `optional_nullable`, `functions`, `generics`, `promise_async_iterable`, `mapped_conditional`, `opaque_extern`, `skip_classification`, `golden_corpus`. Each subtest feeds a representative `ApiSurface` fragment through the type-mapper and asserts the resulting Mochi-side type representation matches the golden. The corpus subtest runs all 24 fixture packages and asserts per-package mapped-item count + SkipReport count against committed golden numbers.

## Lowering decisions

The type table is closed: every `ApiSurface` type kind has exactly one of three outcomes:

1. **Map**: a Mochi-side type is produced (`int`, `float`, `string`, `bool`, `list<T>`, `map<K, V>`, `T?`, `(T) -> U`, etc.).
2. **Extern**: an opaque `extern type T` declaration is produced; the Mochi side carries an opaque handle and the underlying JS object lives on the host runtime.
3. **Skip**: a `SkipReport` is recorded with a stable `SkipReason`; the export is not emitted.

The table:

| TS construct | Mochi outcome |
|--------------|---------------|
| `number` | `float` (map) |
| `number` annotated `@int` via JSDoc | `int` (map) |
| `bigint` | `int` (map; BigInt is the JS path for 64-bit integers) |
| `string` | `string` (map) |
| `boolean` | `bool` (map) |
| `null` | `nil` literal (map) |
| `undefined` | `nil` literal (map; collapsed with null) |
| `void` (return) | `void` (map) |
| `any` | SkipReport `SkipAnyType` (skip) |
| `unknown` | SkipReport `SkipUnknownType` (skip) |
| `never` | SkipReport `SkipNeverType` (skip; effectively unreachable) |
| `T[]` | `list<T>` (recurse on T) (map) |
| `[T, U]` (tuple) | `(T, U)` Mochi tuple (map) |
| `Record<K, V>` | `map<K, V>` (map) |
| `Map<K, V>` | `map<K, V>` (map; the runtime backs both with the JS Map) |
| `{ a: T, b: U }` | Mochi record `{ a: T, b: U }` (map) |
| `T \| null` | `T?` (map) |
| `T \| undefined` | `T?` (map) |
| `T \| U` (other unions) | tagged union mapped to Mochi `T \| U` (map) |
| `T & U` | structural intersection (map; recurse) |
| `(x: T) => U` | `(T) -> U` (map) |
| `(x: T) => Promise<U>` | `async fun(T): U` (map) |
| `Promise<T>` (in return) | wraps the function with `async fun(): T` (map) |
| `AsyncIterable<T>` | `stream<T>` (map) |
| `Iterable<T>` | `list<T>` (map; eager materialise) |
| `T extends U ? X : Y` | resolved eagerly at bind site; if generic-dependent, SkipReport `SkipConditionalType` |
| `{ [K in keyof T]: F<K> }` (mapped depth 1) | resolved if T is concrete; else SkipReport |
| mapped depth >= 2 | SkipReport `SkipMappedDepth` |
| `T` (named, in scope) | recursive lookup |
| `T` (opaque, e.g., Node Buffer, DOM Element) | `extern type T` (extern) |
| function with decorator | SkipReport `SkipDecorator` |
| ambient module `*` wildcard | SkipReport `SkipAmbientWildcard` |
| `@deprecated` JSDoc tag | item kept; lockfile records the deprecation reason |
| `class C { ... }` | Mochi record `C` for fields + `extern fn C.method(...)` per method |
| `enum E { A, B }` | Mochi union literal `enum E = A \| B` |
| `const enum E { A, B }` | inlined at use sites (TypeScript-side const folding) |
| `namespace N { ... }` | Mochi module `N` (map; recurse) |

The mapping is direction-pure: a Mochi-side `list<T>` corresponds 1:1 to a JS `Array<T>`. No reordering or coercion is needed at the call boundary; the JS engine handles it.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/typemap/typemap.go` | `Mapper`, `MapType`, `MapSignature`, `MapItem`, `MapPackage` |
| `package3/typescript/typemap/scalars.go` | scalar table (number, bigint, string, boolean, null, undefined, void, any/unknown/never) |
| `package3/typescript/typemap/collections.go` | array, tuple, Record, Map, ReadonlyArray, ReadonlyMap, Set, WeakMap |
| `package3/typescript/typemap/unions.go` | union + intersection + nullable resolution |
| `package3/typescript/typemap/functions.go` | function-signature + Promise + AsyncIterable + AbortSignal |
| `package3/typescript/typemap/generics.go` | generic-parameter table + monomorphise key generation |
| `package3/typescript/typemap/opaque.go` | opaque-reference classifier (Node Buffer, DOM Element, etc.) |
| `package3/typescript/typemap/skip.go` | SkipReport classifier per type-kind |
| `package3/typescript/typemap/phase05_test.go` | `TestPhase5TypeMap` sentinel |
| `package3/typescript/typemap/testdata/*.json` | per-construct fixtures + per-corpus-package golden counts |

## Test set

13 subtests as listed in the Gate section + corpus run.

## Cross-references

- [Research note 05 Type mapping](/docs/research/0072/05-type-mapping) — the full table this phase implements.
- [MEP-74 phase 5 typemap](/docs/implementation/0074/phase-05-typemap) — the sister Go-side type-mapping phase.
