---
title: "Phase 3. Collections (lists, maps, sets, lists of records)"
sidebar_position: 4
sidebar_label: "Phase 3. Collections"
description: "MEP-52 Phase 3, Mochi list/map/set lowering to TypeScript readonly T[], Map<K, V>, Set<T> with ES2024 methods, plus lists of records; comprehensions; 85 fixtures across 4 sub-phases."
---

# Phase 3. Collections

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 3](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase3Collections`: 85 fixtures green across the four sub-phases on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gates: `tsc --strict --noUncheckedIndexedAccess` zero diagnostics (this is the phase that first exercises index-access narrowing at scale), eslint clean (including `@typescript-eslint/prefer-readonly-parameter-types` for collection function parameters where the IR says read-only).

## Goal-alignment audit

Phase 3 lands the three collection shapes Mochi programs rely on for almost all data manipulation: `list<T>`, `map<K, V>`, `set<T>`. The TypeScript surface gives us each one nearly for free (`T[]`, `Map<K, V>`, `Set<T>` plus ES2024 set methods), but the strict-mode rules force several non-obvious choices: index access under `--noUncheckedIndexedAccess` is `T | undefined`, so every `xs[i]` either needs a runtime bounds check or an IR-proven `i < len(xs)` provenance to justify the non-null assertion; `Map.get` is `V | undefined` for the same reason. Sub-phase 3.4 ties together collections and records (which Phase 4 will land in full), so that the query DSL in Phase 7 has lists of records to operate on.

## Sub-phases

The MEP-52 phase matrix splits Phase 3 into four sub-phases. Each is its own gate; the umbrella is LANDED only when all four are green.

| # | Scope | Fixtures | Status | Commit |
|---|-------|----------|--------|--------|
| 3.1 | Lists (`readonly T[]` for immutable view, `T[]` when mutated; index, len, for-each, list comprehensions) | 25 | NOT STARTED | n/a |
| 3.2 | Maps (`Map<K, V>`; get, set, has, delete, for-each, entries) | 25 | NOT STARTED | n/a |
| 3.3 | Sets (`Set<T>` with ES2024 union/intersection/difference) | 15 | NOT STARTED | n/a |
| 3.4 | Lists of records (records via a Phase 4 preview; comprehensions over records) | 20 | NOT STARTED | n/a |

## Sub-phase 3.1, Lists

### Decisions made (3.1)

**Type**: `readonly T[]` for immutable views (the default for `let xs = [1, 2, 3]` where `xs` is never mutated), `T[]` when the IR proves a mutation site. The monomorphisation pass tags every list-typed value with a `Mutability` field; the emitter picks the type annotation accordingly.

**Literal**: `[1, 2, 3]` lowers to `[1n, 2n, 3n]` (bigint) or `[1, 2, 3]` (number) per the int monomorphisation rule from Phase 2.

**Indexing**: `xs[i]` lowers to either `xs[i]!` (non-null assertion, only when IR-provenance proves `0 <= i < len(xs)`) or `mochiListAt(xs, i)` (runtime-guarded). The eslint rule `@typescript-eslint/no-non-null-assertion` is set to `warn` and the emitter pins it to allow the IR-justified case via a `// eslint-disable-next-line` comment annotated with the IR provenance.

**Length**: `len(xs)` lowers to `BigInt(xs.length)` when the surrounding context expects `bigint`, or `xs.length` when context expects `number`.

**`for x in xs`**: `for (const x of xs)`.

**List comprehensions**: `[f(x) for x in xs if pred(x)]` lowers to `xs.filter(pred).map(f)` when `f` and `pred` are pure synchronous arrow functions and the order is preserved (which it is for arrays). When the comprehension has nested loops (`[f(x, y) for x in xs for y in ys]`), the emitter falls back to a generator `Array.from((function*() { for (const x of xs) for (const y of ys) yield f(x, y); })())`. Phase 7 (query DSL) revisits this with iterator helpers (`Iterator.from(...).flatMap(...).map(...)`) for the longer chains.

**`push`, `pop`, `shift`, `unshift`**: only emitted when `Mutability` is mutable. The emitter refuses to emit these for a `readonly T[]`-typed value (which would be a `tsc` error anyway).

**Non-mutating alternatives (ES2023)**: `toReversed`, `toSorted`, `toSpliced`, `with` are preferred when the IR signals a `readonly T[]` source.

## Sub-phase 3.2, Maps

### Decisions made (3.2)

**Type**: `Map<K, V>` (or `ReadonlyMap<K, V>` view per IR mutability).

**Construction**: `{1: "a", 2: "b"}` lowers to `new Map<bigint, string>([[1n, "a"], [2n, "b"]])`. Object literals are not used as maps (the prototype-chain risk plus key-stringification semantic mismatch make `Map` the only acceptable choice).

**Get**: `m[k]` lowers to `mochiMapGet(m, k)` when Mochi's semantic is "panic if absent", or to a runtime-helper that returns the Mochi `Option<V>` when the IR signals the optional read. The emitter never uses the bare `m.get(k)` form for `m[k]` because `Map.prototype.get` returns `V | undefined`, which differs from `null` (Mochi's `T?` is `T | null`).

**Set**: `m[k] = v` lowers to `m.set(k, v)`.

**Has**: `k in m` lowers to `m.has(k)`.

**Delete**: `delete m[k]` lowers to `m.delete(k)`.

**Iteration**: `for (k, v) in m` lowers to `for (const [k, v] of m)`. Insertion order is guaranteed by the ECMAScript spec (Maps iterate in insertion order); this matches the vm3 ordering.

**Equality**: `Map`s use SameValueZero for key matching (`NaN` is a single key, `1 !== 1n`). The emitter never mixes `number` and `bigint` keys in one map (monomorphisation forces a single K type).

## Sub-phase 3.3, Sets

### Decisions made (3.3)

**Type**: `Set<T>` (or `ReadonlySet<T>` view per IR mutability).

**Construction**: `{1, 2, 3}` lowers to `new Set<bigint>([1n, 2n, 3n])`.

**Membership**: `x in s` lowers to `s.has(x)`.

**Add/remove**: `s.add(x)`, `s.delete(x)`.

**Operators**: Mochi `a + b`, `a & b`, `a - b`, `a ^ b` over sets lower to ES2024 set methods:

| Mochi          | TypeScript                  | ES2024 method      |
|----------------|-----------------------------|--------------------|
| `a + b`        | `a.union(b)`                | union              |
| `a & b`        | `a.intersection(b)`         | intersection       |
| `a - b`        | `a.difference(b)`           | difference         |
| `a ^ b`        | `a.symmetricDifference(b)`  | symmetricDifference|
| `a <= b`       | `a.isSubsetOf(b)`           | isSubsetOf         |
| `a >= b`       | `a.isSupersetOf(b)`         | isSupersetOf       |
| `disjoint?`    | `a.isDisjointFrom(b)`       | isDisjointFrom     |

These methods are TC39 Stage 4, native in Node 22, Deno 2, Bun 1.1, and Chromium 122+. Polyfilling is rejected; the runtime floor enforces availability.

## Sub-phase 3.4, Lists of records

### Decisions made (3.4)

Lists of records are the data shape every query, every datalog rule, and every fold in Phase 7 and 8 will iterate. The phase ships a minimum record surface (Phase 4 lands the full surface):

- Record declaration `record User { id: int, name: string }` emits a `class User { ... }` with `readonly` fields, private constructor, and a static `User.of({id, name})` factory.
- A list of records: `let users: [User] = [User.of({id: 1n, name: "alice"})]` lowers to `[User.of({id: 1n, name: "alice"})]` typed as `readonly User[]` or `User[]` per Mutability.
- Comprehension: `[u.name for u in users]` lowers to `users.map((u) => u.name)`.
- Filtering: `[u for u in users if u.id > 0n]` lowers to `users.filter((u) => u.id > 0n)`.

Sub-phase 3.4 includes record method call chains (`u.name.toUpperCase()` etc.) so that Phase 7's query DSL has a real target.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/lists.go` | List literal, indexing, length, push/pop, comprehensions |
| `transpiler3/typescript/lower/maps.go` | Map literal, get/set/has/delete, iteration |
| `transpiler3/typescript/lower/sets.go` | Set literal, ES2024 method dispatch |
| `transpiler3/typescript/lower/mutability.go` | Mutability inference; tags each collection occurrence as readonly or mutable |
| `runtime3/typescript/src/collections/index.ts` | `mochiListAt`, `mochiMapGet`, helpers |
| `transpiler3/typescript/build/phase03_test.go` | `TestPhase3Collections`, four sub-tests |
| `tests/transpiler3/typescript/fixtures/phase03.1-lists/` | 25 fixtures |
| `tests/transpiler3/typescript/fixtures/phase03.2-maps/` | 25 fixtures |
| `tests/transpiler3/typescript/fixtures/phase03.3-sets/` | 15 fixtures |
| `tests/transpiler3/typescript/fixtures/phase03.4-list-records/` | 20 fixtures |

## Test set

- `TestPhase3_1Lists`, `TestPhase3_2Maps`, `TestPhase3_3Sets`, `TestPhase3_4ListRecords`, each four-runtime.
- `TestPhase3NoObjectAsMap`, asserts no emitted `.ts` uses a plain object literal as a map.
- `TestPhase3IndexProvenance`, asserts every `xs[i]!` non-null assertion is annotated with an IR-provenance comment.

## Deferred work

- Full record surface (methods, equals, hashCode). Deferred to Phase 4.
- Frozen / persistent collections (`as const` deep readonly). Deferred to v2.
- `Object.groupBy` / `Map.groupBy` over lists of records. Deferred to Phase 7 (query DSL).
