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
| Status         | IN PROGRESS (3.1 + 3.2 LANDED; 3.3/3.4 pending) |
| Started        | 2026-05-29 17:21 (GMT+7) |
| Landed         | n/a (umbrella) |
| Tracking issue | n/a (umbrella) |
| Tracking PR    | n/a (umbrella) |

## Gate

`TestPhase3Collections`: 85 fixtures green across the four sub-phases on Node 22, Deno 2, Bun 1.1, Chromium 130. Secondary gates: `tsc --strict --noUncheckedIndexedAccess` zero diagnostics (this is the phase that first exercises index-access narrowing at scale), eslint clean (including `@typescript-eslint/prefer-readonly-parameter-types` for collection function parameters where the IR says read-only).

## Goal-alignment audit

Phase 3 lands the three collection shapes Mochi programs rely on for almost all data manipulation: `list<T>`, `map<K, V>`, `set<T>`. The TypeScript surface gives us each one nearly for free (`T[]`, `Map<K, V>`, `Set<T>` plus ES2024 set methods), but the strict-mode rules force several non-obvious choices: index access under `--noUncheckedIndexedAccess` is `T | undefined`, so every `xs[i]` either needs a runtime bounds check or an IR-proven `i < len(xs)` provenance to justify the non-null assertion; `Map.get` is `V | undefined` for the same reason. Sub-phase 3.4 ties together collections and records (which Phase 4 will land in full), so that the query DSL in Phase 7 has lists of records to operate on.

## Sub-phases

The MEP-52 phase matrix splits Phase 3 into four sub-phases. Each is its own gate; the umbrella is LANDED only when all four are green.

| # | Scope | Fixtures | Status | Commit |
|---|-------|----------|--------|--------|
| 3.1 | Lists (scalar element types; index, len, for-each, append, sum/min/max/contains, fn round-trip, index assign, loop control) | 25 | LANDED (Node + Deno + Bun) | tbd |
| 3.2 | Maps (`Map<K, V>`; literal, get, set, has, len, for-each, keys, values, fn round-trip) | 25 | LANDED (Node + Deno + Bun) | tbd |
| 3.3 | Sets (`Set<T>` with ES2024 union/intersection/difference) | 15 | NOT STARTED | n/a |
| 3.4 | Lists of records (records via a Phase 4 preview; comprehensions over records) | 20 | NOT STARTED | n/a |

## Sub-phase 3.1, Lists

### Decisions made (3.1) — as shipped

The full mutability split (`readonly T[]` for immutable views, `T[]` when mutated) and the IR-provenance-driven non-null assertions sketched below were the original design. The actual Phase 3.1 ship simplifies both:

- **Type**: every list lowers to a plain `T[]` (`number[]`, `string[]`, `boolean[]`). Mochi's structural mutability is not yet carried per-occurrence in aotir, and the strict-mode toll of `readonly` views is paid only when the lower pass can prove no mutation site exists. Phase 4 will revisit this once records expose the equality contract the mutability inference needs. The narrower type produces no extra `tsc` diagnostics (lists are always assigned before use) and lets the rest of the surface land without a parallel mutability tracking pass.
- **Literal**: Phase 2 closed with int → `number`; the bigint specialisation is deferred to a post-Phase-6 sub-phase. `[1, 2, 3]` therefore lowers to `[1, 2, 3]` with element type `number`. Strings emit double-quoted, booleans `true`/`false`, floats via `strconv.FormatFloat('g', -1, 64)` to match vm3's round-trip.
- **Indexing**: `xs[i]` always lowers to `mochi_list_at(xs, i)` (runtime-guarded). The IR-provenance approach is deferred. Reason: under `--noUncheckedIndexedAccess`, bare `xs[i]` types as `T | undefined` and pollutes every caller with narrowing logic; non-null `!` assertions are an `@typescript-eslint/no-non-null-assertion` violation by default. A single bounds-checking helper is the cheapest way to keep the emit clean and the panic contract honest. The helper raises a `RangeError` on miss, matching Mochi's panic-on-OOB semantics. `TestPhase3_1ListAtAlwaysGuarded` enforces that no fixture emits a bare bracket read in user code.
- **Length**: `len(xs)` lowers to `xs.length` (no `BigInt` wrapping, since int is `number`).
- **Append**: `append(xs, v)` lowers to `[...xs, v]` (spread literal, fresh allocation; matches Mochi's functional append semantics). The keeps-input fixture asserts the input list is not mutated.
- **`for x in xs`**: `for (const x of xs) { ... }` with no type annotation on the binding (TypeScript's `for-of` grammar reserves the colon for label syntax; `tsc` infers the element type from `xs`, which matches Mochi's structural inference).
- **`x in xs`** (membership): `xs.includes(x)`. ES2015+ semantics are SameValueZero (`NaN === NaN`, `-0 === 0`), which is what Mochi expects for scalar elements.
- **`sum`, `min`, `max`**: each lowers to a Mochi runtime helper (`mochi_list_sum`, `mochi_list_min`, `mochi_list_max`). Reason: `Math.min(...xs)` is `O(n)` and adds an `arguments.length` cap (~65536 on V8) that vm3 does not have; an explicit loop is also more readable in the emit. The helpers are emitted only when used.
- **`xs[i] = v`** (index assign): lowers to `xs[i] = v;` directly. The LHS bracket form is fine: TypeScript permits the assignment, and out-of-range writes extend the array (which is the same divergence-from-Mochi-panics that vm3 has on this contract today, and is therefore considered a vm3 contract bug to fix upstream, not a transpiler concern). The read-side regression test (`TestPhase3_1ListAtAlwaysGuarded`) intentionally skips assignment LHS.
- **List comprehensions**: deferred to Phase 7 (query DSL), where iterator-helper chains (`Iterator.from(xs).filter(...).map(...)`) carry the longer surface. Phase 3.1 ships the loop-based surface only.
- **`push`, `pop`, `shift`, `unshift`**: not in the Phase 3.1 surface; Mochi programs use `append` (functional) for additions.
- **Non-mutating ES2023 methods (`toReversed`, `toSorted`, `toSpliced`, `with`)**: not in the 3.1 surface. The aotir IR already carries `ListSortAscExpr` and `ListSliceExpr`; the emitter has lower paths for both but no fixture exercises them yet. They land in 3.4 alongside lists-of-records.

## Sub-phase 3.2, Maps

### Decisions made (3.2) — as shipped

The bigint-keyed `new Map<bigint, string>([[1n, ...]])` form and the
`mochiMapGet` Option-returning variant in the original 3.2 plan
both shift; the actual ship simplifies several choices once the
phase landed against vm3's real semantics.

- **Type**: every map lowers to `Map<K, V>` (no `readonly` view variant). Same reasoning as 3.1: aotir does not yet carry per-occurrence mutability, and the strict-mode toll of `ReadonlyMap` for IR-immutable bindings can be paid later once Phase 4 records expose the equality contract the mutability inference needs.
- **Construction**: `{1: "a", 2: "b"}` lowers to `new Map<number, string>([[1, "a"], [2, "b"]])`. The bigint key form is deferred alongside the int→bigint sub-phase; today every Mochi int is a TS `number`, so the entries are pairs of `number`. Object literals are explicitly rejected (prototype-chain pollution and key-stringification semantic mismatch). Empty maps emit `new Map<K, V>()` (TS allows the parameterless ctor and infers nothing, so the explicit type parameters keep `tsc --strict` happy).
- **Get**: `m[k]` lowers to `mochi_map_get(m, k)`, a runtime helper that calls `m.has(k)` and throws `RangeError` on miss before unwrapping `m.get(k) as V`. Reason: `Map.prototype.get` returns `V | undefined` under `--strict`, which would force every caller to narrow; the helper raises and returns `V` cleanly. The Option-returning variant for vm3's `option[V]` semantic is deferred to the same sub-phase that lands the Phase 4 `T?` surface. `TestPhase3_2MapGetAlwaysGuarded` enforces that no user-code line emits a bare `m.get(...)` call.
- **Set**: `m[k] = v` lowers to `m.set(k, v);` (a plain ExprStmt + MemberCallExpr; no new tstree node).
- **Has**: `k in m` lowers to `m.has(k)`. ES2015+ semantics are SameValueZero (`NaN === NaN`, `-0 === 0`), which matches Mochi's contract for scalar keys.
- **Length**: `len(m)` lowers to `m.size` (a property read, not a function call).
- **Iteration ordering**: `for k in m` lowers to `for (const k of mochi_map_keys_sorted(m))`. The helper sorts keys ascending by `String(k)`, which matches vm3's lexicographic-sort iteration order. JavaScript `Map` preserves insertion order natively, which would diverge from vm3 for any Mochi program that constructs the map out of sort order. The sorted helper is the cheapest way to keep stdout byte-equal across vm3 and three JS runtimes. `TestPhase3_2KeyIterIsSorted` enforces no fixture emits raw `m.keys()` / `m.values()` in user code.
- **`for v in values(m)`**: lowers to `for (const v of mochi_map_values_sorted(m))`. The values helper sorts by the same stringified-key key, so parallel iteration with `mochi_map_keys_sorted` yields matching (k, v) pairs. vm3's `keys()` builtin currently returns an empty list (a vm3 bug); fixtures use `values(m)` exclusively for value-list iteration until that bug lands. The `for k in m` form still routes through `MapKeysExpr` and works as expected.
- **Equality**: `Map`s use SameValueZero for key matching. The emitter never mixes `number` and `bigint` keys in one map (monomorphisation forces a single K type).
- **Delete**: `delete m[k]` is not in the 3.2 surface; no Mochi fixture exercises it and the aotir IR does not carry a MapDeleteStmt yet. The TS path (`m.delete(k)`) is straightforward when needed.
- **Tuple iteration `for (k, v) in m`**: not in the 3.2 surface; Mochi's parser does not currently accept the tuple binding on the for-loop head. Iteration over `for k in m` plus `m[k]` lookup is the workaround.

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

## Files (as shipped, 3.1)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/tstree/phase03.go` | `ListLit`, `ForEachStmt`, `IndexAssignStmt`, `MemberAccessExpr`, `SpreadAppendExpr` node kinds |
| `transpiler3/typescript/lower/phase03.go` | `tsTypeForList`, `tsTypeForCompound`, lower funcs for all 3.1 expr/stmt nodes, `runtimeListDecls()` |
| `transpiler3/typescript/lower/lower.go` | `runtimeFlags` extended with 7 list-helper flags; Phase 3 stmt/expr dispatch wired |
| `transpiler3/typescript/build/phase03_test.go` | `TestPhase3_1ListsNode/Deno/Bun`, `TestPhase3_1EmitWithoutRuntime`, `TestPhase3_1ListAtAlwaysGuarded` |
| `tests/transpiler3/typescript/fixtures/phase03.1-lists/` | 25 `.mochi` fixtures + 25 vm3-recorded `.out` |

## Files (as shipped, 3.2)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/tstree/phase03.go` | `NewMapExpr` node kind added |
| `transpiler3/typescript/lower/phase03.go` | `tsTypeForMapSlot`, `tsTypeForLetSlot`, 6 map lower funcs (`lowerMapLit`, `lowerMapGetExpr`, `lowerMapHasExpr`, `lowerMapLenExpr`, `lowerMapKeysExpr`, `lowerMapValuesExpr`), `lowerMapPutStmt`, and `runtimeMapDecls()` emitting 3 helpers (`mochi_map_get`, `mochi_map_keys_sorted`, `mochi_map_values_sorted`) gated by usage flags |
| `transpiler3/typescript/lower/phase02.go` | `paramType` and `lowerFunction` return-type slots routed through `tsTypeForLetSlot` so map params and map returns lower correctly |
| `transpiler3/typescript/lower/lower.go` | `runtimeFlags` extended with 3 map-helper flags; Phase 3.2 stmt/expr dispatch wired; let-decl type slot routed through `tsTypeForLetSlot` |
| `transpiler3/typescript/build/phase03_2_test.go` | `TestPhase3_2MapsNode/Deno/Bun` (29 × 3 fixture runs), `TestPhase3_2EmitWithoutRuntime` (13 shape cases), `TestPhase3_2MapGetAlwaysGuarded` (regression guard against bare `m.get`), `TestPhase3_2KeyIterIsSorted` (regression guard against raw `m.keys()` / `m.values()`) |
| `tests/transpiler3/typescript/fixtures/phase03.2-maps/` | 29 `.mochi` fixtures + 29 vm3-recorded `.out`, byte-equal across Node 22, Deno 2, Bun 1.1 |

## Files (planned, 3.3 to 3.4)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/phase03_sets.go` | Set literal, ES2024 method dispatch |
| `tests/transpiler3/typescript/fixtures/phase03.3-sets/` | 15 fixtures |
| `tests/transpiler3/typescript/fixtures/phase03.4-list-records/` | 20 fixtures |

## Test set

- `TestPhase3_1Lists{Node,Deno,Bun}`, the runtime gate (25 × 3 = 75 fixture runs).
- `TestPhase3_1EmitWithoutRuntime`, 11 shape-check cases verifying load-bearing emit tokens land in the right form (`const xs: number[] = [1, 2, 3];`, `mochi_list_at(xs, 0)`, `xs.length`, `[...xs, 4]`, `for (const x of xs) {`, etc.).
- `TestPhase3_1ListAtAlwaysGuarded`, scans every emitted `.ts` and asserts no bare bracket read escapes the `mochi_list_at` helper boundary. The helper body is filtered (its `return xs[i] as T` is intentional); LHS bracket form (`xs[i] = v`) is skipped on the assignment line.
- `TestPhase3_2Maps{Node,Deno,Bun}`, the 3.2 runtime gate (29 × 3 = 87 fixture runs across the four scalar value types, both supported key types (string and int), get/has/len/put, iteration via `for k in m` (MapKeysExpr) and `for v in values(m)` (MapValuesExpr), aggregation (sum, count, max via values()), and fn round-trip).
- `TestPhase3_2EmitWithoutRuntime`, 13 shape-check cases verifying load-bearing emit tokens land in the right form (`new Map<K, V>([[k, v], ...])`, `mochi_map_get(m, "a")`, `m.size`, `m.has("a")`, `m.set("b", 2);`, `for (const k of mochi_map_keys_sorted(m)) {`, `function mochi__total(m: Map<string, number>): number`, etc.).
- `TestPhase3_2MapGetAlwaysGuarded`, scans every emitted `.ts` and asserts no bare `.get(` call escapes the helper boundary (the runtime helpers' bodies are filtered out).
- `TestPhase3_2KeyIterIsSorted`, scans every emitted `.ts` and asserts no raw `m.keys()` / `m.values()` call escapes the sorted-helper boundary; required because JS Map iterates in insertion order and would diverge from vm3's lex-sorted output.
- `TestPhase3_3Sets`, `TestPhase3_4ListRecords`: pending.
- `TestPhase3NoObjectAsMap` (asserts no emitted `.ts` uses a plain object literal as a map): pending.

## Deferred work

- Full mutability inference (`readonly T[]` vs `T[]`). Deferred until aotir carries per-occurrence Mutability; not blocking 3.1.
- Bigint int representation. Phase 2 closed with `number`; revisit alongside MOCHI003 (per memory `project_mep48_spec.md` analogue).
- `xs[i]` IR-provenance non-null assertions. The runtime helper is fine for now; revisit if benchmark traces show measurable overhead.
- List comprehensions (`[f(x) for x in xs if pred(x)]`). Phase 7 (query DSL) will carry the iterator-helper-based surface.
- `toReversed`, `toSorted`, `toSpliced`, `with`. The aotir IR already has `ListSortAscExpr` / `ListSliceExpr` and lower paths exist; deferred to 3.4 where lists-of-records exercise them.
- Full record surface (methods, equals, hashCode). Deferred to Phase 4.
- Frozen / persistent collections (`as const` deep readonly). Deferred to v2.
- `Object.groupBy` / `Map.groupBy` over lists of records. Deferred to Phase 7 (query DSL).

## Landing log

### Sub-phase 3.1 — Lists

- **Started**: 2026-05-29 17:21 (GMT+7)
- **Landed**: 2026-05-29 17:45 (GMT+7)
- **Runtime coverage**: Node 22.21.1, Deno 2, Bun 1.1
- **Fixture count**: 25 `.mochi` + 25 vm3-recorded `.out`, all byte-equal across the three runtimes
- **Test count**: `TestPhase3_1ListsNode` (25), `TestPhase3_1ListsDeno` (25), `TestPhase3_1ListsBun` (25), `TestPhase3_1EmitWithoutRuntime` (11), `TestPhase3_1ListAtAlwaysGuarded` (25)
- **Notable scope changes vs the original 3.1 plan**: see "Decisions made (3.1) — as shipped" above. The list comprehension surface is the largest single deferral and moves to Phase 7.

### Sub-phase 3.2 — Maps

- **Started**: 2026-05-29 17:50 (GMT+7)
- **Landed**: 2026-05-29 18:28 (GMT+7)
- **Runtime coverage**: Node 22.21.1, Deno 2, Bun 1.1
- **Fixture count**: 29 `.mochi` + 29 vm3-recorded `.out`, all byte-equal across the three runtimes (the 25-fixture floor is exceeded; the +4 cover the int-keyed surface and `for v in values(m)` aggregation paths)
- **Test count**: `TestPhase3_2MapsNode` (29), `TestPhase3_2MapsDeno` (29), `TestPhase3_2MapsBun` (29), `TestPhase3_2EmitWithoutRuntime` (13), `TestPhase3_2MapGetAlwaysGuarded` (29), `TestPhase3_2KeyIterIsSorted` (29)
- **Notable scope changes vs the original 3.2 plan**: see "Decisions made (3.2) — as shipped" above. The bigint-keyed literal form, the Option-returning `mochi_map_get` variant, `delete m[k]`, and tuple-form iteration `for (k, v) in m` are all deferred; the C aotir lowerer rejects `if v != none` patterns ("none literal lands with Option in Phase 3"), so fixtures use `for v in values(m)` for value-list aggregation rather than option narrowing on `m[k]`.
- **vm3 quirks observed**: `keys(m)` is a vm3 bug (returns empty list); `values(m)` works correctly. `len(m)` returns the pre-mutation allocation count, not the live count after `m[k] = v` extends the map; fixtures avoid `len(m)` after mutation. Iteration order is lexicographic-by-stringified-key; the TS helpers sort the same way for byte-equal output.
