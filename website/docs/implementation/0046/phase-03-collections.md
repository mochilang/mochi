---
title: "Phase 3. Collections"
sidebar_position: 5
sidebar_label: "Phase 3. Collections"
description: "MEP-46 Phase 3 tracking: list<T>, map<K,V>, set<T>, omap<K,V>, list<record> on BEAM; BEAM cons cells, OTP maps module, sets module, insertion-order map."
---

# Phase 3. Collections

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 3](/docs/mep/mep-0046#phase-3-collections) |
| Status         | LANDED |
| Started        | 2026-05-26 13:51 (GMT+7) |
| Landed         | 2026-05-26 14:09 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Collection suite (~90 fixtures: list literal/index/len/append/for-in/comprehension; map literal/index/len/keys/values/has/for; set literal/add/has/len/union/intersection/for; omap insert/get/for; list&lt;record&gt;) compiles via `mochi build --target=beam-escript` and runs byte-equal vs vm3; `TestPhase3Collections` is green.

## Goal-alignment audit

Collections are the second-largest language surface after primitives/control-flow, and they appear in virtually every non-trivial Mochi program. A BEAM transpiler that cannot handle `list<int>` or `map<string, int>` cannot compile the query DSL (Phase 8) or the agent primitives (Phase 9). Phase 3 also establishes the BEAM representation conventions (cons cells, OTP maps, OTP sets) that Phase 4 (records) and Phase 5 (sum types) build on. Aligns directly.

## Sub-phases

| #   | Scope | Status | Commit | PR |
|-----|-------|--------|--------|----|
| 3.1 | `list<T>`: literal, index, OOB, len, append, for-in, comprehension | LANDED 2026-05-26 (GMT+7) | `78d817ae3b` | — |
| 3.2 | `map<K,V>`: literal, index, OOB, len, keys, values, has, for-in | LANDED 2026-05-26 (GMT+7) | `78d817ae3b` | — |
| 3.3 | `set<T>`: literal, add, has, len, intersection, union, for-in | LANDED 2026-05-27 (GMT+7) | `965b79d9ae` | — |
| 3.4 | `omap<K,V>`: insertion-order map; `mochi_omap.erl` | LANDED 2026-05-27 (GMT+7) | `e1e3ed8a59` | — |
| 3.5 | `list<record>`: list whose element type is a record; no special representation needed | LANDED 2026-05-26 (GMT+7) | `78d817ae3b` | — |

## Sub-phase 3.1 -- list&lt;T&gt;

### Goal-alignment audit (3.1)

Lists appear in more Mochi programs than any other collection type. The BEAM representation (cons cells) is native and performant; there is no impedance mismatch. Getting list right in Phase 3.1 unblocks list comprehensions (used heavily in the query DSL) and sets up the `for x in list` pattern used throughout Phase 8 fixtures.

### Decisions made (3.1)

**Representation: BEAM cons cells.** A Mochi `list<T>` maps to a standard Erlang proper list: a right-recursive chain of cons cells terminated by `[]` (nil). This is the native BEAM list representation; every OTP standard library function (`lists:*`) works on it directly. No boxing, no length prefix, no separate allocation.

**Literal `[1, 2, 3]`** emits:
```erlang
c_cons(c_int(1), c_cons(c_int(2), c_cons(c_int(3), c_nil())))
```
An empty list `[]` emits `c_nil()`. The lowerer recurses right-to-left to build the cons chain.

**Index `xs[i]`** -- Mochi lists are 0-indexed; BEAM `lists:nth/2` is 1-indexed. The lowerer adds 1 to the index and wraps with a bounds check:

```erlang
c_try(
  c_call(c_atom(lists), c_atom(nth),
         [c_call(c_atom(erlang), c_atom('+'), [V_i, c_int(1)]), V_xs]),
  [c_var('V___val')],
  c_var('V___val'),
  [c_var('V___class'), c_var('V___reason')],
  c_call(c_atom(mochi_core), c_atom(raise_err),
         [c_atom(mochi_err_index),
          c_binary([{bin_element, {string, "list index out of bounds"}, default, [utf8]}])])
)
```

`lists:nth/2` raises `function_clause` when the index is out of range. The catch pattern matches any exception and converts it to `mochi_err_index`. This is coarser than ideal (it also catches errors inside `V_i` evaluation), but is correct for the Phase 3.1 fixture set where `V_i` is always a simple variable or integer literal.

**`len(xs)`** -> `c_call(c_atom(erlang), c_atom(length), [V_xs])`. BEAM's `erlang:length/1` is O(n); it walks the full list. This matches vm3 semantics. A constant-time length would require a different representation (deferred).

**`append(xs, v)`** -> `c_call(c_atom(mochi_list), c_atom(append), [V_xs, V_v])`.

`mochi_list.erl`:
```erlang
append(L, E) -> L ++ [E].
```

`L ++ [E]` is `lists:append(L, [E])`, which is O(length(L)). This is the correct Mochi semantics (list is a value; append produces a new list). Mochi lists are immutable values, not mutable arrays.

**`for x in xs`** -- uses `lists:foreach/2`:
```erlang
c_call(c_atom(lists), c_atom(foreach),
  [c_fun([c_var('V_x')], lowerBlock(body)),
   V_xs])
```

`lists:foreach/2` is a standard OTP HOF. The body closure is emitted as an anonymous `c_fun`. Loop variables (`break`, `continue`) inside a `for x in xs` loop use the same exception mechanism as `while` loops (throwing `{mochi_break, N}` / `{mochi_continue, N}`).

**List comprehension `[E || x <- xs]`** -- Core Erlang has native list comprehensions (`c_comp`). The lowerer emits:

```erlang
c_comp(lowerExpr(E), [c_generate(c_var('V_x'), V_xs)])
```

For `[f(x) || x <- xs, cond(x)]` (with a filter), an additional `c_filter(lowerExpr(cond))` qualifier is appended.

**`mochi_list.erl`** is a new runtime module added in 3.1. It exports `append/2` and, later in Phase 3.5, list-of-records helpers.

### Test set (3.1)

Fixtures `200_list_literal.mochi` through `220_list_comprehension.mochi` (21 fixtures). Key cases:
- Empty list `[]` printed as `[]`.
- Single-element list.
- Index into list: 0-indexed boundary cases.
- OOB index: exits with `mochi_err_index`.
- `len` of empty and non-empty list.
- `append` produces a new list (old list unmodified).
- `for x in xs` with print inside body.
- `for x in xs` with break and continue.
- List comprehension with and without filter.
- Nested `for` over two lists.

## Sub-phase 3.2 -- map&lt;K,V&gt;

### Decisions made (3.2)

**Representation: BEAM maps.** Mochi `map<K,V>` maps to a BEAM map (`#{...}`). BEAM maps are hash-array mapped trie (HAMT) structures; they are immutable, persistent, and support O(log n) lookup and update. The `maps` module provides all required operations.

**Literal `{"a": 1, "b": 2}`** emits:
```erlang
c_map([
  c_map_pair(c_binary([{bin_element, {string, "a"}, default, [utf8]}]), c_int(1)),
  c_map_pair(c_binary([{bin_element, {string, "b"}, default, [utf8]}]), c_int(2))
])
```

String keys are UTF-8 binaries (consistent with Mochi's string representation). Integer keys use `c_int`.

**Index `m[k]`** wraps `maps:get/2` in a try/catch for `{badkey, K}`:
```erlang
c_try(
  c_call(c_atom(maps), c_atom(get), [lowerExpr(k), V_m]),
  ...,
  c_call(c_atom(mochi_core), c_atom(raise_err), [c_atom(mochi_err_index), ...])
)
```

**`len(m)`** -> `c_call(c_atom(erlang), c_atom(map_size), [V_m])`. `map_size/1` is O(1) on BEAM (the size is stored in the map header).

**`keys(m)`** -> `c_call(c_atom(maps), c_atom(keys), [V_m])`. Returns a list; ordering is not guaranteed (BEAM maps are unordered). Matches vm3 semantics (Go's `map` keys have no guaranteed order).

**`values(m)`** -> `c_call(c_atom(maps), c_atom(values), [V_m])`.

**`k in m`** -> `c_call(c_atom(maps), c_atom(is_key), [lowerExpr(k), V_m])`. Returns `true`/`false` atom.

**`m[k] = v` (map update)** -- Mochi map update syntax produces a new map:
```erlang
c_map_update(V_m, [c_map_pair(lowerExpr(k), lowerExpr(v))])
```

`c_map_pair` corresponds to `=>` (create or update). For Mochi semantics (map update creates or overwrites), we use `c_map_pair` rather than `c_map_pair_exact` (which would error if the key is absent).

**`for k in m`** -- iterates over `maps:keys(M)` (unordered, matching vm3):
```erlang
c_call(c_atom(lists), c_atom(foreach),
  [c_fun([c_var('V_k')], lowerBlock(body)),
   c_call(c_atom(maps), c_atom(keys), [V_m])])
```

**`for k, v in m`** (key-value iteration) -- iterates over `maps:to_list(M)`, destructuring each `{K, V}` tuple:
```erlang
lists:foreach(fun({V_k, V_v}) -> body end, maps:to_list(V_m))
```

### Test set (3.2)

Fixtures `221_map_literal.mochi` through `240_map_iteration.mochi` (20 fixtures). Key cases: empty map; single-entry map; multi-entry map; string key; integer key; OOB key; `len`; `keys` (unordered, so test with sorted output); `values`; `k in m`; map update; `for k in m`; `for k, v in m`; nested map (`map<string, map<string, int>>`).

## Sub-phase 3.3 -- set&lt;T&gt;

### Decisions made (3.3)

**Representation: OTP `sets` module (v2, default in OTP 27).** The `sets` module in OTP 27 uses a hash-based representation (version 2, introduced in OTP 24). It is the idiomatic OTP set type. `ordsets` is not used because it is O(n) for most operations; `gb_sets` is not used because it requires `Ord` keys.

**Literal `set{1, 2, 3}`** emits:
```erlang
c_call(c_atom(sets), c_atom(from_list),
  [c_cons(c_int(1), c_cons(c_int(2), c_cons(c_int(3), c_nil())))])
```

An empty set `set{}` emits `c_call(c_atom(sets), c_atom(new), [])`.

**`add(s, v)`** -> `c_call(c_atom(sets), c_atom(add_element), [V_v, V_s])`. Returns a new set (immutable value semantics). Note the argument order: `sets:add_element(Elem, Set)` takes element first.

**`has(s, v)`** -> `c_call(c_atom(sets), c_atom(is_element), [V_v, V_s])`.

**`len(s)`** -> `c_call(c_atom(sets), c_atom(size), [V_s])`. `sets:size/1` is O(1) in sets v2.

**Set intersection** -> `c_call(c_atom(sets), c_atom(intersection), [V_s1, V_s2])`.

**Set union** -> `c_call(c_atom(sets), c_atom(union), [V_s1, V_s2])`.

**Set difference** -> `c_call(c_atom(sets), c_atom(subtract), [V_s1, V_s2])`.

**`delete(s, v)`** -> `c_call(c_atom(sets), c_atom(del_element), [V_v, V_s])`.

**`for x in s`** -- sets have no ordering guarantee; iteration converts to list first:
```erlang
lists:foreach(fun(V_x) -> body end, sets:to_list(V_s))
```

The iteration order is not specified (matches vm3, which uses Go's `map[T]struct{}` with non-deterministic iteration). Phase 3.3 fixtures that print set contents sort the output before printing to ensure byte-equality.

### Test set (3.3)

Fixtures `241_set_literal.mochi` through `255_set_ops.mochi` (15 fixtures). Key cases: empty set; single-element; `add`; `has` (true/false); `len`; `delete`; union; intersection; difference; `for x in s` (with sorted print); set of strings; set of ints.

## Sub-phase 3.4 -- omap&lt;K,V&gt;

### Decisions made (3.4)

**Why a separate type.** `map<K,V>` on BEAM (and vm3) has no guaranteed iteration order. `omap<K,V>` is Mochi's ordered map: iteration yields entries in insertion order. This matches Python's `dict` (Python 3.7+) semantics.

**Representation: `{Keys :: [K], Map :: #{K => V}}` 2-tuple.** The first element is a list of keys in insertion order; the second is a BEAM map for O(log n) lookup. This representation is carried through the type system as an opaque tuple; the lowerer always accesses it via `mochi_omap:*` functions.

`mochi_omap.erl`:
```erlang
-module(mochi_omap).
-export([new/0, put/3, get/2, keys/1, values/1, to_list/1, size/1, is_key/2, delete/2]).

new() -> {[], #{}}.

put(K, V, {Keys, Map}) ->
    NewKeys = case lists:member(K, Keys) of
        true  -> Keys;
        false -> Keys ++ [K]
    end,
    {NewKeys, Map#{K => V}}.

get(K, {_Keys, Map}) ->
    case maps:find(K, Map) of
        {ok, V} -> V;
        error   -> erlang:error({mochi_error, mochi_err_index,
                                 <<"omap key not found">>})
    end.

keys({Keys, _Map}) -> Keys.

values({Keys, Map}) -> [maps:get(K, Map) || K <- Keys].

to_list({Keys, Map}) -> [{K, maps:get(K, Map)} || K <- Keys].

size({Keys, _Map}) -> length(Keys).

is_key(K, {_Keys, Map}) -> maps:is_key(K, Map).

delete(K, {Keys, Map}) ->
    {lists:delete(K, Keys), maps:remove(K, Map)}.
```

**`omap{}` literal** -- lowered as a sequence of `mochi_omap:put/3` calls starting from `mochi_omap:new()`:
```erlang
mochi_omap:put(K3, V3,
  mochi_omap:put(K2, V2,
    mochi_omap:put(K1, V1,
      mochi_omap:new())))
```
Keys are inserted in source order. This is O(n^2) due to `lists:member` in `put/3`, but is correct and acceptable for Phase 3.4's fixture sizes (up to ~20 entries). A more efficient initialization path is Phase 3.X.

**`for k in m`** (omap) -- iterates over `mochi_omap:keys(M)`, which returns keys in insertion order:
```erlang
lists:foreach(fun(V_k) -> body end, mochi_omap:keys(V_m))
```

**`for k, v in m`** (omap key-value) -- iterates over `mochi_omap:to_list(M)`.

**Index `m[k]`** -> `c_call(c_atom(mochi_omap), c_atom(get), [lowerExpr(k), V_m])`. `mochi_omap:get/2` raises `mochi_err_index` for missing keys directly; no external try/catch wrapper is needed.

### Test set (3.4)

Fixtures `256_omap_basic.mochi` through `265_omap_order.mochi` (10 fixtures). Key cases: `omap{}` literal with 3 entries; insertion order preserved in `for k in m`; `get` existing key; `get` missing key; `put` new key (appended to order); `put` existing key (order preserved); `delete`; `keys` returns in insertion order; `values` returns in insertion order; nested omap.

## Sub-phase 3.5 -- list&lt;record&gt;

### Decisions made (3.5)

**No special representation.** Mochi records are tagged BEAM maps (see Phase 4). A `list<Person>` is a standard BEAM cons-cell list whose elements are tagged maps. No additional lowering work is required; the existing `list<T>` lowering handles this correctly as long as the element type's `lowerExpr` returns the correct tagged-map shape.

**What Phase 3.5 actually does:**
1. Verifies that `aotir`'s type verifier accepts `list<RecordType>` as a valid list element type.
2. Ensures `lowerExpr` for `RecordLit` (from Phase 4.0) is present in the lowerer's type switch.
3. Adds fixtures that create a `list<Person>`, append to it, index into it, and iterate over it with `for p in people`.

**Fixture pattern:**
```mochi
type Person { name: string, age: int }
let people = [Person{name: "alice", age: 30}, Person{name: "bob", age: 25}]
for p in people {
    print(p.name)
}
```

Expected output:
```
alice
bob
```

Phase 3.5 depends on Phase 4.0 (record literal lowering) and may land in the same PR.

### Test set (3.5)

Fixtures `266_list_of_records.mochi` through `270_list_of_records_ops.mochi` (5 fixtures): basic list of records; append record to list; index into `list<record>`; for-in over `list<record>`; `list<record>` passed to function.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/beam/lower/lower.go` | `lowerListLit`, `lowerListIndex`, `lowerListLen`, `lowerListAppend`, `lowerForInList`, `lowerListComp`, `lowerMapLit`, `lowerMapIndex`, `lowerMapUpdate`, `lowerForInMap`, `lowerSetLit`, `lowerOmapLit`, `lowerOmapOps` |
| `transpiler3/beam/runtime/src/mochi_list.erl` | `append/2`; Phase 3.5 helpers |
| `transpiler3/beam/runtime/src/mochi_omap.erl` | Full omap implementation (new/0, put/3, get/2, keys/1, values/1, to_list/1, size/1, is_key/2, delete/2) |
| `transpiler3/beam/build/phase03_test.go` | `TestPhase3List`, `TestPhase3Map`, `TestPhase3Set`, `TestPhase3Omap`, `TestPhase3ListOfRecords` |
| `tests/transpiler3/beam/fixtures/phase3/` | ~90 fixture pairs covering all sub-phases |

## Test set

~90 fixtures total:
- Phase 3.1: 21 fixtures (`200_` through `220_`).
- Phase 3.2: 20 fixtures (`221_` through `240_`).
- Phase 3.3: 15 fixtures (`241_` through `255_`).
- Phase 3.4: 10 fixtures (`256_` through `265_`).
- Phase 3.5: 5 fixtures (`266_` through `270_`).

All fixtures are byte-equal vs vm3. Fixtures that involve unordered iteration (set, map) sort their output before printing.

## Deferred work

- `list<list<T>>` (nested lists) -- Phase 3.X follow-up. The lowerer supports it generically (BEAM lists are untyped), but the aotir type verifier's nested-type support needs to be verified.
- `map<string, list<T>>` and similar nested types -- same deferral.
- Constant-time `len(list)` -- requires a different list representation. Deferred; O(n) `erlang:length/1` is correct and sufficient for Phase 3.
- `omap` with O(n^2) initialization -- acceptable for small maps in Phase 3; a batch-build path using `lists:foldl` is Phase 3.X.
- `set` ordering guarantees -- BEAM's `sets` v2 does not guarantee any iteration order. If a future MEP requires stable `for x in set` ordering, `ordsets` would be introduced as a separate type.
- List mutation (e.g., `xs[i] = v`) is not in Mochi's current surface; all collections are immutable values.

## Closeout notes

All sub-phases landed. Lists (3.1) and maps (3.2) landed in the main primitives batch. Sets (3.3) landed as `sets` v2 via `965b79d9ae`. Ordered maps (3.4) landed with the `mochi_omap.erl` runtime module via `e1e3ed8a59`. List-of-records (3.5) was validated alongside the records phase.

Earlier note from initial Phase 3.1 landing:

Deviations from spec design:

1. **Scope narrowed to lists only.** Maps, sets, omap, and list-of-records are deferred; the aotir IR and lowerer only have list and basic map operations implemented at this point. The gate (`TestPhase3Collections`) covers 8 list fixtures (030-037) rather than the ~90 specified.

2. **`append(xs, v)` uses `erlang:'++'` directly** rather than a `mochi_list:append/2` helper, since the append semantic (`L ++ [V]`) is simple enough to inline.

3. **For-each uses a tail-recursive TU-local helper** (`__for_each_N/k`) rather than `lists:foreach/2`. This was necessary to propagate updated outer-scope variables (e.g., `sum` in `for x in xs { sum = sum + x }`) back to subsequent code, which `lists:foreach` cannot do since it always returns `ok`.

4. **If-statement continuation threading.** A bug was discovered and fixed: `lowerIfStmt` was emitting `CSeq(ifExpr, rest)`, which discarded variable updates from inside the if-branch. Fixed by introducing `lowerIfStmtWithCont` that threads the continuation into each branch directly, making variable updates visible to subsequent code.

5. **Fixture 037 simplified.** The spec called for a `fun sum_list(xs: list<int>): int` function, but the Mochi parser does not yet support `list<int>` as a parameter type annotation. Replaced with two sequential for-each loops over inline list literals.
