---
title: "Phase 3. Lists, maps, sets"
sidebar_position: 5
sidebar_label: "Phase 3. Collections"
description: "MEP-53 Phase 3, list / map / set lowering with Vec, HashMap, HashSet."
---

# Phase 3. Lists, maps, sets

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | f6d9c68bb3 |

## Gate

`TestPhase3Collections` walks `tests/transpiler3/rust/fixtures/phase03-collections/` (12 fixtures) and asserts byte-equal stdout. Coverage: list literal, indexing, slice, append, len, sort, in (contains); map literal, `m[k]`, has, keys, values; set literal, add, has, set arithmetic.

## Lowering decisions

`list<T>` → `Vec<T>`, `map<K, V>` → `std::collections::HashMap<K, V>`, `set<T>` → `std::collections::HashSet<T>`. List literals use `vec![1, 2, 3]`; map and set literals use `HashMap::from([(k, v), ...])` / `HashSet::from([1, 2])` (the `from` impls in stable since 1.56). Indexing routes through `mochi_runtime::check::list_index(&xs, i)` to raise panic code 4 on out-of-range (matching vm3 semantics).

Mutation is functional: `append(xs, v)` lowers to `{ let mut __t = xs.clone(); __t.push(v); __t }` rather than `xs.push(v)` to preserve Mochi's value semantics. The clone is gated by the colour pass in phase 6 — phase 3 always clones, phase 6 elides where the original is dead afterward.

`sort(xs)` lowers to `{ let mut __t = xs.clone(); __t.sort(); __t }` (panics on `Vec<f64>` because f64 doesn't impl `Ord` — Mochi rejects this at typecheck via `OrdConstraint`). `slice(xs, lo, hi)` is `xs.get(lo..hi).unwrap_or(&[]).to_vec()` so out-of-range slices return an empty Vec instead of panicking, matching vm3.

`in(x, xs)` is `xs.contains(&x)`; works for Vec, HashMap (uses key), and HashSet.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/lower.go` | Wire Vec / HashMap / HashSet lowerings |
| `runtime3/rust/mochi-runtime/src/lib.rs` | Add `check::list_index` |
| `transpiler3/rust/build/phase03_test.go` | 12-fixture gate |
| `tests/transpiler3/rust/fixtures/phase03-collections/*.mochi` + `.out` | 12 fixtures |

## Test set

- `TestPhase3Collections/<fixture>` for each `.mochi` in the fixture directory (12 fixtures).

## Closeout notes

HashMap iteration order is unspecified in Rust stdlib, which would cause stdout mismatch against vm3 (which uses insertion-ordered maps via a Go map + slice). Phase 3 sidesteps this by only printing maps in tests that explicitly sort keys or iterate values via a known-order key list. Phase 7 (query) handles the order-preserving cases via `BTreeMap` for `omap` semantics; phase 3 stays with HashMap.
