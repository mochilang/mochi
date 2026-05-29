---
title: "Phase 7. Query DSL"
sidebar_position: 9
sidebar_label: "Phase 7. Query"
description: "MEP-53 Phase 7, Mochi query DSL (from / where / select / order / skip / take) lowered to Rust iterator chains."
---

# Phase 7. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | cf15291eb7 |

## Gate

`TestPhase7Query` walks `tests/transpiler3/rust/fixtures/phase07-query/` (43 fixtures) and asserts byte-equal stdout. Coverage: single-source query, where clause, select projection (scalar, record, anonymous record), order by ascending / descending, skip / take, multi-source (cross join), group by, count / sum / min / max / avg aggregates, distinct.

## Lowering decisions

Mochi's query DSL is desugared by `clower` (the C-target lower pass shared across backends) into an `aotir.QueryExpr` with explicit `From`, `Where`, `Select`, `OrderBy`, `Skip`, `Take`, and `Aggregate` fields. The Rust lower pass renders this as a Rust iterator chain:

```rust
let result: Vec<_> = users.iter()
    .filter(|u| u.age >= 18)
    .map(|u| u.name.clone())
    .collect();
```

`order by k desc` lowers to `.sorted_by_key(|x| std::cmp::Reverse(k(x)))` via the `itertools` crate. The choice to use `itertools` is a compromise: stable Rust has no `Iterator::sorted` (it requires `Vec` collection first). itertools is widely deployed and adds a single thin dep.

Cross-join (multi-source from) is materialised into a Vec then iterated:

```rust
let rows: Vec<_> = users.iter().flat_map(|u| {
    orders.iter().map(move |o| (u.clone(), o.clone()))
}).collect();
```

Group by lowers to `BTreeMap<K, Vec<V>>` (the BTreeMap gives deterministic iteration order matching vm3's sorted output).

Aggregates (`count`, `sum`, `min`, `max`, `avg`) are emitted inline rather than via `.fold`: `.count() as i64`, `.sum::<i64>()`, `.min().unwrap_or(0)`, `.max().unwrap_or(0)`, `let total: f64 = ...sum(); total / (n as f64)`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/query.go` | Query DSL → iterator chain |
| `runtime3/rust/mochi-runtime/Cargo.toml` | Add `itertools` dep |
| `transpiler3/rust/build/phase07_test.go` | 43-fixture gate |
| `tests/transpiler3/rust/fixtures/phase07-query/*.mochi` + `.out` | 43 fixtures |

## Test set

- `TestPhase7Query/<fixture>` for each `.mochi` in the fixture directory (43 fixtures).

## Closeout notes

Group-by output order was the longest-running bug in phase 7. HashMap's unspecified iteration order produces nondeterministic stdout; switching the group accumulator to BTreeMap fixed every fixture at once. The cost is a `log n` group-insert vs O(1) for HashMap, which is acceptable for the fixture set (largest group has 100 elements) and matches vm3's behavior.
