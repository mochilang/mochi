---
title: "08. Dataset pipeline"
sidebar_position: 9
sidebar_label: "08. Dataset"
description: "Query DSL via Iterator + itertools, BTreeMap for deterministic group-by, compile-time Datalog (semi-naive fixpoint) emitted as frozen Vec literals."
---

# 08. Dataset pipeline

This note covers how MEP-53 lowers Mochi's query DSL and Datalog rules to Rust iterator chains and compile-time constants.

## Query DSL: from / where / select / order / skip / take

Mochi's query DSL is desugared by the shared `clower` pass (used by C, BEAM, JVM, .NET, Swift, Kotlin, Python, TypeScript, Ruby, and Rust targets) into an `aotir.QueryExpr` with explicit `From`, `Where`, `Select`, `OrderBy`, `Skip`, `Take`, and `Aggregate` fields. The Rust lower pass renders this as a Rust iterator chain:

```mochi
from u in users
where u.age >= 18
select u.name
```

```rust
let result: Vec<String> = users.iter()
    .filter(|u| u.age >= 18)
    .map(|u| u.name.clone())
    .collect();
```

The `.clone()` on `u.name` is because `.map`'s closure takes `&User` by reference, and we want an owned String out. The colour pass can sometimes elide this when the consumer doesn't need ownership; for `collect::<Vec<String>>()`, ownership is required.

## Sort: order by

`order by k` lowers to `.sorted_by_key(|x| k(x))` from itertools (stable Rust has no `Iterator::sorted`; users either roll their own `Vec::sort_by_key` after collect, or pull in itertools).

`order by k desc` lowers to `.sorted_by_key(|x| std::cmp::Reverse(k(x)))`. The `Reverse` newtype is stdlib (no extra dep).

For non-totally-ordered keys (e.g., f64 in the absence of NaN), `sorted_by` is used with an explicit comparator. The typecheck pass rejects f64 keys at lower time when a query needs ordering, to avoid the silent partial-order surprise.

## Skip / take

`skip s take t` lowers to `.skip(s).take(t)`. Both are stdlib Iterator adapters.

## Group by

Group by lowers to a `BTreeMap<K, Vec<V>>` accumulator built by a manual loop:

```rust
let mut groups: std::collections::BTreeMap<K, Vec<V>> = std::collections::BTreeMap::new();
for row in rows {
    groups.entry(key(&row)).or_insert_with(Vec::new).push(row);
}
```

BTreeMap gives sorted iteration order; HashMap would give unspecified order which would break the byte-equal stdout gate against vm3 (which uses Go map + slice for insertion order, which doesn't match either Rust hash or sort order — but for query-by-derived-key cases, sort order matches insertion order by construction in the fixture set).

## Aggregates

Aggregates are emitted inline rather than via `.fold`:

| Mochi | Rust |
|-------|------|
| `count(xs)` | `xs.len() as i64` (when xs is a Vec) or `xs.count() as i64` (when xs is an Iterator) |
| `sum(xs)` | `xs.sum::<i64>()` or `xs.sum::<f64>()` |
| `min(xs)` | `xs.iter().min().cloned().unwrap_or(0)` |
| `max(xs)` | `xs.iter().max().cloned().unwrap_or(0)` |
| `avg(xs)` | `xs.iter().sum::<i64>() as f64 / xs.len() as f64` |
| `distinct(xs)` | `xs.into_iter().collect::<std::collections::BTreeSet<_>>().into_iter().collect::<Vec<_>>()` |

`distinct` uses BTreeSet for deterministic iteration. HashSet would be wrong.

## Cross joins (multi-source from)

```mochi
from u in users, o in orders where u.id == o.user_id
```

```rust
let rows: Vec<_> = users.iter().flat_map(|u| {
    orders.iter().map(move |o| (u.clone(), o.clone()))
}).filter(|(u, o)| u.id == o.user_id).collect();
```

The clones are mandatory because flat_map's closure body must produce owned values (the iterator returns owned tuples). The colour pass cannot elide these.

## Datalog: compile-time semi-naive fixpoint

Mochi Datalog (`rule`, `query`) is evaluated **at compile time** in the Rust lower pass via semi-naive fixpoint. `transpiler3/rust/lower/datalog.go` walks the rule set, computes the least fixed point of the EDB, and emits the result tuples as a frozen Rust `Vec` literal:

```mochi
rule ancestor(X, Y) :- parent(X, Y).
rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
query ancestor(_, Y).
```

```rust
let ancestors: Vec<(String, String)> = vec![
    ("alice".to_string(), "bob".to_string()),
    ("bob".to_string(), "carol".to_string()),
    ("alice".to_string(), "carol".to_string()),
];
```

The trade-off: compile-time evaluation means the rule body and EDB facts must be statically known at lower time. Mochi forbids runtime EDB updates (no `assert` / `retract` statements at the source level), so this restriction is already enforced. The benefit: zero runtime cost (the query result is a baked-in constant), no Datalog runtime in the emitted code, no Datalog crate dep.

Semi-naive evaluation reuses the same `aotir.DatalogProgram` representation as the C target (MEP-45 phase 11); only the result emission differs (C emits a static array initializer; Rust emits a `vec!` macro call).

For very large rule sets (thousands of facts), compile-time evaluation becomes slow. The fixture set in phase 8 caps facts at ~50 per program, which lowers in under 10ms. A future sub-phase could add a runtime-evaluated path for cases where the rule body depends on runtime input.

## Cross-references

- [[language-surface]] for the surface syntax.
- [[type-lowering]] for BTreeMap vs HashMap.
- [[runtime]] for the itertools dep rationale.
- [MEP-53 §3](/docs/mep/mep-0053#3-surface-syntax-lowering) for the normative query and Datalog lowerings.
