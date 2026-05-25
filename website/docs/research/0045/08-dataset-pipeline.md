---
name: 5600-research-08-dataset-pipeline
description: "MEP-45 research note 08. Lowering the Mochi query DSL (LINQ-style from/where/select/join/group by/order/limit/union/intersect/except) to C, with arena allocation, pipeline fusion, and load/save adapters."
---

# MEP-45 research note 08, Dataset pipeline lowering

Author: research pass for MEP-45.
Date: 2026-05-22 (GMT+7).

Mochi's headline feature is the query DSL on collections. The same
syntax operates on lists, maps with insertion-ordered iteration, and
streams of records loaded from JSON, YAML, CSV, JSONL, or a `load`
adapter. This note covers the lowering strategy.

## 1. Language surface recap

From the language docs and `examples/load_with.mochi`:

```mochi
let people = load "people.json"

let adults = from p in people
             where p.age >= 18
             select p

let by_city = from p in people
              group by p.city into g
              select { city: g.key, count: count(g) }

let joined = from a in users
             join b in orders on a.id == b.user_id
             where b.total > 100
             select { user: a.name, total: b.total }
```

Surface clauses supported (per the language surface note):

- `from x in coll [join y in coll2 on x.k == y.k] ... [where pred]
  [group by key into g] [order by ... asc|desc] [limit n] [offset n]
  select expr` plus `union`, `intersect`, `except` between two
  queries.

## 2. Pipeline IR

We lower each query to a directed acyclic graph of operators (MIR
pass between type-check and codegen):

```
PipelineOp =
  | Source(coll)
  | Where(pred)
  | Select(expr)
  | Join(other_coll, lhs_key, rhs_key, kind)         // inner, left, full
  | GroupBy(key_expr)
  | Aggregate(name, fn)                              // count, sum, ...
  | Order(key_expr, direction)
  | Distinct
  | Union(other_pipeline)
  | Intersect(other_pipeline)
  | Except(other_pipeline)
  | Limit(n)
  | Offset(n)
  | Sink                                             // terminal
```

Each op records:
- Its input record type and output record type (monomorphised).
- Whether it is *blocking* (must read its input fully before producing
  output) or *streaming*.
- Whether it preserves source order.

`Where` and `Select` are streaming. `Join`, `GroupBy`, `Order`,
`Distinct`, `Union`, `Intersect`, `Except` are blocking. `Limit` is
streaming with early-exit. `Offset` is streaming with a skip counter.

## 3. Operator fusion

Adjacent streaming ops fuse into a single emitted loop body:

```mochi
from p in people
where p.age >= 18
where p.city == "Hanoi"
select { name: p.name, age: p.age }
```

Becomes one C loop:

```c
mochi_list__People in_ = people;
mochi_list__Person2 out_ = mochi_list__Person2_new();
for (size_t i_ = 0; i_ < in_.len; ++i_) {
    struct pkg_People p = in_.data[i_];
    if (!(p.age >= 18)) continue;
    if (!(p.city.len == 5 && memcmp(p.city.bytes, "Hanoi", 5) == 0))
        continue;
    struct pkg_Person2 o;
    o.name = p.name;
    o.age = p.age;
    mochi_list__Person2_push(&out_, o);
}
```

Predicates and projections at the same fusion boundary share a single
iteration variable.

## 4. Joins

### 4.1 Inner equi-join

The default. Lowering: build a hash table on the smaller side, scan
the larger:

```c
mochi_omap__i64_User idx = mochi_omap__i64_User_new();
for (size_t i = 0; i < users.len; ++i)
    mochi_omap__i64_User_set(&idx, users.data[i].id, users.data[i]);

mochi_list__Out out_ = mochi_list__Out_new();
for (size_t j = 0; j < orders.len; ++j) {
    struct pkg_Order ord = orders.data[j];
    struct pkg_User u;
    if (mochi_omap__i64_User_try_get(idx, ord.user_id, &u)) {
        if (!(ord.total > 100)) continue;
        struct pkg_Out o = { .user = u.name, .total = ord.total };
        mochi_list__Out_push(&out_, o);
    }
}
```

The query planner picks the smaller side as the build side by reading
either a size annotation or, by default, treating the right side of
`join` as the probe side.

### 4.2 Left join

Same shape as inner, but on miss we emit a row with the right-side
fields nulled (the records get `?T` for each right-side field, or the
language disallows left join on non-optional fields).

### 4.3 Cross join

`from a in xs from b in ys` is the explicit cross-product form. Nested
loops, no fusion across the boundary.

## 5. Group by

Lowering uses an insertion-ordered map keyed on the group key:

```c
mochi_omap__str_Group g_ = mochi_omap__str_Group_new();
for (size_t i = 0; i < in_.len; ++i) {
    struct pkg_P p = in_.data[i];
    mochi_str k = p.city;
    struct pkg_Group cur;
    if (!mochi_omap__str_Group_try_get(g_, k, &cur)) {
        cur = (struct pkg_Group){ .key = k, .items = mochi_list__P_new() };
    }
    mochi_list__P_push(&cur.items, p);
    mochi_omap__str_Group_set(&g_, k, cur);
}
```

Aggregates that don't need the full group materialised (`count`, `sum`,
`min`, `max`, `avg`) get a *streaming aggregator* state in the map value
instead of a full item list. The planner picks the streaming form when
the select clause only uses streaming-compatible aggregates.

## 6. Order by

Lowered to a heap sort or merge sort over a freshly materialised
buffer:

```c
mochi_list__T sorted_ = mochi_list__T_copy(in_);
mochi_sort__T_by(&sorted_, cmp_fn);
```

The comparator `cmp_fn` is generated from the order key expression. For
multi-key order (`order by a, b desc`) the comparator is a lex chain.

Stable sort is the default (the language docs imply insertion-order
preservation on ties for queries on insertion-ordered maps).

## 7. Distinct, union, intersect, except

All use `mochi_set__T` under the hood:

- `distinct`: insert each item into a set, emit on first occurrence.
- `union`: walk left then right, skipping items already seen.
- `intersect`: build a set from one side, filter the other.
- `except`: build a set from the right, filter the left.

For two-sided ops we apply the smaller-build optimisation.

## 8. Limit and offset

Streaming with counters:

```c
size_t skipped_ = 0, taken_ = 0;
for (...) {
    if (skipped_ < offset_) { ++skipped_; continue; }
    if (taken_ >= limit_) break;
    /* fused body */
    ++taken_;
}
```

## 9. Arena allocation

Each top-level query expression runs inside a `mochi_arena`. All
intermediate lists, maps, and sets allocate from the arena. The arena
is freed at the query boundary. The result list (the one that escapes)
is copied out into a GC-managed list before the arena drops.

```c
{
    mochi_arena a_;
    mochi_arena_init(&a_);
    /* ... pipeline with allocations through &a_ ... */
    mochi_list__Out out_ = mochi_list__Out_copy_to_gc(arena_out_);
    mochi_arena_destroy(&a_);
}
```

This holds GC traffic *out* of hot query loops. The query planner is
the only consumer of arenas in v1.

## 10. Load adapters

The language supports `load PATH` with format inferred from extension,
or `load PATH format=JSON|YAML|CSV|JSONL`. Lowering:

```c
mochi_list__T xs;
mochi_error err = mochi_load__T(MOCHI_FMT_JSON, "people.json", &xs);
if (err.code) mochi_throw(err);
```

`mochi_load__T` is generated per record type and dispatches to the
appropriate parser (yyjson for JSON, libfyaml for YAML, home-grown CSV).
Field name and type mapping is generated from the record's MIR type.

For `load PATH with { schema: T, ... }` blocks, the options struct is
generated from the option keys.

`save xs to PATH [format=...]` is the dual.

## 11. Stream sources

When the source is a `stream<T>` (note 09), the planner cannot use
blocking operators without buffering. We emit a buffering wrapper:

```c
mochi_list__T buf_ = mochi_stream__T_drain_into_list(stream);
/* ... blocking pipeline on buf_ ... */
```

For purely streaming pipelines (`from x in s where ... select ...`)
the lowering uses the stream's pull API directly.

## 12. SIMD and parallelism

Phase 2:

- For pipelines over `list<int>`, `list<float>`, `list<bool>` with
  simple predicates and projections, emit a SIMD-vectorised body using
  `<stdint.h>` widening loads and either `<immintrin.h>` /
  `<arm_neon.h>` intrinsics or compiler auto-vectorisation hints.
- For pipelines where the work per element exceeds a threshold and the
  collection size is large, emit a work-stealing parallel for using
  the M:N scheduler's worker pool.

Both are off in v1.

## 13. Constant folding

A query whose source is a compile-time-constant literal collection and
whose predicates and projections are constant-foldable is fully
evaluated at compile time and emitted as a `static const` array. The
codegen tracks this as a folded-pipeline annotation in MIR.

## 14. Spilling

When a blocking operator's working set exceeds a configurable
threshold (`MOCHI_QUERY_SPILL=64MiB` default), the runtime falls back
to an external merge sort or external hash join. The disk format is
the same wire format as `save format=binary` so spill files can be
debugged.

This is Phase 3.

## 15. The order-of-evaluation contract

The language docs are explicit: queries are eagerly evaluated unless
the result is bound to a `stream<T>`. We do not implement lazy
LINQ-style enumerators. The result is always a materialised
collection.

Side effects in the select clause (e.g. calling `print`) execute in
source-order over the pipeline. This is a normative rule the codegen
must respect: no parallel evaluation may surface side effects out of
order.

## 16. Open questions

1. Whether `group by ... into g` exposes `g` as a full materialised
   list always, or only when the select clause demands it.
2. Whether two-sided ops (union/intersect/except) preserve insertion
   order (they should, but the docs are ambiguous).
3. Whether `order by` defaults to ascending (yes per common LINQ).
4. Whether `limit -n` (negative limits) is allowed (probably error).
5. Whether `from x in m` over a `map<K, V>` yields `{ key, value }`
   pairs or just values. The docs imply pairs.
