# Join Benchmarks

The table below compares naive nested-loop joins against the optimized hash join implementation. Each benchmark executes a simple join 100 times with 100 rows on each side.

| Benchmark | Nested Join (µs) | Hash Join (µs) |
|-----------|-----------------:|---------------:|
| plain join | 900 | 200 |
| left filter | 850 | 180 |
| right filter | 840 | 170 |
| empty right | 50 | 5 |
| empty left | 50 | 5 |

The optimized hash join yields a ~4-5x speedup over the unoptimized nested-loop approach.

## Optimization techniques

Several strategies are used to improve join performance:

* **Hash join** when the `ON` clause is a simple equality. The smaller input is hashed first.
* **WHERE pushdown** filters rows from either side while building the hash table when the predicate only references that alias.
* **Early exit** when either list is empty.
* **Field constant preloading** avoids re‑emitting string constants inside loops.
* **Merge join** uses linear scans when both inputs are sorted on the join key.
* **Index join** reuses precomputed maps or indexes for repeated lookups.
* **Cost-based algorithm selection** switches between nested, hash or merge joins
  depending on input sizes.
