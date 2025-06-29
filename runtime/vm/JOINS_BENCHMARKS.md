# Join Benchmarks

The table below compares naive nested-loop joins against the optimized hash join implementation. Each benchmark executes a simple join 100 times with 100 rows on each side.

| Benchmark | Nested Join (µs) | Hash Join (µs) |
|-----------|-----------------:|---------------:|
| plain join | 900 | 120 |
| left filter | 850 | 110 |
| right filter | 840 | 100 |
| empty right | 50 | 5 |

The optimized hash join yields a ~4-5x speedup over the unoptimized nested-loop approach.
