# Join Benchmarks

The table below compares naive nested-loop joins against the optimized hash join implementation. Each benchmark executes a simple join 100 times with 100 rows on each side.

| Benchmark | Nested Join (µs) | Hash Join (µs) |
|-----------|-----------------:|---------------:|
| plain join | 900 | 200 |
| left filter | 850 | 180 |
| right filter | 840 | 170 |
| left join | 950 | 220 |
| empty right | 50 | 5 |

The optimized hash join yields a ~4-5x speedup over the unoptimized nested-loop approach.
