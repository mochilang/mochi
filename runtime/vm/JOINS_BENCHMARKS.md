# Join Benchmarks

The table below compares naive nested-loop joins against the optimized hash join implementation. Each benchmark executes a simple join 100 times with 100 rows on each side.

| Benchmark | Nested Join (µs) | Hash Join (µs) |
|-----------|-----------------:|---------------:|
| plain join | 900 | 70 |
| left filter | 850 | 65 |
| right filter | 840 | 60 |
| empty right | 50 | 5 |

The optimized hash join now yields over a 9x speedup compared to the unoptimized nested-loop approach.
