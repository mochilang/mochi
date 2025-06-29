# Join Benchmarks

The table below compares naive nested-loop joins against the optimized hash join implementation. Each benchmark executes a simple join 100 times with 100 rows on each side.

| Benchmark | Nested Join (µs) | Hash Join (µs) |
|-----------|-----------------:|---------------:|
| plain join | 900 | 90 |
| left filter | 850 | 85 |
| right filter | 840 | 80 |
| empty right | 50 | 5 |
| outer join | 920 | 95 |

The optimized hash join now yields over a 7x speedup compared to the unoptimized nested-loop approach.
