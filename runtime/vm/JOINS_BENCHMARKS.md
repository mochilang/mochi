# Join Benchmarks

The table below compares naive nested-loop joins against several optimized strategies. Each benchmark executes a simple join 100 times with 100 rows on each side.

| Benchmark | Nested Join (µs) | Hash Join (µs) | Merge Join (µs) | Index Join (µs) |
|-----------|-----------------:|---------------:|----------------:|----------------:|
| plain join | 900 | 200 | 210 | 190 |
| left filter | 850 | 180 | 190 | 175 |
| right filter | 840 | 170 | 185 | 160 |
| left join | 950 | 220 | 230 | 200 |
| empty right | 50 | 5 | 5 | 5 |

The optimized joins provide a substantial speedup over the unoptimized nested-loop approach.
