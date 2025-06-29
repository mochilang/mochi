# Join benchmarks

The following results were obtained on an Intel Xeon Platinum 8370C using
`go test ./runtime/vm -bench Join -count=1`.

| Benchmark        | Time (s) |
|------------------|---------:|
| `BenchmarkJoinEqual`   | 1.28 |
| `BenchmarkJoinAddSub`  | 1.31 |
| `BenchmarkJoinShift`   | 13.80 |

`BenchmarkJoinEqual` performs a simple equality join and uses the hash based
implementation. `BenchmarkJoinAddSub` includes a `+1 - 1` in the join condition
which is recognized as an equality and is similarly fast. `BenchmarkJoinShift`
adds a constant, preventing the optimization and falling back to the slower
nested-loop join.
