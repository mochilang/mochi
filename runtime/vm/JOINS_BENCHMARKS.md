# Join Benchmarks

Benchmarks are implemented in [join_bench_test.go](./join_bench_test.go). Each test constructs two lists of 100 rows with an integer `id` field and executes the join algorithm.

| Benchmark    | Nested Join (µs) | Hash Join (µs) | Hash+Prealloc (µs) | Merge Join (µs) |
|--------------|-----------------:|---------------:|-------------------:|---------------:|
| plain join   | 193              | 50             | 33                 | 36             |

The preallocated hash join reduces allocations compared to the basic hash join.
Merge join further optimizes joins when both inputs are pre-sorted by the join key.
