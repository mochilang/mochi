# Join Benchmarks

Benchmarks are implemented in [join_bench_test.go](./join_bench_test.go) using the algorithms defined in [join_algos.go](./join_algos.go). Each test constructs two lists of 100 rows with an integer `id` field and executes the join algorithm.

| Benchmark    | Nested Join (µs) | Hash Join (µs) | Hash+Prealloc (µs) | Merge Join (µs) | Index Join (µs) |
|--------------|-----------------:|---------------:|-------------------:|---------------:|---------------:|
| plain join   | 306              | 64             | 59                 | 24             | 36             |

The preallocated hash join reduces allocations compared to the basic hash join.
Merge join further optimizes joins when both inputs are pre-sorted by the join key. An index join performs binary search over a sorted table to avoid scanning the entire input.
