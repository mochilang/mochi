# Swift Backend Tasks for TPCH Q1

The Swift backend can compile and run the `tpc-h/q1.mochi` benchmark.

Completed work:

- `group by` implemented using a helper `_group_by` backed by a dictionary.
- Added numeric helpers `_sum` and `_avg` for arrays.
- Struct values map to `Codable` Swift `struct` types for JSON output.
- Added golden test `tpch_q1.mochi` under `tests/compiler/swift`.
- Added dataset golden test `TestSwiftCompiler_TPCHQ1_Golden` and stored generated code under `tests/dataset/tpc-h/compiler/swift/q1.swift.out`.
- `_group_by` now hashes keys using JSON for stable grouping.

Further improvements will expand coverage of dataset queries.

- Added JOB dataset golden tests for queries `q1` to `q10` under `tests/compile/x/swift`.
- Generated Swift sources stored as `tests/dataset/job/compiler/swift/q*.swift.out`.
- Running the generated Swift with `swiftc` still fails due to dictionary based
  record handling; future work should emit proper struct types so the programs
  compile.
