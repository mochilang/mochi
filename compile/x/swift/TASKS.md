# Swift Backend Tasks for TPCH Q1

The Swift backend can now compile and run the `tpc-h/q1.mochi` benchmark.

Completed work:

- `group by` implemented using a helper `_group_by` backed by a dictionary.
- Added numeric helpers `_sum` and `_avg` for arrays.
- Struct values map to `Codable` Swift `struct` types for JSON output.
- Added golden test `tpch_q1.mochi` under `tests/compiler/swift`.

Further improvements will expand coverage of dataset queries.
