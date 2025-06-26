# F# Backend Tasks for TPCH Q1

The F# compiler focuses on algorithmic examples and lacks dataset grouping support.

- Lower queries to `Seq.groupBy` followed by `Seq.map` for aggregation.
- Map Mochi records to F# record types with typed fields.
- Implement inline functions for `sum`, `avg` and `count` over sequences.
- Serialize results with `System.Text.Json` and add tests under `tests/compiler/fs`.
