# F# Backend Tasks for TPCH Q1

The F# compiler focuses on algorithmic examples and lacks dataset grouping support.

- Lower queries to `Seq.groupBy` followed by `Seq.map` for aggregation.
- Map Mochi records to F# record types with typed fields.
- Implement inline functions for `sum`, `avg` and `count` over sequences.
- Serialize results with `System.Text.Json` and add tests under `tests/compiler/fs`.
- JOB queries q1–q10 now compile by emitting field name constants before the
  generated program. Remaining work is focused on dataset grouping and
  serialization.

## Outstanding Issues

- TPCH q2 compiles but running the generated F# fails. Array literals need
  their closing `|]` indented and map values should use `box` to unify types.
