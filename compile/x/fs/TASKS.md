# F# Backend Tasks for TPCH Q1

The F# compiler focuses on algorithmic examples and lacks dataset grouping support.

- Lower queries to `Seq.groupBy` followed by `Seq.map` for aggregation.
- Map Mochi records to F# record types with typed fields.
- Implement inline functions for `sum`, `avg` and `count` over sequences.
- Serialize results with `System.Text.Json` and add tests under `tests/compiler/fs`.
- JOB queries q1–q10 currently fail to compile because field name constants like
  `movie_id` and `person_id` are not emitted. The generated code uses bare
  identifiers which F# treats as variables. Add helper constants for record
  fields so the maps are built correctly and ensure the queries pass when run
  with `dotnet fsi`.
