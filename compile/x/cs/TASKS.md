# C# Backend Tasks for TPCH Q1

The C# backend currently emits straightforward loops but lacks LINQ-style grouping
and structured record handling.

- Extend query lowering to use `Enumerable.GroupBy` with subsequent aggregation.
- Generate classes for records such as `lineitem` and support field projection.
- Add helpers for `sum`, `avg` and `count` implemented with `System.Linq`.
- Serialize query results using `System.Text.Json`.
- Include a golden test in `tests/compiler/cs` once the query compiles.
