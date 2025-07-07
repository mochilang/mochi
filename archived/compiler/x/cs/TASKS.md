# C# Backend Tasks for TPCH Q1

The C# backend currently emits straightforward loops but lacks LINQ-style grouping
and structured record handling.

- Extend query lowering to use `Enumerable.GroupBy` with subsequent aggregation.
- Generate classes for records such as `lineitem` and support field projection.
- Add helpers for `sum`, `avg` and `count` implemented with `System.Linq`.
- Serialize query results using `System.Text.Json`.
- Include a golden test in `tests/compiler/cs` once the query compiles.

## JOB Queries

Currently only `q1.mochi` and `q2.mochi` compile successfully. Queries `q3.mochi`
through `q10.mochi` fail due to missing record generation and dictionary field
projection. Implement LINQ-style joins and proper struct handling so these
queries can be built and executed.

## TPCH Queries

`q1.mochi` compiles and runs when the `dotnet` SDK is available. `q2.mochi`
also compiles, but we could not run the generated code in this environment
because `dotnet` is missing. When `dotnet` is installed, capture the output of
running the compiled program and add it as `tests/dataset/tpc-h/compiler/cs/q2.out`.

## TPCDS Queries

The TPCDS queries `q1.mochi` through `q99.mochi` now compile and run
successfully. Generated C# code and runtime output for each query are stored
under `tests/dataset/tpc-ds/compiler/cs`.
