# Zig Backend Tasks for TPCH Q1

Grouping support for simple queries has been implemented so that
`tests/dataset/tpc-h/q1.mochi` compiles successfully and runs. Aggregation
helpers (`sum`, `avg`, `count`) are available and JSON output is handled via
`std.json`. Grouping now uses a `std.AutoHashMap` for constant time lookups
instead of a linear search.

Remaining work
---------------

- Add join and sorting support for query expressions.
- JOB dataset queries `q1`-`q10` still fail to build. The boolean precedence issue
  has been partially addressed but the generated Zig programs do not compile due
  to missing variable declarations inside query loops. Investigate the query code
  generation and ensure all temporary identifiers are declared before use so the
  tests in `job_run_test.go` succeed.
