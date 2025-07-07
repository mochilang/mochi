# Zig Backend Tasks for TPCH Queries

Grouping support for simple queries has been implemented so that
`tests/dataset/tpc-h/q1.mochi` compiles. Aggregation helpers (`sum`, `avg`,
`count`) are available and JSON output is handled via `std.json`. Grouping uses
a `std.AutoHashMap` for constant time lookups.

Query `q2` currently generates Zig code but the resulting program fails to
compile due to undeclared identifiers and duplicate labels inside generated
query loops.

Remaining work
---------------

- Address join and sorting support for query expressions.
- Fix variable scoping and label generation so that TPCH `q2` builds and runs.
- JOB dataset queries `q1`–`q10` still fail to build. The boolean precedence
  issue has been partially addressed but the generated Zig programs do not
  compile due to missing variable declarations inside query loops. Investigate
  the query code generation and ensure all temporary identifiers are declared
  before use so the tests in `job_run_test.go` succeed.
- The TPCDS suite is not yet implemented. Attempting to compile `q1` results
  in an "unsupported query features" error because grouping with joins is
  unhandled. Full support for groups combined with join expressions is required
  before the Zig backend can run the TPCDS queries.
