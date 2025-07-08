# Go Compiler JOB Queries

The JOB dataset queries now build using the Go backend, but several
still fail their embedded tests. Generation results:

- `q1`, `q2` and `q6` compile and pass tests.
- `q3`, `q4`, `q5`, `q8`, `q9` and `q10` compile, but the generated
  programs report failed expectations.
- `q7` does not emit a Go file when built.

Golden outputs have been updated for the queries that compile. Further
work is required to make the failing programs pass their tests and to
determine why `q7` is skipped during compilation.

TPC-H progress:

- `q1` and `q2` now compile and execute successfully with the Go backend.
