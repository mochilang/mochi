# Pascal Backend Tasks for TPCH Queries

The Pascal backend now supports executing the first TPCH query. Recent work
added record types for dataset rows, dynamic arrays for query results and
helper functions for aggregations. Groups are implemented using `TFPGMap` and
a small JSON printer is included for test output.

Work is ongoing to extend coverage to further TPCH queries and to improve
runtime error handling. Query `q2` currently fails to compile due to missing
support for complex record field accesses.

## JOB dataset

The JOB benchmark programs up to `q10.mochi` now compile and the generated
Pascal sources are stored under `tests/dataset/job/compiler/pas`. Runtime
execution still depends on the Free Pascal Compiler being available at test
time. If `fpc` is missing the golden test skips execution.

Remaining tasks:

* Verify all JOB queries beyond `q10` compile and run once `fpc` is
  available.
* Improve error handling during dataset joins.

## TPCDS dataset

The Pascal backend now compiles the first TPCDS query `q1`. Generated
Pascal code and expected output are stored under
`tests/dataset/tpc-ds/compiler/pas`.
