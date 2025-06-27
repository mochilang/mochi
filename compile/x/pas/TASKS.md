# Pascal Backend Tasks for TPCH Q1

The Pascal backend now supports executing the first TPCH query. Recent work
added record types for dataset rows, dynamic arrays for query results and
helper functions for aggregations. Groups are implemented using `TFPGMap` and
a small JSON printer is included for test output.

Future improvements could extend coverage to the remaining TPCH queries and
expand runtime error handling.

## JOB dataset

The JOB benchmark programs up to `q10.mochi` now compile and the generated
Pascal sources are stored under `tests/dataset/job/compiler/pas`. Runtime
execution still depends on the Free Pascal Compiler being available at test
time. If `fpc` is missing the golden test skips execution.

Remaining tasks:

* Verify all JOB queries beyond `q10` compile and run once `fpc` is
  available.
* Improve error handling during dataset joins.
