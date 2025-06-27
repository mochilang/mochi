# Pascal Backend Tasks for TPCH Q1

The Pascal backend now supports executing the first TPCH query. Recent work
added record types for dataset rows, dynamic arrays for query results and
helper functions for aggregations. Groups are implemented using `TFPGMap` and
a small JSON printer is included for test output.

Future improvements could extend coverage to the remaining TPCH queries and
expand runtime error handling.

## JOB dataset

Initial support for the JOB benchmark was added. The compiler can translate
`tests/dataset/job/q1.mochi` and `q2.mochi` and the generated source is stored
under `tests/dataset/job/compiler/pas`. However the resulting Pascal code does
not yet compile with `fpc` due to incomplete join handling and unstable
temporary variables.

Remaining tasks:

* Fix join code generation so JOB queries build successfully.
* Stabilise temporary variable names to avoid missing identifiers.
* Enable running the JOB Q1 and Q2 programs as part of the golden tests.
