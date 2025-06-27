# Dart Backend Tasks for TPCH Q1

The Dart compiler now runs `tests/dataset/tpc-h/q1.mochi`. Grouping is handled
using `_Group` instances stored in a `Map<String,_Group>` while aggregates are
computed via helper functions. Struct definitions become typed Dart classes with
`fromJson` constructors and results are printed using `jsonEncode`.

Completed tasks:

- [x] Lower `group by` queries to loops building the groups map.
- [x] Add `_Group.count`, `_Group.sum` and `_Group.avg` methods.
- [x] Generate typed structs for dataset rows and register parsers.
 - [x] Add golden test `tpch_q1_test.go` and sample generated code.

## Remaining Work for JOB Queries

`compile/x/dart/job_test.go` now attempts to compile the first ten JOB queries
(`q1`–`q10`). The generated Dart sources match the goldens but fail to run
because map fields are accessed via `.` instead of `[...]` or typed structs.

To execute the JOB programs the backend needs to:

- Map record literals to generated Dart classes or provide dynamic field access
  helpers.
- Emit valid test code so the JOB golden tests no longer skip when invoking the
  Dart VM.
