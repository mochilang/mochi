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

## Remaining work for JOB queries

The JOB dataset queries up to `q10` compile but running the generated Dart code
fails because dataset rows are translated to dynamic maps. The compiler needs to
generate typed structs for the JOB schemas and adjust field access accordingly.
Once struct generation works the tests in `job_test.go` should execute without
skipping for `q1`–`q10`.
