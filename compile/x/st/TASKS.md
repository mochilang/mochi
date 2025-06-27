# Smalltalk Backend Tasks for TPCH Q1

The Smalltalk backend now supports dataset grouping used by TPCH Q1.
Groups are accumulated via a `_Group` class backed by `Dictionary`
objects and runtime helpers provide `sum`, `avg` and `count`.
Query results can be printed as JSON via the `json` built-in and
golden tests cover `tpch_q1` and simple `group_by` queries.

## Pending JOB Queries

The compiler still fails to execute JOB queries q1-q10 under GNU Smalltalk.
Generated code raises "undefined variable" errors when run with `gst`.
Future work should fix code generation for global result values and
verify runtime output matches the VM implementation. Golden files for
q1-q10 have been generated but tests remain skipped until execution
succeeds.
