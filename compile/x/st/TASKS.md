# Smalltalk Backend Tasks for TPCH Q1

The Smalltalk backend now supports dataset grouping used by TPCH Q1.
Groups are accumulated via a `_Group` class backed by `Dictionary`
objects and runtime helpers provide `sum`, `avg` and `count`.
Query results can be printed as JSON via the `json` built-in and
golden tests cover `tpch_q1` and simple `group_by` queries.
