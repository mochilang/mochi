# Smalltalk Backend Tasks for TPCH Q1-2

The Smalltalk backend now supports dataset grouping used by TPCH Q1.
Groups are accumulated via a `_Group` class backed by `Dictionary`
objects and runtime helpers provide `sum`, `avg` and `count`.
Query results can be printed as JSON via the `json` built-in and
golden tests cover `tpch_q1` and `tpch_q2` as well as simple `group_by` queries.
Running the TPCH programs under `gst` still reports parse errors,
so executing the compiled Smalltalk code remains TODO.

The TPCDS suite now covers queries `q1` through `q99`. The Smalltalk backend
successfully compiles these programs and golden output is stored under
`tests/dataset/tpc-ds/compiler/st`.

## Pending JOB Queries

Initial work towards running the JOB dataset defined global variables
before class creation and quoted map keys. The programs now compile,
but `gst` still reports parse errors around query expressions. Further
debugging of the generated Smalltalk syntax is required before the
queries execute successfully. Golden files for `q1`–`q10` exist and a
new `JOB_Golden` test validates the emitted code.
