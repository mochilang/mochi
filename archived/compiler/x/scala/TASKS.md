# Scala Backend Tasks

Scala code generation now supports running the `tpc-h/q1.mochi` benchmark and JOB queries.
`tpc-h/q2.mochi` compiles but the generated Scala fails to run due to missing
runtime helpers and variable scoping issues.

Completed work:

- Dataset queries compile to helper calls that perform filtering and grouping, emitting Scala collections.
- Aggregates such as `sum`, `avg` and `count` are handled when operating over a group.
- Results are serialised to JSON via a minimal runtime helper.
- Added golden test `TestScalaCompiler_TPCHQ1` checking generated code and runtime output.
- Generated code lives under `tests/dataset/tpc-h/compiler/scala/q1.scala.out`.
- Added golden test `TestScalaCompiler_JOBQ1` covering the JOB dataset query.
- Added golden test `TestScalaCompiler_JOBQ2` covering the JOB dataset query.
- Added golden test `TestScalaCompiler_JOB_Golden` compiling JOB queries `q1` through `q10`.
- Added golden test `TestScalaCompiler_TPCDSQ1` covering the first TPC-DS query.

Remaining work:

- Fix runtime helpers (`_compare`, aggregates) and scope handling so TPCH queries beyond `q1` run.
- Scala output for JOB queries beyond `q2` fails to compile due to map field access using dot notation.
- Implement map/struct field access so JOB queries `q3`-`q10` run successfully.
