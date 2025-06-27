# Scala Backend Tasks for TPCH Q1

Scala code generation now supports running the `tpc-h/q1.mochi` benchmark.

Completed work:

- Dataset queries compile to helper calls that perform filtering and grouping, emitting Scala collections.
- Aggregates such as `sum`, `avg` and `count` are handled when operating over a group.
- Results are serialised to JSON via a minimal runtime helper.
- Added golden test `TestScalaCompiler_TPCHQ1` checking generated code and runtime output.
- Generated code lives under `tests/dataset/tpc-h/compiler/scala/q1.scala.out`.
- Added golden test `TestScalaCompiler_JOBQ1` covering the JOB dataset query.
