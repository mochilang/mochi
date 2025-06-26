# MLIR Backend Tasks for TPCH Q1

This backend reuses the C compiler and converts the result to MLIR. Grouping logic is not yet translated.

- Lower dataset loops and aggregations to structured control flow operations in MLIR.
- Map Mochi structs to `memref` or `struct` types and generate accessors.
- Emit MLIR operations computing `sum`, `avg` and `count` using standard arithmetic.
- Provide a pipeline to dump JSON results after executing the compiled module.
- Add a golden test under `tests/compiler/mlir` when Q1 works.
