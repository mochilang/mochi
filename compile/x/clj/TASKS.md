# Clojure Backend Tasks for TPCH Q1

The Clojure compiler currently only supports basic loops and list processing. When
compiling `q1.mochi` it fails on grouping and struct handling.

- Lower dataset queries to Clojure's `group-by`, `map` and `reduce` functions.
- Generate Clojure records for Mochi structs like `lineitem` and emit field accessors.
- Provide helpers implementing `sum`, `avg` and `count` over grouped sequences.
- Use a library such as `cheshire` to produce JSON output for the result.
- Add a golden test in `tests/compiler/clj` after the query compiles.
