# Rust Backend Tasks for TPCH Q1

The Rust compiler is built on the Go AST walker and lacks high-level dataset support.

- Use `HashMap` and `Vec` to implement grouping of rows by key.
- Generate `struct` definitions for TPCH rows with `serde` `Serialize` implementations.
- Provide iterator-based helpers for `sum`, `avg` and `count`.
- Add a golden test in `tests/compiler/rust` once the query compiles successfully.
