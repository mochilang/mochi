# Rust Backend Tasks for TPCH Q1

The Rust compiler now supports running the `tpc-h/q1.mochi` benchmark. Grouping
with optional filters is handled using `HashMap` and `Vec` and aggregate helpers
for `sum`, `avg` and `count`.

Completed tasks:

- [x] Use `HashMap` and `Vec` to implement grouping of rows by key.
- [x] Generate `struct` definitions for TPCH rows with `serde` `Serialize` implementations.
- [x] Provide iterator-based helpers for `sum`, `avg` and `count`.
- [x] Add a golden test in `tests/compiler/rust` once the query compiles successfully.
