# Rust Backend Tasks for TPCH Q1

The Rust compiler now supports running the `tpc-h/q1.mochi` benchmark. Grouping
with optional filters is handled using `HashMap` and `Vec` and aggregate helpers
for `sum`, `avg` and `count`.

Completed tasks:

- [x] Use `HashMap` and `Vec` to implement grouping of rows by key.
- [x] Generate `struct` definitions for TPCH rows with `serde` `Serialize` implementations.
- [x] Provide iterator-based helpers for `sum`, `avg` and `count`.
- [x] Add a golden test in `tests/compiler/rust` once the query compiles successfully.
- [x] Support iterating over group values and updated builtins to use `_count`, `_avg` and `_sum` helpers.
- [x] Added TPCH q1 generated code under `tests/dataset/tpc-h/compiler/rust`.

## Remaining work

- [ ] Generate valid Rust for JOB dataset queries. Field access on map values
  currently emits invalid syntax like `m.field.contains`.
- [ ] Enable runtime tests for `job/q1.mochi` and `job/q2.mochi` once the
  generated code compiles successfully.
