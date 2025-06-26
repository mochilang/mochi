# C++ Backend Tasks for TPCH Q1

The C++ backend mirrors the C compiler but currently stops when encountering
`group by` clauses.

- Use `std::unordered_map` to accumulate groups of rows keyed by expressions.
- Emit template helpers for `sum`, `avg` and `count` working on `std::vector` values.
- Define `struct` types for TPCH rows and ensure field access compiles correctly.
- Provide JSON output via a small library such as `nlohmann/json`.
- Add tests under `tests/compiler/cpp` running the generated program on `q1.mochi`.
