# C++ Backend Tasks for TPCH Q1

The backend now supports compiling `tests/dataset/tpc-h/q1.mochi`. Grouping is
implemented using `std::unordered_map` and helper functions provide `sum`,
`avg` and `count` over `std::vector` values. Generated structs allow field
access and results are serialised to JSON.

Golden tests under `tests/compiler/cpp` exercise `tpch_q1.mochi`.

The JOB dataset queries `q1` through `q10` now have golden tests that
exercise the C++ backend. The generated code is compared against the
`tests/dataset/job/compiler/cpp` directory and each program is compiled
and executed. Runtime output is validated for the first ten queries.

The TPC-DS suite has been expanded. Compilation now attempts queries
`q1` through `q99` and golden tests cover any that successfully build
and run under `tests/dataset/tpc-ds/compiler/cpp`.

Some generated programs still rely on `auto` in template parameters which
prevents successful compilation with `g++`. Remaining work includes
emitting concrete types for map and vector values so that all JOB queries
build and run without errors.

## Outstanding

- `tpch_q2.mochi` does not compile to runnable C++ yet. Nested field
  access in query joins emits dot notation on `unordered_map` values and
  the generated grouping map uses a map key type that is itself another
  `unordered_map`. The compiler needs to track element types for `from`
  and `join` variables inside queries so that map fields are emitted with
  `[]` indexing and grouping keys use concrete structs.
