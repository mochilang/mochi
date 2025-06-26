# C++ Backend Tasks for TPCH Q1

The backend now supports compiling `tests/dataset/tpc-h/q1.mochi`. Grouping is
implemented using `std::unordered_map` and helper functions provide `sum`,
`avg` and `count` over `std::vector` values. Generated structs allow field
access and results are serialised to JSON.

Golden tests under `tests/compiler/cpp` exercise `tpch_q1.mochi`.

Further work will broaden coverage of the remaining TPCH queries.
