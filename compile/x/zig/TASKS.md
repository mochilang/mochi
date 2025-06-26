# Zig Backend Tasks for TPCH Q1

Grouping support for simple queries has been implemented so that
`tests/dataset/tpc-h/q1.mochi` compiles successfully. Aggregation helpers
(`sum`, `avg`, `count`) are available and JSON output is handled via
`std.json`.

Remaining work
---------------

- Replace the current linear search grouping with an optimized
  `std.AutoHashMap` implementation.
- Add join and sorting support for query expressions.
