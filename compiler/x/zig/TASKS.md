# Zig Backend Progress

## Recent Enhancements
- 2025-07-15 07:14 - Added `compile_tpcds_zig.go` script and fixed empty struct handling in `zigTypeOf`.
- 2025-07-15 05:45 - Golden test captures build errors and verifies TPCDS outputs.
- 2025-07-15 05:00 - Added TPCDS golden test running q1-q99 for zig backend.
- 2025-07-13 05:12 - Added `ensureGroupSlice` helper to avoid repeated `.Items.items` chains when iterating grouped data.
- 2025-07-13 17:48 - Expanded TPCH golden tests to compile and run q1-q22 with output verification.
- 2025-07-16 00:00 - Renamed user-defined `main` to `user_main` to prevent conflicts with the exported entry point.
- 2025-07-16 02:00 - Added VM valid golden tests and improved float sum detection.
- 2025-07-16 17:30 - `_append` now returns an owned slice and lists print via `_print_list`.
- 2025-07-17 01:30 - Removed duplicate VM valid test file and regenerated golden files for `tests/vm/valid`.
- 2025-07-17 01:17 - Fixed closure compilation in `compileFunExpr` to use a bound function and cleared generated `.error` file.

## Remaining Work
- Support struct field type inference for TPCH data sets.
- Confirm length handling for group expressions.
- 2025-07-17 compiled dataset_sort_take_limit.mochi in Zig
- 2025-07-17 compiled exists_builtin.mochi in Zig
- 2025-07-17 compiled break_continue.mochi in Zig
- 2025-07-17 compiled cast_string_to_int.mochi in Zig
- 2025-07-17 compiled cast_struct.mochi in Zig
- 2025-07-17 compiled closure.mochi in Zig
- 2025-07-17 compiled count_builtin.mochi in Zig
- 2025-07-17 compiled cross_join.mochi in Zig
- 2025-07-17 compiled cross_join_filter.mochi in Zig
- 2025-07-17 compiled cross_join_triple.mochi in Zig
- 2025-07-17 compiled dataset_where_filter.mochi in Zig
- 2025-07-17 compiled for_loop.mochi in Zig
- 2025-07-17 compiled for_list_collection.mochi in Zig
- 2025-07-17 compiled for_map_collection.mochi in Zig
- 2025-07-17 compiled fun_call.mochi in Zig
- 2025-07-17 compiled fun_expr_in_let.mochi in Zig
- 2025-07-17 compiled fun_three_args.mochi in Zig
- 2025-07-17 compiled list_index.mochi and string_index.mochi in Zig
- 2025-07-17 improved slice and index inference to remove runtime helpers
- 2025-07-17 enhanced int constant folding and dynamic slice ranges
- 2025-07-18 refined skip/take slicing to use direct range expressions
- 2025-07-19 removed list membership helper when element type is known,
  using `std.mem.indexOfScalar` instead
- 2025-07-20 added substring builtin support and negative index/slice
  handling without runtime helpers

- 2025-07-21 improved substring builtin to emit direct slice when bounds are constant
- 2025-07-21 infer range loop indices to avoid index helpers
- 2025-07-22 compiled go_auto, python_auto, and python_math.mochi in Zig and updated machine README to 100/100.
