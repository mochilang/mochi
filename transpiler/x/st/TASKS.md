## Progress (21 Jul 2025 16:57 +0700)
- VM valid golden test results updated
- Added match expression support
- match_expr now passes (73/100)

## Progress (21 Jul 2025 16:17 +0700)
- VM valid golden test results updated
- Added nested index assignment for lists and maps
- `list_nested_assign` and `map_nested_assign` now pass (72/100)

## Progress (21 Jul 2025 16:07 +0700)
- VM valid golden test results updated
- Added list and string slicing
- string_prefix_slice now passes
- group_by_multi_join_sort now passes (70/100)

## Progress (21 Jul 2025 15:29 +0700)
- VM valid golden test results updated
- Grouping keys now allow maps and floats
- Unary minus supports floats
- group_by_multi_join_sort now passes (68/100)

## Progress (21 Jul 2025 13:50 +0700)
- VM valid golden test results updated
- Added support for float literals and numeric operations
- Sum builtin now handles floats
- Implemented multi-join queries; join_multi, left_join_multi and group_by_multi_join pass (67/100)

## Progress (21 Jul 2025 13:27 +0700)
- VM valid golden test results updated
- Added support for identifier keys in map literals so queries can "select{n: n}"
- Implemented join queries with grouping; group_by_join and group_by_left_join now pass (64/100)

## Progress (21 Jul 2025 13:09 +0700)
- VM valid golden test results updated
- Added basic query sorting support; group_by_sort.mochi now passes (62/100)

## Progress (21 Jul 2025 12:53 +0700)
- VM valid golden test results updated
- Added support for identifier keys in map literals so queries can "select{n: n}"
- Implemented basic join handling and truthy checks; right_join.mochi now passes

## Progress (21 Jul 2025 12:05 +0700)
- VM valid golden test results updated
- Added basic `group_by` query handling and iteration over group items
- `group_by.mochi` now passes (58/100)

# Smalltalk Transpiler Tasks
## History
- 2025-07-19 05:32:23 +0000 - st transpiler: basic constant folding
- 2025-07-19 11:48:47 +0700 - Add Smalltalk transpiler

## Recent Updates (2025-07-19 06:01 +0000)
- Added golden tests for let_and_print, typed_let, typed_var, unary_neg, string_concat and var_assignment.
- Generated Smalltalk sources and expected outputs.
