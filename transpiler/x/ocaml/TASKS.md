## Progress (2025-07-21 15:41 +0700)
- Checklist updated: 61/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 08:34 +0000)
- Checklist updated: 55/100 tests compiled
- Added `where` filters and group `having` support.
- Generated dataset_where_filter golden output.

## Progress (2025-07-21 15:18 +0700)
- Checklist updated: 55/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 15:18 +0700)
- Checklist updated: 55/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 15:18 +0700)
- Checklist updated: 55/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 15:18 +0700)
- Checklist updated: 55/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 15:18 +0700)
- Checklist updated: 54/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 15:18 +0700)
- Checklist updated: 54/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 08:23 +0000)
- Checklist updated: 55/100 tests compiled
- Added query filtering with `where` clauses and group `having` support.
- Generated dataset_where_filter golden output.

## Progress (2025-07-21 15:18 +0700)
- Checklist updated: 54/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 15:18 +0700)
- Checklist updated: 54/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 13:50 +0700)
- Checklist updated: 54/100 tests compiled
- Implemented typed lists and maps for better inference.
- Added group_by query support and removed runtime helpers.

## Progress (2025-07-21 12:53 +0700)
- Checklist updated: 54/100 tests compiled
- Added initial support for `group_by` queries without runtime helpers.
- Implemented simple grouping logic and group key field access.

## Progress (2025-07-21 07:08 +0700)
- Checklist updated: 53/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 07:08 +0700)
- Checklist updated: 53/100 tests compiled
- Added cross join query support.

## Progress (2025-07-21 07:08 +0700)
- Checklist updated: 53/100 tests compiled
- Added support for `list_nested_assign` and improved list assignment handling.

## Progress (2025-07-20 16:45 +0700)
- Checklist updated: 52/100 tests compiled
- Added support for function return types so `closure` now compiles.
- Improved type inference for function expressions.

## Progress (2025-07-20 08:24 UTC)
- Checklist updated: 56/100 tests compiled
- Added support for `break` and `continue` statements using recursive loops.
- Implemented list slicing for lists.
- Enabled golden tests for `break_continue`, `short_circuit`, `slice`, `tail_recursion` and `two-sum`.

## Progress (2025-07-20 15:14 +0700)
- Checklist updated: 51/100 tests compiled
- Added support for `list_nested_assign` and improved list assignment handling.

## Progress (2025-07-20 08:02 UTC)
- Checklist updated: 51/100 tests compiled
- Added map type inference with support for integer keys and nested updates.
- Implemented string slicing and map iteration loops.
- Enabled golden tests for for_map_collection, map_int_key, map_nested_assign, nested_function, string_prefix_slice.

## Progress (2025-07-20 14:31 +0700)
- Checklist updated: 46/100 tests compiled
- Added support for `fun_expr_in_let`, `fun_three_args`, `membership`, `min_max_builtin` and `values_builtin`. Improved list printing for list outputs.

## Progress (2025-07-20 06:26 UTC)
- Checklist updated: 41/100 tests compiled
- Added `in` operator handling for lists, maps and strings.
- Implemented match expressions and multi-argument printing.
- Enabled `len_map`, `map_literal_dynamic`, `map_membership`, `map_in_operator`, `string_in_operator` and `match_expr` golden tests.

## Progress (2025-07-20 12:54 +0700)
- Checklist updated: 35/100 tests compiled
- Added support for `len` builtin on lists.

## Progress (2025-07-20 12:19 +0700)
- Checklist updated: 34/100 tests compiled
- Added support for `map_assign` using association lists.

## Progress (2025-07-20 11:55 +0700)
- Checklist updated: 33/100 tests compiled
- Added support for `map_index` using association lists.

## Progress (2025-07-20 10:18 +0700)
- Checklist updated: 60/100 tests compiled
- Removed runtime helper functions for cleaner output.
- Enhanced type inference and code generation.

## Progress (2025-07-20 09:50 +0700)
- Checklist updated: 32/100 tests compiled
- Added support for `count_builtin` using `List.length`.
- Improved list assignment handling for nested updates.

## Progress (2025-07-20 09:17 +0700)
- Checklist updated: 31/100 tests compiled
- Added support for `list_nested_assign` and improved list assignment handling.

## Progress (2025-07-20 08:41 +0700)
- Checklist updated: 30/100 tests compiled
- Added support for `cast_string_to_int` and improved type inference.

## Progress (2025-07-20 08:27 +0700)
- Checklist updated: 29/100 tests compiled
- Added `avg_builtin` and `fun_call` programs with support for function parameters.
- Simplified boolean printing and improved function declarations.

## Progress (2025-07-20 08:14 +0700)
- Checklist updated: 27/100 tests compiled
- Added list index assignment support generating `List.mapi` updates. Generated golden files for `list_assign`.

## Progress (2025-07-19 19:12 +0000)
- Checklist updated: 26/100 tests compiled
- Added support for `append_builtin`, `list_index` and `string_index` programs. Removed runtime helpers and improved type inference.

## Progress (2025-07-20 02:02 +0700)
- Checklist updated: 23/100 tests compiled
- Added support for `substring_builtin` and `sum_builtin` programs.

## Progress (2025-07-20 01:34 +0700)
- Checklist updated: 23/100 tests compiled
- Added support for `bool_chain` and improved boolean output.

## Progress (2025-07-19 19:41 +0700)
- Checklist updated: 22/100 tests compiled
- Added support for `substring_builtin` and `sum_builtin` programs.

# OCaml Transpiler Progress

## Recent Updates
- 2025-07-19 18:32 +0700 - Added support for 'for_loop' and 'for_list_collection' programs.
- 2025-07-19 10:40 +0000 - Added support for 'if_else' and 'while_loop' programs.
- 2025-07-19 07:53 +0000 - Added support for 'typed_let', 'typed_var', 'var_assignment' and 'len_string' programs.
- 2025-07-19 13:56 +0700 - Added support for 'binary_precedence', 'if_then_else_nested', and 'str_builtin' programs.
- 2025-07-19 13:18 +0700 - Added support for 'if_then_else' and 'unary_neg' programs.
- 2025-07-19 12:42 - Added support for 'string_concat' and 'string_compare' programs.
- 2025-07-19 11:52 - Initial checklist generated from `tests/vm/valid` and added support for `basic_compare` program.
- 2025-07-19 05:24 - Added support for 'let_and_print' and 'math_ops' programs.
