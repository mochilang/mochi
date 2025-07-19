# Erlang Transpiler Progress

## Recent Updates
- 2025-07-19 17:43 +0000 - Removed runtime helpers and improved printing with io:format. Updated checklist.
- 2025-07-19 21:14 +0700 - Added math operations and golden files for math_ops.
- 2025-07-19 07:49 +0000 - Added `in` operator for lists. Generated golden files for in_operator and membership.
- 2025-07-19 13:31 +0700 - Added built-ins `append`, `avg`, `count`, `sum`, `min` and `max`. Generated golden files for append_builtin, avg_builtin, count_builtin, sum_builtin and min_max_builtin.
- 2025-07-19 12:35 +0700 - Added list literals and `if` expressions. Generated golden files for len_builtin, let_and_print, string_concat, if_then_else and if_then_else_nested.
- 2025-07-19 12:23 +0700 - Added support for `len` and `str` built-ins plus unary negation. Generated golden files for len_string, str_builtin, string_compare and unary_neg.
- 2025-07-19 11:52 +0700 - Initial stub supporting only `print` with string literals.
- 2025-07-19 12:04 +0700 - Added integer and boolean expressions, variable bindings and casts. Generated golden files for basic_compare, binary_precedence and cast_string_to_int.
- 2025-07-19 13:47 +0700 - Added if statement support and golden test for if_else.
