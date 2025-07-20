## Progress (2025-07-20 09:22 +0700)
- Improved type inference for strings, booleans and constant lists.
- Removed AST debug helpers and cleaned generated code format.
- Regenerated golden files and updated README checklist (26/100 tests passing)

## Progress (2025-07-20 08:31 +0700)
- Added support for range and list based `for` loops.
- Generated golden tests for `for_loop` and `for_list_collection` (26/100 tests passing)

## Progress (2025-07-19 19:14 +0000)
- Improved code readability and removed unused helper functions.
- Updated header timestamp format and README checklist.
- Regenerated golden files - 24/100 vm valid programs passing

# Zig Transpiler Tasks

- 2025-07-20 08:31 +0700 - Added for-loop support and generated golden tests for new loops (26/100 tests passing)

- 2025-07-19 19:14 +0000 - Improved code generation and cleaned up helper logic (24/100 tests passing)
- 2025-07-19 20:15 +0700 - Added constant folding for `min`, `max` and `substring`; generated golden tests for substring_builtin and min_max_builtin (24/100 tests passing)
- 2025-07-19 19:46 +0700 - Added index assignment and string comparison support; generated golden tests for list_assign, list_nested_assign and string_compare (22/100 tests passing)

- 2025-07-19 12:39 +0000 - Added `str` and `sum` builtin constant folding; generated golden tests for str_builtin and sum_builtin (19/100 tests passing)
- 2025-07-19 18:46 +0700 - Added list literal and indexing support; generated golden tests for len_builtin and list_index (17/100 tests passing)
- 2025-07-19 17:47 +0700 - Added while loop support and generated golden test for while_loop (15/100 tests passing)
- 2025-07-19 13:18 +0700 - Added `len` builtin and generated golden tests for basic_compare, len_string and let_and_print (9/100 tests passing)
- 2025-07-19 07:53 +0000 - Added if statements, conditional expressions and constant string concatenation; generated golden tests for binary_precedence, string_concat, if_else, if_then_else and if_then_else_nested (14/100 tests passing)

- 2025-07-19 12:07 +0700 - Initial support for variable declarations, assignments and integer expressions (5/100 tests passing)
- 2025-07-19 12:57 +0700 - Added math operations support (6/100 tests passing)
