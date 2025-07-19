# Kotlin Transpiler Tasks

_Last updated: 2025-07-19T12:53:06+00:00_

- Basic programs using `print` are supported.
- Added integer and list literals.
- Built-ins `count`, `sum` and `avg` over integer lists are now emitted using Kotlin collection helpers.
- Added built-ins `len` for strings and lists and `str` for integers.
- Golden tests for `print_hello`, `count_builtin`, `avg_builtin`, `sum_builtin`, `len_builtin`, `len_string` and `str_builtin` compile and run via `kotlinc`.
- Implemented `let` statements, variable references and basic binary operators. Golden test `let_and_print` now passes.
- Added variable declarations with `var` and assignment statements.
- Implemented boolean literals and operators `&&`, `||`, `!`.
- New built-ins: `append`, `min`, `max` and `substring`.
- Added map literals and indexing expressions for lists and strings.
- Built-in `contains` for strings and support for the `in` operator.
- `len` now works for maps.
- Added built-in `values` for maps.

## VM Golden Progress (2025-07-19T12:29:09+00:00)
- Added support for `if` statements and `while` loops in the Kotlin transpiler.
- Implemented conditional expressions via Kotlin `if`.
- Generated golden tests for `if_else`, `if_then_else`, `if_then_else_nested` and `while_loop` and updated README progress to 31/100.

## VM Golden Progress (2025-07-19 11:39 UTC)
- Implemented function definitions and return statements.
- Generated Kotlin source for `bool_chain` and updated README progress to 27/100.

## VM Golden Progress (2025-07-19 10:39 UTC)
- Added operator precedence handling and parenthesized expressions.
- New golden tests generated for `basic_compare`, `binary_precedence`, `math_ops`, `unary_neg`, `string_compare` and `string_concat`.
- More Kotlin tests run for `append_builtin`, `min_max_builtin`, `substring_builtin`, `typed_let`, `typed_var` and `var_assignment`.
- Added tests for `len_map`, `list_index`, `string_contains`, `string_in_operator` and `string_index`.
- Added test for `values_builtin`.

## VM Golden Progress (2025-07-19T12:53:06+00:00)
- Implemented `for` loops over ranges and collections.
- Added `break` and `continue` statements.
- Generated Kotlin sources for `for_loop`, `for_list_collection` and `break_continue` and updated README progress to 34/100.
