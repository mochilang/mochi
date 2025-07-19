# Kotlin Transpiler Tasks

_Last updated: 2025-07-19 06:18 UTC_

- Basic programs using `print` are supported.
- Added integer and list literals.
- Built-ins `count`, `sum` and `avg` over integer lists are now emitted using Kotlin collection helpers.
- Added built-ins `len` for strings and lists and `str` for integers.
- Golden tests for `print_hello`, `count_builtin`, `avg_builtin`, `sum_builtin`, `len_builtin`, `len_string` and `str_builtin` compile and run via `kotlinc`.
- Implemented `let` statements, variable references and basic binary operators. Golden test `let_and_print` now passes.

## VM Golden Progress (2025-07-19 06:18 UTC)
- Added operator precedence handling and parenthesized expressions.
- New golden tests generated for `basic_compare`, `binary_precedence`, `math_ops`, `unary_neg`, `string_compare` and `string_concat`.
