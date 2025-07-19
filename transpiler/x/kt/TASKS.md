# Kotlin Transpiler Tasks

_Last updated: 2025-07-19 05:22 UTC_

- Basic programs using `print` are supported.
- Added integer and list literals.
- Built-ins `count`, `sum` and `avg` over integer lists are now emitted using Kotlin collection helpers.
- Added built-ins `len` for strings and lists and `str` for integers.
- Golden tests for `print_hello`, `count_builtin`, `avg_builtin`, `sum_builtin`, `len_builtin`, `len_string` and `str_builtin` compile and run via `kotlinc`.
