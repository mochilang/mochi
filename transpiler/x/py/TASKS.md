# Python Transpiler Tasks

## Recent Enhancements (2025-07-19 11:52)
- Added support for integer, float and boolean literals.
- Added binary and unary expression handling so arithmetic works.
- List literals and variable references now emit valid Python.

## Recent Enhancements (2025-07-19 12:19 +07)
- Added variable declarations and assignments for `let`, `var` and `=` statements.
- Added support for indexing expressions like `xs[0]`.

## Recent Enhancements (2025-07-19 12:28 +07)
- Added conditional expressions with `if ... then ... else`.
- Added transpiler tests for `if_then_else` and `if_then_else_nested`.

## Recent Enhancements (2025-07-19 12:40 +07)
- Mapped logical operators `&&`, `||` and `!` to Python syntax.
- Added transpiler support and tests for `in_operator` and `string_compare`.

## Recent Enhancements (2025-07-19 13:18 +07)
- Implemented function definitions, return statements and lambda expressions.
- Added method call handling and `contains` operator.
- Enabled transpiler tests for `fun_call`, `fun_three_args`, `fun_expr_in_let`, `closure`, `bool_chain` and `string_contains`.

## Recent Enhancements (2025-07-19 13:40 +07)
- Added builtin support for `append` and `avg`.
- Enabled transpiler tests for `append_builtin`, `avg_builtin` and `len_string`.

## Recent Enhancements (2025-07-19 17:19 +07)
- Added dictionary literal support and mapping for builtins `count`, `sum` and `values`.
- Enabled transpiler tests for `count_builtin`, `sum_builtin`, `values_builtin`, `str_builtin`, `string_in_operator`, `map_literal_dynamic`, `map_int_key` and `map_index`.

## Recent Enhancements (2025-07-19 17:44 +07)
- Enabled transpiler test for `len_map`.

## Recent Enhancements (2025-07-19 18:01 +07)
- Implemented slice expressions and substring builtin.
- Added support for `while` and `for` loops.
- Enabled transpiler tests for `slice`, `string_prefix_slice`, `substring_builtin`, `while_loop`, `for_loop` and `for_list_collection`.

## Recent Enhancements (2025-07-19 19:05 +07)
- Implemented if statements with optional else branch.
- Added break and continue statements.
- Added index assignment for lists and maps.
- Enabled transpiler tests for `break_continue`, `if_else`, `list_assign`, `map_assign` and `map_in_operator`.

## Recent Enhancements (2025-07-19 19:41 +07)
- Added default value handling for typed `let` and `var` declarations.
- Implemented nested index assignment for lists.
- Generated golden tests for `typed_let`, `typed_var` and `list_nested_assign`.
