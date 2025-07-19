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
