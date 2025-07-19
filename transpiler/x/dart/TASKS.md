# Dart Transpiler Tasks

## Progress (2025-07-19T06:28:31+00:00)
- Initial Dart transpiler handled only function calls with string literals.
- Added support for `let` statements, integer literals and `+` expressions.
- Golden tests generate Dart code and run `print_hello.mochi` and `let_and_print.mochi`.
- Support for `var` declarations, assignments, unary `-`, and basic operators.
- Added support for simple `if` statements.
- Added comparisons and typed variables; updated golden tests.
