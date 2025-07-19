# Prolog Transpiler Tasks

## Progress (2025-07-19 14:41 +0700)
- Implemented variable assignments and simple arithmetic/boolean expressions.
- Added golden tests for `basic_compare.mochi` and `binary_precedence.mochi`.
- Added support for casts to int and generated code for `cast_string_to_int.mochi`.
- Added typed variable declarations and unary minus handling.
- Added modulo operator support and golden tests for `math_ops.mochi` and `let_and_print.mochi`.
- Updated README checklist for all programs in `tests/vm/valid`.
- Implemented variable reassignment support and golden test for `var_assignment.mochi`.
- 2025-07-19 17:31 +0700 - Added `len` and `str` built-ins, list literals and constant string concatenation. Generated golden files for `len_string.mochi`, `len_builtin.mochi`, `string_concat.mochi` and `str_builtin.mochi`.
- 2025-07-19 18:42 +0700 - Added string comparison support and golden test for `string_compare.mochi`. Updated README checklist accordingly.

## Progress (2025-07-19 19:24 +0700)
- Added list manipulation and numeric aggregation built-ins.
- Implemented `append`, `count`, `sum`, `min`, `max`, `avg` and `substring` support.
- Generated Prolog code and outputs for additional golden tests.
- Updated README checklist to 20/100 passing programs.
