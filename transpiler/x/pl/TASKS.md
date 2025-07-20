# Prolog Transpiler Tasks

## Progress (2025-07-19 12:43 UTC)
- Implemented `substring` builtin and added golden test.
- README checklist updated to reflect 21/100 passing programs.

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

## Progress (2025-07-20 08:31 +0700)
- Added boolean operators `&&` and `||` with proper short-circuit semantics.
- Removed the `substring/4` runtime helper and emit direct calls to `sub_string/5`.
- Improved code generation readability and type inference.

## Progress (2025-07-20 09:18 +0700)
- Switched generated output to use `writeln/1` for readability.
- Removed timestamped header for deterministic results.
- Updated README checklist (21/100).

## Recent Updates
- [2025-07-20T09:18:48+07:00] Clean print statements and deterministic header; progress 21/100
