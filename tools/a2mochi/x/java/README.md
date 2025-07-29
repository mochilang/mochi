# a2mochi Java Converter

This directory contains helpers and golden files for converting Java programs under `tests/transpiler/x/java` back into Mochi AST form.

Completed programs: 29/103

The converter is experimental and supports simple variable declarations,
assignments, print statements and basic `while`/`for` loops.

## Checklist
- [x] let_and_print
- [x] print_hello
- [x] unary_neg
- [x] var_assignment
- [x] for_loop
- [x] while_loop
- [x] binary_precedence
- [x] math_ops
- [x] string_concat
- [x] basic_compare
- [x] cast_string_to_int
- [x] if_else
- [x] typed_let
- [x] typed_var
- [x] for_list_collection
- [x] len_builtin
- [x] len_string
- [x] break_continue
- [x] string_compare
- [x] string_contains
- [x] string_index
- [x] str_builtin
- [x] substring_builtin
- [x] string_prefix_slice
- [x] if_then_else
- [x] if_then_else_nested
- [x] list_index
- [x] slice
