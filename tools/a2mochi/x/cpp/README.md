# a2mochi C++ Converter

This directory contains helpers and golden files for converting C++ programs under
`tests/transpiler/x/cpp` back into Mochi AST form using `clang++`.

Completed programs: 15/104

Supported examples:
- [x] print_hello
- [x] for_loop
- [x] let_and_print
- [x] typed_let
- [x] typed_var
- [x] unary_neg
- [x] var_assignment
- [x] while_loop
- [x] len_builtin
- [x] len_string
- [x] basic_compare
- [x] binary_precedence
- [x] string_concat
- [x] string_compare
- [x] bool_chain

The converter is experimental and only understands a very small subset of the
C++ code emitted by the Mochi transpiler.
