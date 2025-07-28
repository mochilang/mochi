# a2mochi C++ Converter

This directory contains helpers and golden files for converting C++ programs under
`tests/transpiler/x/cpp` back into Mochi AST form using `clang++`.

Completed programs: 3/104

Supported examples:
- [x] print_hello
- [x] for_loop
- [x] let_and_print

The converter is experimental and only understands a very small subset of the
C++ code emitted by the Mochi transpiler.
