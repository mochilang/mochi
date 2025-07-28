# a2mochi Racket Converter

This directory contains a very small converter that translates simple
Racket programs back into Mochi form. It is inspired by the Python and
TypeScript converters and is only powerful enough for the examples used
in the repository tests.

The converter does not rely on a language server. It tokenises the input
and recognises basic forms such as `define`, `struct` and `for`. Only a
subset of expressions and statements are supported.

Completed programs: 11/104

## Checklist
- [x] append_builtin
- [x] fun_call
- [x] fun_three_args
- [x] let_and_print
- [x] for_loop
- [x] for_list_collection
- [x] print_hello
- [x] unary_neg
- [x] basic_compare
- [x] string_concat
- [x] list_index

