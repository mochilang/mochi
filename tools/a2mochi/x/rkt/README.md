# a2mochi Racket Converter

This directory contains a very small converter that translates simple
Racket programs back into Mochi form. It is inspired by the Python and
TypeScript converters and is only powerful enough for the examples used
in the repository tests.

The converter does not rely on a language server. It tokenises the input
and recognises basic forms such as `define`, `struct` and `for`. Only a
subset of expressions and statements are supported.

Completed programs: 39/104

## Checklist
- [x] append_builtin
- [x] fun_call
- [x] fun_three_args
- [x] let_and_print
- [x] for_loop
- [x] for_list_collection
- [x] for_map_collection
- [x] print_hello
- [x] unary_neg
- [x] basic_compare
- [x] string_concat
- [x] list_index
- [x] bool_chain
- [x] if_then_else
- [x] short_circuit
- [x] avg_builtin
- [x] binary_precedence
- [x] cast_string_to_int
- [x] cast_struct
- [x] closure
- [x] fun_expr_in_let
- [x] list_assign
- [x] map_index
- [x] string_index
- [x] substring_builtin
- [x] map_assign
- [x] var_assignment
- [x] values_builtin
- [x] sum_builtin
- [x] min_max_builtin
- [x] string_compare
- [x] membership
- [x] str_builtin
- [x] math_ops
- [x] string_contains
- [x] len_builtin
- [x] len_map
- [x] len_string
- [x] count_builtin

