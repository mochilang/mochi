# a2mochi Ruby Converter

This directory contains a minimal converter that translates simple Ruby
programs back into Mochi form. It uses Ruby's built in `ripper` library to
obtain an s-expression AST which is then converted to Mochi code. The
implementation mirrors the Python and TypeScript converters and is only
powerful enough for the examples under `tests/transpiler/x/rb`.

Completed programs: 40/104

## Checklist
- [x] append_builtin
- [x] avg_builtin
- [x] basic_compare
- [x] binary_precedence
- [x] bool_chain
- [x] break_continue
- [x] cast_string_to_int
- [x] closure
- [x] count_builtin
- [x] for_list_collection
- [x] for_loop
- [x] for_map_collection
- [x] fun_call
- [x] if_else
- [x] len_builtin
- [x] len_map
- [x] len_string
- [x] let_and_print
- [x] list_assign
- [x] list_index
- [x] map_assign
- [x] map_index
- [x] map_int_key
- [x] map_literal_dynamic
- [x] map_membership
- [x] map_nested_assign
- [x] membership
- [x] print_hello
- [x] str_builtin
- [x] string_compare
- [x] string_concat
- [x] string_contains
- [x] string_in_operator
- [x] string_index
- [x] sum_builtin
- [x] unary_neg
- [x] var_assignment
- [x] while_loop
