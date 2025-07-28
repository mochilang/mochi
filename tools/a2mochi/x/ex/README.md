# Elixir AST Conversion

This directory provides a small converter that turns a subset of Elixir source
code into Mochi AST form. The implementation mirrors the Python and TypeScript
converters and is mostly regex based.

Completed programs: 20/104

## Checklist
- [x] append_builtin
- [x] basic_compare
- [x] binary_precedence
- [x] bool_chain
- [x] let_and_print
- [x] print_hello
- [x] unary_neg
- [x] count_builtin
- [x] sum_builtin
- [x] len_builtin
- [x] len_string
- [x] string_concat
- [x] string_compare
- [x] avg_builtin
- [x] min_max_builtin
- [x] string_contains
- [x] substring_builtin
- [x] for_loop
- [x] if_else
- [x] var_assignment

Supported features include basic `def` functions, simple control flow and
`IO.puts` calls. More advanced Elixir constructs such as macros or pattern
matching are not handled.
