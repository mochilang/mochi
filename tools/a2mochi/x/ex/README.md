# Elixir AST Conversion

This directory provides a small converter that turns a subset of Elixir source
code into Mochi AST form. The implementation mirrors the Python and TypeScript
converters and is mostly regex based.

Completed programs: 5/104

Supported features include basic `def` functions, simple control flow and
`IO.puts` calls. More advanced Elixir constructs such as macros or pattern
matching are not handled.

## Checklist
- [x] bool_chain
- [x] let_and_print
- [x] print_hello
- [x] unary_neg
- [x] var_assignment
