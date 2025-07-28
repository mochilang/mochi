# a2mochi Haskell Converter

This package provides a small Haskell frontend for the `a2mochi` tool. It parses
very simple Haskell programs and converts them into Mochi AST form. Only a tiny
subset of the language is recognised – single line function declarations,
variable bindings and trivial `main` blocks built from `putStrLn`, `print` and
`mapM_` loops.

Completed programs: 16/104

- append_builtin
- avg_builtin
- basic_compare
- binary_precedence
- cast_string_to_int
- print_hello
- fun_call
- fun_three_args
- if_then_else
- unary_neg
- for_loop
- for_list_collection
- for_map_collection
- var_assignment
- len_builtin
- sum_builtin
