# Mochi to Prolog Machine Outputs

This directory stores machine generated Prolog translations of programs from `tests/vm/valid`. Each entry is compiled and executed during tests. If a program fails to compile or run, a `.error` file will contain the diagnostic details.

Checklist of programs that currently compile and run (31/97):

- print_hello
- let_and_print
- var_assignment
- for_loop
- for_list_collection
- append_builtin
- avg_builtin
- basic_compare
- binary_precedence
- math_ops
- unary_neg
- len_builtin
- len_string
- sum_builtin
- min_max_builtin
- count_builtin
- bool_chain
- fun_call
- fun_three_args
- string_concat
- typed_var
- typed_let
- if_then_else
- if_then_else_nested
- if_else
- map_index
- map_int_key
- map_literal_dynamic
- map_in_operator
- map_membership
- membership
