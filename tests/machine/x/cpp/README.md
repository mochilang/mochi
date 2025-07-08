# Mochi to C++ Machine Outputs

This directory stores machine generated C++ translations of programs from
`tests/vm/valid`. Each entry is compiled and executed during tests. If a program
fails to compile or run, a `.error` file will contain the diagnostic details.

Checklist of programs that currently compile and run (43/97):

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
- [x] fun_call
- [x] fun_three_args
- [x] fun_expr_in_let
- [x] if_else
- [x] in_operator
- [x] len_builtin
- [x] len_string
- [x] let_and_print
- [x] list_index
- [x] math_ops
- [x] membership
- [x] min_max_builtin
- [x] print_hello
- [x] pure_fold
- [x] short_circuit
- [x] slice
- [x] str_builtin
- [x] string_compare
- [x] string_concat
- [x] string_contains
- [x] string_index
- [x] string_in_operator
- [x] string_prefix_slice
- [x] substring_builtin
- [x] sum_builtin
- [x] tail_recursion
- [x] two-sum
- [x] typed_let
- [x] typed_var
- [x] unary_neg
- [x] var_assignment
- [x] while_loop

