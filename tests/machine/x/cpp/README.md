# Mochi to C++ Machine Outputs

This directory stores machine generated C++ translations of programs from
`tests/vm/valid`. Each entry is compiled and executed during tests. If a program
fails to compile or run, a `.error` file will contain the diagnostic details.

Checklist of programs that currently compile and run:

- [x] append_builtin
- [x] avg_builtin
- [x] binary_precedence
- [x] bool_chain
- [x] break_continue
- [x] for_list_collection
- [x] for_loop
- [x] fun_call
- [x] fun_three_args
- [x] if_else
- [x] len_builtin
- [x] let_and_print
- [x] list_index
- [x] math_ops
- [x] print_hello
- [x] pure_fold
- [x] short_circuit
- [x] string_compare
- [x] string_index
- [x] sum_builtin
- [x] tail_recursion
- [x] two-sum
- [x] unary_neg
- [x] var_assignment
- [x] while_loop

