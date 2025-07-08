# Mochi to C++ Machine Outputs

This directory stores machine generated C++ translations of programs from `tests/vm/valid`.
Each entry is compiled and executed during tests. If a program fails to compile or run,
a `.error` file will contain the diagnostic details.

Checklist of programs that currently compile and run (24/97):
- [x] basic_compare
- [x] for_list_collection
- [x] for_loop
- [x] fun_call
- [x] fun_three_args
- [x] if_else
- [x] len_builtin
- [x] len_map
- [x] let_and_print
- [x] list_assign
- [x] list_index
- [x] map_assign
- [x] map_index
- [x] math_ops
- [x] print_hello
- [x] pure_fold
- [x] short_circuit
- [x] slice
- [x] string_compare
- [x] string_index
- [x] tail_recursion
- [x] two-sum
- [x] var_assignment
- [x] while_loop

