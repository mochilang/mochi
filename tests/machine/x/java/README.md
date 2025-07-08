# Mochi to Java Compiler Results

This directory stores machine-generated Java programs compiled from the Mochi sources in `tests/vm/valid`.
Each `.java` file is the output from the compiler under `compiler/x/java`. For successful compilations the
program is executed and its stdout is written to a `.out` file. Failed compilations or executions produce a
`.error` file containing the error message and surrounding source context.

## Completed Examples (31/97)

- [x] avg_builtin.mochi
- [x] basic_compare.mochi
- [x] binary_precedence.mochi
- [x] bool_chain.mochi
- [x] cast_string_to_int.mochi
- [x] count_builtin.mochi
- [x] for_list_collection.mochi
- [x] for_loop.mochi
- [x] fun_call.mochi
- [x] fun_three_args.mochi
- [x] if_else.mochi
- [x] len_builtin.mochi
- [x] len_map.mochi
- [x] len_string.mochi
- [x] let_and_print.mochi
- [x] math_ops.mochi
- [x] print_hello.mochi
- [x] pure_fold.mochi
- [x] pure_global_fold.mochi
- [x] short_circuit.mochi
- [x] str_builtin.mochi
- [x] string_compare.mochi
- [x] string_concat.mochi
- [x] substring_builtin.mochi
- [x] sum_builtin.mochi
- [x] tail_recursion.mochi
- [x] typed_let.mochi
- [x] typed_var.mochi
- [x] unary_neg.mochi
- [x] var_assignment.mochi
- [x] while_loop.mochi
