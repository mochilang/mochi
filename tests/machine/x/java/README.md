# Mochi to Java Compiler Results

This directory stores machine-generated Java programs compiled from the Mochi sources in `tests/vm/valid`.
Each `.java` file is the output from the compiler under `compiler/x/java`. For successful compilations the
program is executed and its stdout is written to a `.out` file. Failed compilations or executions produce a
`.error` file containing the error message and surrounding source context.

## Completed Examples

- [x] print_hello.mochi
- [x] var_assignment.mochi
- [x] typed_var.mochi
- [x] typed_let.mochi
- [x] for_loop.mochi
- [x] string_concat.mochi
- [x] string_compare.mochi
- [x] math_ops.mochi
- [x] bool_chain.mochi
- [x] unary_neg.mochi
- [x] while_loop.mochi
