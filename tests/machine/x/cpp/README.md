# Mochi to C++ Machine Outputs

This directory stores machine generated C++ translations of programs from
`tests/vm/valid`. Each entry is compiled and executed during tests. If a program
fails to compile or run, a `.error` file will contain the diagnostic details.

Checklist of programs that currently compile and run:

- [ ] append_builtin
- [ ] avg_builtin
- [ ] binary_precedence
- [ ] bool_chain
- [ ] for_loop
- [ ] math_ops
- [ ] print_hello
- [ ] var_assignment
- [ ] while_loop

