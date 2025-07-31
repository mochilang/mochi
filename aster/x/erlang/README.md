# Erlang AST Printer

This directory contains utilities for inspecting Erlang source code using
Tree-sitter and printing it back from the parsed AST.  Golden tests verify that
the printed source matches the original program output.

## Golden files status

- [x] append_builtin.erl
- [x] avg_builtin.erl
- [x] basic_compare.erl
- [x] bool_chain.erl
- [x] binary_precedence.erl

Completed 5/5 on 2025-07-31 14:20 GMT+7
