# AST Printer Support 7/12 (2025-07-28 06:44 GMT+0)

The following programs from `tests/vm/valid` are currently covered by
`printer_test.go`. Checked items successfully round-trip from source to
AST and back with matching output.

- [ ] append_builtin
- [ ] avg_builtin
- [ ] basic_compare
- [ ] binary_precedence
- [ ] bool_chain
- [x] break_continue
- [x] cast_string_to_int
- [x] cast_struct
- [x] closure
- [x] count_builtin
- [x] cross_join
- [x] cross_join_filter
