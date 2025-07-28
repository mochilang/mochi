# a2mochi Pascal Converter

This directory contains the test helpers and golden files for converting
Pascal programs under `tests/transpiler/x/pas` into Mochi AST form.  The
implementation is mostly regex based and is inspired by the Python and
TypeScript converters.

Completed programs: 16/104

Supported features include:
- basic `program` blocks with variable declarations
- simple `for` and `while` loops
- `if` statements and `repeat/until`
- assignments and `writeln` calls mapped to `print`

## Checklist
- [x] binary_precedence
- [x] for_loop
- [x] len_builtin
- [x] len_string
- [x] let_and_print
- [x] list_assign
- [x] list_index
- [x] print_hello
- [x] str_builtin
- [x] string_compare
- [x] string_concat
- [x] string_index
- [x] sum_builtin
- [x] unary_neg
- [x] while_loop
