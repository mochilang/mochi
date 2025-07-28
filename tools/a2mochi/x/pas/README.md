# a2mochi Pascal Converter

This directory contains the test helpers and golden files for converting
Pascal programs under `tests/transpiler/x/pas` into Mochi AST form.  The
implementation is mostly regex based and is inspired by the Python and
TypeScript converters.

Completed programs: 15/104

Supported features include:
- basic `program` blocks with variable declarations
- simple `for` and `while` loops
- `if` statements and `repeat/until`
- assignments and `writeln` calls mapped to `print`

## Checklist
- [x] print_hello
- [x] for_loop
- [x] let_and_print
- [x] unary_neg
- [x] len_builtin
- [x] len_string
- [x] map_index
- [x] if_else
- [x] while_loop
- [x] string_concat
- [x] string_compare
- [x] string_index
- [x] for_list_collection
- [x] list_index
