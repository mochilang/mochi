# a2mochi Pascal Converter

This directory contains the test helpers and golden files for converting
Pascal programs under `tests/transpiler/x/pas` into Mochi AST form.  The
implementation is mostly regex based and is inspired by the Python and
TypeScript converters.

Completed programs: 17/104

Supported features include:
- basic `program` blocks with variable declarations
- simple `for` and `while` loops
- `if` statements and `repeat/until`
- assignments and `writeln` calls mapped to `print`
