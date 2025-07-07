# COBOL Converter

This package implements the experimental COBOL frontend used by the `any2mochi` tool. It converts a very small subset of COBOL into Mochi.

The parser is written in pure Go and extracts statements from the `PROCEDURE DIVISION`. `Convert` walks the minimal AST produced by `Parse` and emits Mochi code.

## Supported Features
- `DISPLAY` statements are translated to `print` calls
- `MOVE`, `COMPUTE` and `ADD` assignments
- `PERFORM UNTIL` loops mapped to `while`
- `PERFORM VARYING` loops mapped to `for`
- `IF`/`ELSE` conditionals

## Unsupported
- Declarations outside `PROCEDURE DIVISION`
- Complex data structures or file IO
- Most of COBOL syntax beyond the minimal subset used in tests
