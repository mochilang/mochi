# Scala to Mochi Conversion

This directory stores golden files for converting Scala source code into Mochi.

## Supported Features

- Function declarations using the Metals language server
- `println` mapped to `print`
- Variable declarations with `var` and `val`
- `return` statements
- Basic assignment expressions
- Simple `while` and `for` loops
- `if`/`else` blocks
- Indexing and slicing via helper functions like `_indexList` and `_sliceString`
- Pattern matching using `match`
- Simple case class and sealed trait definitions

## Unsupported Features

- Advanced for comprehensions and generators
- Class and trait definitions
- Implicits, generics and type inference
- Complex library calls
